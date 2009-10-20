(** Utility functions for switching the representation of values between CilQual and Otter.

    {0:lattice The $null source/$nonnull sink discrete partial order}
    This module currently understands $null/$nonnull CilQual annotations for detecting null-pointer errors. $null
    annotates sources of NULL values, and marks pointers that may have a NULL assignment. $nonnull annotates sinks that
    cannot be NULL, e.g., the parameters of library functions. An null-pointer error is detected when a $null source
    flows to $nonnull sink.

    {0 Converting from CilQual qualified types to Otter symbolic values}
    $null pointers from CilQual indicates that a pointer may be null because there exists a NULL assignment, and
    is therefore converted to pointers that may or may not be null in Otter. Conversely, $nonnull and unannotated
    pointers indicates that a pointer is never null because there are no NULL assignments, and is therefore converted to
    valid pointers to some memory in Otter.

    {0 Converting from Otter symbolic values to CilQual qualifed types}
    Null pointers from Otter are converted into additional $null constraints on the corresponding pointers in CilQual.
    Conversely, non-null pointers from Otter are not converted into any additional constraints, since according to
    {!section:lattice}, they are neither sources of null values nor non-null sinks (the latter only makes sense in
    blocks analyzed by CilQual).
*)

open CilQual.CilData
open CilQual.Environment.CilFieldOrVar

open TypedBlock.G.QualType.Qual
open TypedBlock.G.QualType
open TypedBlock.G
open TypedBlock.GOps
open TypedBlock.DiscreteSolver
open TypedBlock

module Aliasing = TypeQual.QualSolver.Aliasing (G.QualGraph)


(** Prepare a Cil file for block switching
    @param file the Cil file to prepare
*)
let prepare_file file =
    (* set up to use Cil's pointer analysis *)
    Ptranal.analyze_file file;
    Ptranal.compute_results false


let aliasing = Hashtbl.create 0

let alias pointer file =
    (* up to use CilQual's alias analysis *)
    let constraints = try Hashtbl.find aliasing file with Not_found -> begin
        let expM = interpret_file file in
        let ((((((), constraints), _), _), _), _) =
            run expM ((((((), QualGraph.empty), emptyContext), 0), emptyUnionTable), emptyEnv) in
        Hashtbl.add aliasing file constraints;
        constraints
    end in
    Aliasing.alias pointer constraints


(** Take off one Ref from qualified types, useful for matching the representation of symbolic values. *)
let drop_qt = function
    (* TODO: handle function pointers *)
    | Ref (_, qt) -> qt
    | _ -> failwith "TODO: report drop_qt of non-variable"


(** Convert a qualified type to symbolic value.
    @param expState     the CilQual monad state
    @param solution     the qualifier constraints solution
    @param state        the symbolic execution state
    @param typ          the corresponding C type of the qualified type
    @param qt           the qualified type to convert
    @return [(state, bytes)] the updated symbolic execution state and the converted result
*)
let qt_to_bytes file expState solution state exp qt =
    let rec qt_to_bytes expState state exp qt = match Cil.typeOf exp, qt with
        | Cil.TPtr (_, _), Ref (Var v, qtarget) ->
            let malloc_deferred state =
                qt_to_bytes expState state (Cil.Lval (Cil.Mem exp, Cil.NoOffset)) qtarget
            in
            make_pointer expState state exp v malloc_deferred

        | Cil.TComp (compinfo, _) as typ, Base (Var v) when compinfo.Cil.cstruct ->
            (* for structs, initialize and iterate over the fields *)
            let size = (Cil.bitsSizeOf typ) / 8 in
            let size = if size <= 0 then 1 else size in
            let bytes = MemOp.bytes__symbolic size in
            begin match exp with
                | Cil.Lval lval ->
                    List.fold_left begin fun (state, bytes) field ->
                        let (((((qtfield, _), _), _), _), _) = run (get_field qt field) expState in
                        let field_lval = Cil.addOffsetLval (Cil.Field (field, Cil.NoOffset)) lval in
                        let state, field_bytes = qt_to_bytes expState state (Cil.Lval field_lval) (drop_qt qtfield) in
                        let offset = Convert.lazy_int_to_bytes (Eval.field_offset field) in
                        let size = MemOp.bytes__length field_bytes in
                        let bytes = MemOp.bytes__write bytes offset size field_bytes in
                        (state, bytes)
                    end (state, bytes) compinfo.Cil.cfields
                | _ ->
                    failwith "are there any other Cil.exp that can have type Cil.TComp?"
            end

        | Cil.TComp (compinfo, _), Base (Var v) when not compinfo.Cil.cstruct ->
            failwith "TODO: qt_to_bytes: handle unions"

        | Cil.TPtr (typtarget, _) as typ, Base (Var v) when Cil.isVoidPtrType typ ->
            (* for void * pointers, generate a pointer that cannot be dereferenced *)
            let malloc_deferred state = failwith "TODO: qt_to_bytes: handle malloc'ed void *" in
            make_pointer expState state exp v malloc_deferred

        | typ, Base (Var v) when Cil.isArithmeticType typ ->
            (* arithmetic values are just that *)
            let size = (Cil.bitsSizeOf typ) / 8 in
            let size = if size <= 0 then 1 else size in
            let bytes = MemOp.bytes__symbolic size in
            (state, bytes)

        | _, _ ->
            failwith "TODO: qt_to_bytes: handle other mismatched types?"

    and make_pointer expState state exp pointer malloc_deferred =
        (* TODO: factor out make_pointer into Otter, since it's common to "start in the middle" executions *)

        (* for pointers, generate bytes according to aliasing:
         * - point to all global variables they may point to;
         * - for each local variable they may point to, find all occurrences of the local variable in the stack and
         *   point to them;
         * - for each local variable they may point to, create an extra instance of each in the extra allocations,
         *   and point to all extra instances of those variables (including those created for other pointers); this
         *   is to account for possible extra recursion that is not in the partial call stack;
         * - TODO: for each dynamic allocation site, make a new block and point to it.
         *)

        (* TODO: add support for pointers into arrays *)
        (* TODO: add support for pointers to fields, or somehow detect and bail *)

        (* determine all targets this pointer may point to, and generate a MayBytes tree for it *)
        let malloc_list, target_varinfo_list =
            (* Don't really know what sort of expressions Ptranal.resolve_exp will work with; it doesn't even seem to
             * handle Cil.Lval consistently *)
            let points_to = begin match exp with
                | Cil.Lval lval ->
                    Ptranal.resolve_lval lval
                | _ ->
                    Ptranal.resolve_exp exp
            end in
            List.partition (fun p -> List.mem p.Cil.vname Ptranal.alloc_names) points_to
        in

        (* then, find all occurrences of the targets in the globals and callstack *)
        let target_block_list = List.fold_left begin fun target_block_list frame ->
            List.fold_left begin fun pointer_list x ->
                try
                    (Types.VarinfoMap.find x frame.Types.varinfo_to_block)::target_block_list
                with Not_found ->
                    target_block_list
            end target_block_list target_varinfo_list
        end [] (state.Types.global::(state.Types.formals @ state.Types.locals)) in

        (* also add all targets from extra allocations *)
        let state, target_block_list = List.fold_left begin fun (state, target_block_list) x ->
            let extra_block_list = try Types.VarinfoMap.find x state.Types.extra_locals with Not_found -> [] in
            let state, extra_block_list = if x.Cil.vglob then
                (state, extra_block_list)
            else
                (* if it's a local variable, conservatively add an additional target to account for possible
                 * extra recursion that is not in the partial call stack *)
                let extra_exp = Cil.Lval (Cil.Var x, Cil.NoOffset) in
                let extra_block = MemOp.block__make
                    (CilVar.printer Format.str_formatter x; Format.flush_str_formatter ())
                    ((Cil.bitsSizeOf x.Cil.vtype) / 8)
                    Types.Block_type_Local
                in
                let extra_deferred state =
                    let (((((qt, _), _), _), _), _) = run (lookup_var x) expState in
                    qt_to_bytes expState state extra_exp qt
                in
                let extra_block_list = extra_block::extra_block_list in
                let state = MemOp.state__add_deferred_block state extra_block extra_deferred in
                let state = { state with
                    Types.extra_locals = Types.VarinfoMap.add x extra_block_list state.Types.extra_locals
                } in
                (state, extra_block_list)
            in
            (state, extra_block_list @ target_block_list)
        end (state, target_block_list) target_varinfo_list in

        (* TODO: mallocs may alias too, should handle it just like local variables; annoyingly enough, although Ptranal
         *       returns n malloc sites, there doesn't seem to be a way to distinguish them in the output *)
        let state, target_block_list = if malloc_list <> [] then
            failwith "TODO: support malloc"
            (*
            let malloc_block = MemOp.block__make
                (Format.fprintf Format.str_formatter "malloc:%a" Var.printer pointer; Format.flush_str_formatter ())
                ((Cil.bitsSizeOf (Cil.typeOfLval (Cil.Mem exp, Cil.NoOffset))) / 8)
                Types.Block_type_Heap
            in
            let state = MemOp.state__add_deferred_block state malloc_block malloc_deferred in
            (state, malloc_block::target_block_list)
            *)
        else
            (state, target_block_list)
        in

        (* generate bytes to the target blocks *)
        let target_bytes_list = List.map begin fun block ->
            Types.make_Bytes_Address (Some block, MemOp.bytes__zero)
        end target_block_list in

        (* check to see if there exists a null assignment through a qualified type that may point to this pointer *)
        (* TODO: replace the overly simplistic alias analysis below *)
        let aliases = alias pointer file in
        let has_null = Aliasing.VarSet.exists (fun a -> Solution.equal_const a "null" solution) aliases in
        let target_bytes_list = if has_null then
            (* if $null, there exists some null assignment to qt *)
            let null_bytes = Types.make_Bytes_Address (None, MemOp.bytes__zero) in
            null_bytes::target_bytes_list
        else
            (* if $nonnull or unannotated, then there does not exist some null assignment to qt *)
            target_bytes_list
        in

        (* finally, return a MayBytes pointing to the targets *)
        let target_bytes = MemOp.bytes__maybytes_from_list target_bytes_list in
        (state, target_bytes)
    in
    qt_to_bytes expState state exp qt


(** Re-initialize a symbolic memory frame by coverting all corresponding qualified types to new symbolic values in the
    the symbolic state.
    @param expState     the CilQual monad state
    @param solution     the qualifier constraints solution
    @param state        the symbolic execution state
    @param frame        the symbolic memory frame to re-initialize
    @return [state]     the updated symbolic execution state
*)
let frame_qt_to_bytes file expState solution state frame =
    Types.VarinfoMap.fold begin fun v block state ->
        let (((((qt, _), _), _), _), _) = run (lookup_var v) expState in
        let deferred state =
            qt_to_bytes file expState solution state (Cil.Lval (Cil.Var v, Cil.NoOffset)) (drop_qt qt)
        in
        MemOp.state__add_deferred_block state block deferred
    end frame.Types.varinfo_to_block state


(** Convert a symbolic value to constaints on a qualified type.
    @param state        the symbolic execution state
    @param typ          the corresponding C type of the qualified type
    @param bytes        the symbolic value to convert
    @param qt           the qualified type to add constraints to
    @return [(state)]{i monad} the symbolic execution state in the CilQual monad updated with additional constraints.
*)
let bytes_to_qt old_state state typ bytes qt =

    (* check a given pointer bytes, adding $null annotations if it is a null pointer, and call check_target with
     * the target bytes if it is a valid pointer *)
    let check_pointer state bytes qt check_target = perform
        (* first, try to deref the bytes; if it succeeds, then it's a valid pointer, otherwise it is invalid *)
        target_lvals_opt <-- begin try
            return (Some (Eval.deref state bytes))
        with
            | Failure "Dereference a dangling pointer" -> perform
                (* a null pointer *)
                annot qt "null";
                return None
            | Failure _ ->
                (* uninitialized and unused pointer *)
                if MemOp.same_bytes bytes MemOp.bytes__zero then perform
                    (* is definitely a null pointer *)
                    annot qt "null";
                    return None
                else
                    (* TODO: otherwise, nonnull? *)
                    return None
        end;
        begin match target_lvals_opt with
            | Some target_lvals ->
                (* valid pointer, so check what it points to *)
                let rec recurse = function
                    | Types.Lval_Block (block, offset) ->
                        let old_deferred_opt =
                            try Some (MemOp.state__get_deferred_from_block old_state block)
                            with Not_found -> None
                        in
                        let deferred = MemOp.state__get_deferred_from_block state block in
                        begin match old_deferred_opt, deferred with
                            | Some old_deferred, deferred when old_deferred == deferred ->
                                return state
                            | _, _ ->
                                let state, target_bytes =
                                    MemOp.state__get_bytes_from_lval state (block, offset, Types.word__size)
                                in
                                (* not tail-recursive! *)
                                check_target state target_bytes
                        end
                    | Types.Lval_May (indicator, lval1, lval2) -> perform
                        recurse lval1;
                        recurse lval2
                    | Types.Lval_IfThenElse (condition, lval1, lval2) -> perform
                        recurse lval1;
                        recurse lval2
                in
                recurse target_lvals

            | _ ->
                (* null pointer *)
                return state
        end
    in

    let rec bytes_to_qt state typ bytes qt =
        match typ, qt with
        | Cil.TPtr (typtarget, _), Ref (Var v, qtarget) ->
            let check_target state target_bytes = bytes_to_qt state typtarget target_bytes qtarget in
            check_pointer state bytes qt check_target

        | Cil.TComp (compinfo, _), Base (Var v) when compinfo.Cil.cstruct -> perform
            (* for structs, iterate over the fields *)
            List.fold_right begin fun f stateM -> perform
                state <-- stateM;
                qtf <-- get_field qt f;
                let offset = Convert.lazy_int_to_bytes (Eval.field_offset f) in
                let size = (Cil.bitsSizeOf f.Cil.ftype) / 8 in
                let field_bytes = MemOp.bytes__read bytes offset size in
                bytes_to_qt state f.Cil.ftype field_bytes (drop_qt qtf);
                stateM
            end compinfo.Cil.cfields (return state)

        | Cil.TComp (compinfo, _), Base (Var v) when not compinfo.Cil.cstruct ->
            failwith "TODO: bytes_to_qt: handle unions"

        | Cil.TPtr (typtarget, _), Base (Var v) when Cil.isVoidPtrType typ ->
            let check_target state target_bytes = failwith "TODO: bytes_to_qt: handle void *" in
            check_pointer state bytes qt check_target

        | typ, Base (Var v) when Cil.isArithmeticType typ -> perform
            (* value types, nothing to dereference *)
            return state

        | _, _ ->
            failwith "TODO: bytes_to_qt: handle other mismatched types?"
    in
    bytes_to_qt state typ bytes qt


(** Convert a symbolic memory frame to qualifier constraints on all qualified types corresponding to variables in the
    memory frame.
    @param state        the symbolic execution state
    @param frame        the symbolic memory frame to re-initialize
    @return [(state)]{i monad} the symbolic execution state in the CilQual monad updated with additional constraints.
*)
let frame_bytes_to_qt old_state state frame =
    Types.VarinfoMap.fold begin fun v block stateM -> perform
        state <-- stateM;
        x <-- lookup_var v;
        let old_deferred_opt =
            try Some (MemOp.state__get_deferred_from_block old_state block)
            with Not_found -> None
        in
        let deferred = MemOp.state__get_deferred_from_block state block in
        match old_deferred_opt, deferred with
            | Some old_deferred, deferred when old_deferred = deferred ->
                return state
            | _, _ ->
                let state, bytes = MemOp.state__force state deferred in
                bytes_to_qt old_state state v.Cil.vtype bytes (drop_qt x)
    end frame.Types.varinfo_to_block (return state)

