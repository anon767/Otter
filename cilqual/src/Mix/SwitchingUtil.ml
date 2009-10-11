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

open CilQual.Environment.CilFieldOrVar

open TypedBlock.G.QualType.Qual
open TypedBlock.G.QualType
open TypedBlock.G
open TypedBlock.GOps
open TypedBlock.DiscreteSolver
open TypedBlock


(** Take off one Ref from qualified types, useful for matching the representation of symbolic values. *)
let drop_qt = function
    (* TODO: handle function pointers *)
    | Ref (_, qt) -> qt
    | _ -> failwith "TODO: report drop_qt of non-variable"


(** Convert a qualified type to symbolic value.
    @param solution     the qualifier constraints solution
    @param state        the symbolic execution state
    @param block_type   the symbolic memory block type to create pointers as
    @param typ          the corresponding C type of the qualified type
    @param qt           the qualified type to convert
    @return [(state, bytes)]{i monad} the updated symbolic execution state and the converted result in the CilQual monad
*)
let qt_to_bytes solution state block_type typ qt =
    let rec qt_to_bytes state block_type typ qt = match typ, qt with
        (* first handle qualified types *)

        | Cil.TPtr (typtarget, _), Ref (Var v, qtarget) -> perform
            (* for pointers, recursively deref and generate point-to blocks *)
            (state, target_bytes) <-- qt_to_bytes state Types.Block_type_Global typtarget qtarget;

            (* TODO: generate blocks according to aliasing *)
            let target_block = MemOp.block__make
                (Var.printer Format.str_formatter v; Format.flush_str_formatter ())
                (MemOp.bytes__length target_bytes)
                block_type
            in
            let state = MemOp.state__add_block state target_block target_bytes in
            let nonnull_bytes = Types.make_Bytes_Address (Some target_block, MemOp.bytes__zero) in

            if Solution.equal_const v "null" solution then
                (* if $null, there exists some null assignment to qt *)
                let indicator = MemOp.indicator__next () in
                let null_bytes = Types.make_Bytes_Address (None, MemOp.bytes__zero) in
                let bytes = Types.make_Bytes_MayBytes (indicator, nonnull_bytes, null_bytes) in
                return (state, bytes)
            else
                (* if $nonnull or unannotated, then there does not exist some null assignment to qt *)
                return (state, nonnull_bytes)

        | Cil.TComp (compinfo, _), Base (Var v) when compinfo.Cil.cstruct ->
            (* for structs, initialize and iterate over the fields *)
            let size = (Cil.bitsSizeOf (typ)) / 8 in
            let size = if size <= 0 then 1 else size in
            let bytes = MemOp.bytes__symbolic size in
            foldM begin fun (state, bytes) f -> perform
                qtf <-- get_field qt f;
                (state, field_bytes) <-- qt_to_bytes state block_type f.Cil.ftype (drop_qt qtf);
                let offset = Convert.lazy_int_to_bytes (Eval.field_offset f) in
                let size = MemOp.bytes__length field_bytes in
                let bytes = MemOp.bytes__write bytes offset size field_bytes in
                return (state, bytes)
            end (state, bytes) compinfo.Cil.cfields

        | Cil.TComp (compinfo, _), Base (Var v) when not compinfo.Cil.cstruct ->
            failwith "TODO: qt_to_bytes: handle unions"

        | Cil.TPtr (typtarget, _), Base (Var v) when Cil.isVoidPtrType typ ->
            failwith "TODO: qt_to_bytes: handle void *"

        | typ, Base (Var v) when Cil.isArithmeticType typ ->
            (* arithmetic values are just that *)
            let size = (Cil.bitsSizeOf (typ)) / 8 in
            let size = if size <= 0 then 1 else size in
            let bytes = MemOp.bytes__symbolic size in
            return (state, bytes)

        | _, _ ->
            failwith "TODO: qt_to_bytes: handle other mismatched types?"
    in
    qt_to_bytes state block_type typ qt


(** Re-initialize a symbolic memory frame by coverting all corresponding qualified types to new symbolic values in the
    the symbolic state.
    @param solution     the qualifier constraints solution
    @param state        the symbolic execution state
    @param frame        the symbolic memory frame to re-initialize
    @param block_type   the symbolic memory block type to create pointers as
    @return [state]{i monad} the updated symbolic execution state in the CilQual monad
*)
let frame_qt_to_bytes solution state frame block_type =
    Types.VarinfoMap.fold begin fun v block stateM -> perform
        state <-- stateM;
        qt <-- lookup_var v;
        (state, bytes) <-- qt_to_bytes solution state block_type v.Cil.vtype (drop_qt qt);
        let state = MemOp.state__add_block state block bytes in
        return state
    end frame.Types.varinfo_to_block (return state)


(** Convert a symbolic value to constaints on a qualified type.
    @param state        the symbolic execution state
    @param typ          the corresponding C type of the qualified type
    @param bytes        the symbolic value to convert
    @param qt           the qualified type to add constraints to
    @return [(state)]{i monad} the symbolic execution state in the CilQual monad updated with additional constraints.
*)
let bytes_to_qt state typ bytes qt =
    let rec bytes_to_qt state typ bytes qt = match typ, qt with
        | Cil.TPtr (typtarget, _), Ref (Var v, qtarget) ->
            (* $null is a source, $nonnull a sink, so only add $null annotations *)
            begin try perform
                let target_lvals = Eval.deref state bytes in
                (* didn't fail, so is not a null pointer *)
                begin match typtarget with
                    | Cil.TPtr _ ->
                        let rec recurse = function
                            | Types.Lval_Block (block, offset) ->
                                let target_bytes = MemOp.state__get_bytes_from_lval state
                                    (block, offset, Types.word__size)
                                in
                                (* not tail-recursive! *)
                                bytes_to_qt state typtarget target_bytes qtarget
                            | Types.Lval_May (indicator, lval1, lval2) -> perform
                                recurse lval1;
                                recurse lval2
                            | Types.Lval_IfThenElse (condition, lval1, lval2) -> perform
                                recurse lval1;
                                recurse lval2
                        in
                        recurse target_lvals
                    | _ ->
                        return state
                end
            with
                | Failure "Dereference a dangling pointer" -> perform
                    (* a null pointer *)
                    annot qt "null";
                    return state
                | Failure _ ->
                    (* uninitialized and unused pointer *)
                    if MemOp.same_bytes bytes MemOp.bytes__zero then perform
                        (* is definitely a null pointer *)
                        annot qt "null";
                        return state
                    else
                        (* TODO: otherwise, nonnull? *)
                        return state
            end

        | Cil.TComp (compinfo, _), Base (Var v) when compinfo.Cil.cstruct -> perform
            (* for structs, iterate over the fields *)
            List.fold_right begin fun f stateM -> perform
                state <-- stateM;
                qtf <-- get_field qt f;
                let offset = Convert.lazy_int_to_bytes (Eval.field_offset f) in
                let size = (Cil.bitsSizeOf (f.Cil.ftype)) / 8 in
                let field_bytes = MemOp.bytes__read bytes offset size in
                bytes_to_qt state f.Cil.ftype field_bytes (drop_qt qtf);
                stateM
            end compinfo.Cil.cfields (return state)

        | Cil.TComp (compinfo, _), Base (Var v) when not compinfo.Cil.cstruct ->
            failwith "TODO: bytes_to_qt: handle unions"

        | Cil.TPtr (typtarget, _), Base (Var v) when Cil.isVoidPtrType typ ->
            failwith "TODO: bytes_to_qt: handle void *"

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
let frame_bytes_to_qt state frame =
    Types.VarinfoMap.fold begin fun v block stateM -> perform
        state <-- stateM;
        x <-- lookup_var v;
        let bytes = MemOp.state__get_bytes_from_block state block in
        bytes_to_qt state v.Cil.vtype bytes (drop_qt x);
        return state
    end frame.Types.varinfo_to_block (return state)

