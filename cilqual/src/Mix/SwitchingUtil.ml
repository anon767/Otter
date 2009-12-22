(** Utility functions for switching the representation of values between CilQual and Otter.

    {b The $null source/$nonnull sink discrete partial order}

    [SwitchingUtil] currently understands $null/$nonnull CilQual annotations for detecting null-pointer errors. $null
    annotates sources of NULL values, and marks pointers that may have a NULL assignment. $nonnull annotates sinks that
    cannot be NULL, e.g., the parameters of library functions. An null-pointer error is detected when a $null source
    flows to $nonnull sink.

        {i Converting from CilQual qualified types to Otter symbolic values}: [qt_to_bytes], [qt_to_lval_block],
        [qt_to_frame]

        $null pointers from CilQual indicates that a pointer may be null because there exists a NULL assignment, and
        is therefore converted to pointers that may or may not be null in Otter. Conversely, $nonnull and un-annotated
        pointers indicates that a pointer is never null because there are no NULL assignments, and is therefore
        converted to valid pointers to some memory in Otter.

        {i Converting from Otter symbolic values to CilQual qualified types}: [bytes_to_qt], [bytes_to_frame]

        Null pointers from Otter are converted into additional $null constraints on the corresponding pointers in
        CilQual. Conversely, non-null pointers from Otter are not converted into any additional constraints, since
        according to the partial order, they are neither sources of null values nor non-null sinks (the latter only
        makes sense in blocks analyzed by CilQual).


    {b Initializing memory for Otter ("starting-in-the-middle symbolic execution")}

    [SwitchingUtil] lazily initializes the memory for symbolic execution in Otter. When a variable is first used, or
    a pointer dereferenced, new memory blocks for those locations are mapped into memory and then returned. To
    handle possible aliasing via pointers, the following scheme is used:

    {ul
        {- Each variable is associated with an alias set ([Types.state.Types.extra]), which is initially empty.
        }
        {- When a variable is accessed for the first time ([qt_to_lval_block]):
           {ol
               {- create a new memory block of the variable's type and add to it's alias set (this captures the case
                  where the variable is not aliased);}
               {- return a conditional [Bytes.lval_block] that may point to it's alias set.}
           }
        }
        {- When a pointer is dereferenced for the first time ([make_pointer] in [qt_to_bytes]):
           {ol
               {- determine the pointer's points-to set;}
               {- create a new memory block for each type in the pointer's points-to set;}
               {- for each target in the points-to set, add the newly created memory block to the alias set of those
                  target ([Types.state.Types.extra])}
               {- return a conditional [Bytes.lval_block] that may point to the newly created memory blocks as well
                  as to its points-to set.}
           }
        }
    }

    For example:
    {ul
        {- Assume the following variables:
              {ul {- [int r, s; struct t \{ int u; int v; \} t; int *x, *y;]}}
           where the points-to set for [x] is \{ [r], [s], [t.v] \} and for [y] is \{ [r], [s] \}.
        }
        {- When [r] is accessed:
               {ol {- create a new memory block [r:int] and add to {i aliases}([r]) = \{ [r:int] \};}
                   {- return [s] = \{ [*x:int], [s:int] \}.}
               }
        }
        {- When [x] is dereferenced:
               {ol {- determine the points-to set for [x], which is \{ [r], [s], [t.v] \};}
                   {- create new memory blocks [*x:int] and [*x:struct t] for each type [x] may point to;}
                   {- add to the {i aliases}([r]) = \{ [r:int], [*x:int] \}, to {i aliases}([s]) = \{ [*x:int]
                      \}, and to {i aliases}([t]) = \{ [*x:struct t] \};}
                   {- return [*x] = \{ [r:int], [*x:int], [( *x:struct t ).v] \}.}
               }
        }
        {- When [s] is accessed:
               {ol {- create [s:int] and add to {i aliases}([s]) = \{ [*x:int], [s:int] \};}
                   {- return [s] = \{ [*x:int], [s:int] \}.}
               }
        }
        {- When [y] is dereferenced:
               {ol {- determine the points-to set for [y], which is \{ [r], [s] \};}
                   {- create [*y:int];}
                   {- add to {i aliases}([r]) = \{ [r:int], [*x:int], [*y:int] \} and to {i aliases}([s]) = \{ [*x:int],
                      [s:int], [*y:int] \};}
                   {- return [*y] = \{ [r:int], [*x:int], [*y:int], [s:int] \}.}
               }
        }
    }

    The above scheme conservatively generates all possible aliasing relationships that is consistent with the provided
    points-to set for each variable. The advantage of this scheme is that the number of aliasing relationships
    generated grows with the number of pointers dereferenced, {i not} with the size of points-to set of the pointers
    dereferenced. If few pointers are dereferenced, the former may be much smaller than the latter.

    The trade-off of this scheme is that it may generate aliasing relationships that are impossible in C, e.g., in the
    example above, the variables [r] and [s] may alias. This also inflates the number of aliasing relationships if
    many pointers are dereferenced.
*)

open CilQual.CilData

open TypedBlock.G.QualType.Qual
open TypedBlock.G.QualType
open TypedBlock.G
open TypedBlock.GOps
open TypedBlock.DiscreteSolver
open TypedBlock


(** Prepare a Cil file for block switching
    @param file the Cil file to prepare
*)
let prepare_file file =
    (* set up to use Cil's pointer analysis *)
    Ptranal.analyze_file file;
    Ptranal.compute_results false


(** Take off one Ref from qualified types, useful for matching the representation of symbolic values. *)
let drop_qt = function
    (* TODO: handle function pointers *)
    | Ref (_, qt) -> qt
    | _ -> failwith "TODO: report drop_qt of non-variable"


(** map from Cil.typ *)
module CilTypeMap = Map.Make (CilCanonicalType)


(** fold helper for Cil.TComp *)
let fold_struct f acc compinfo =
    List.fold_left f acc compinfo.Cil.cfields


(** fold helper for Cil.TArray *)
let fold_array f acc len_opt =
    begin match len_opt with
        | Some len ->
            begin match Cil.isInteger len with
                | Some n ->
                    let rec fold_array acc i index =
                        if Int64.compare i n == 0 then
                            Some acc
                        else
                            let acc = f acc index in
                            fold_array acc (Int64.succ i) (Cil.increm index 1)
                    in
                    fold_array acc Int64.zero Cil.zero
                | None ->
                    (* non-constant length *)
                    None
            end
        | None ->
            (* no length *)
            None
    end


(** Wrapper for Ptranal that resolves to fields conservatively and filters by type, and groups by struct type.
    @param file     the file begin analyzed
    @param exp      the expression to resolve
    @return         [(targets_map, mallocs)] where [target_maps] contains the points to targets, grouped by type, and
                    split into variables and offsets such that the targets are the cross product :
                    [(Cil.varinfo list * Cil.offsets list) CilTypeMap]; and [mallocs] contains a list of malloc targets
*)
let points_to file exp =
    (* filter pointer types *)
    let base_type = Cil.unrollType (Cil.typeOf exp) in
    match base_type with
        | Cil.TPtr (target_type, _) when not (Cil.isFunctionType target_type) ->
            (* Don't really know what sort of expressions Ptranal.resolve_exp will work with; it doesn't even seem to
               handle Cil.Lval consistently. *)
            let targets = match exp with
                | Cil.Lval lval ->
                    begin try
                        Ptranal.resolve_lval lval
                    with
                        | Not_found ->
                            (* seems to occur for unused variable *)
                            []
                    end
                | exp ->
                    Ptranal.resolve_exp exp
            in

            let malloc_list, targets =
                List.partition (fun p -> List.mem p.Cil.vname Ptranal.alloc_names) targets
            in

            let accept_type typ =
                (* filter types that are compatible with the target type *)
                if CilCanonicalType.equal target_type typ then
                    true (* equal types are compatible *)
                else
                    (* TODO: what else? enums with ints? structs with structs? *)
                    let rec accept_type x y = match x, y with
                        | x, _ when Cil.isVoidType x -> (* void * points to anything *)
                            true
                        | x, Cil.TArray (y, _, _) when CilCanonicalType.equal x y -> (* pointers may point to arrays *)
                            true
                        | Cil.TPtr (x, _), Cil.TPtr (y, _) ->
                            accept_type x y
                        | _, _ ->
                            false
                    in
                    accept_type target_type (Cil.unrollType typ)
            in

            let to_offsets typ =
                let rec to_offsets offsets typ base =
                    let offset_type = Cil.typeOffset typ base in
                    let offsets = if accept_type offset_type then
                        base::offsets
                    else
                        offsets
                    in
                    match offset_type with
                        | Cil.TComp (compinfo, _) ->
                            fold_struct begin fun offsets field ->
                                to_offsets offsets typ (Cil.addOffset (Cil.Field (field, Cil.NoOffset)) base)
                            end offsets compinfo
                        | Cil.TArray (_, len_opt, _) ->
                            let offsets_opt = fold_array begin fun offsets index ->
                                to_offsets offsets typ (Cil.addOffset (Cil.Index (index, Cil.NoOffset)) base)
                            end offsets len_opt in
                            begin match offsets_opt with
                                | Some offsets -> offsets
                                | None         -> offsets
                            end
                        | _ ->
                            offsets
                in
                to_offsets [] typ Cil.NoOffset
            in

            (* make sure that the map contains at least the target type *)
            let targets_map = if malloc_list != [] && not (Cil.isVoidType target_type) then
                CilTypeMap.add target_type ([], to_offsets target_type) CilTypeMap.empty
            else
                CilTypeMap.empty
            in
            let targets_map = List.fold_left begin fun targets_map v ->
                let target_vars, offsets =
                    try CilTypeMap.find v.Cil.vtype targets_map
                    with Not_found -> ([], to_offsets v.Cil.vtype)
                in
                CilTypeMap.add v.Cil.vtype ((v::target_vars), offsets) targets_map
            end targets_map targets in

            (targets_map, malloc_list)

        | _ ->
            (CilTypeMap.empty, [])


(** Convert a qualified type to symbolic value.
    @param file         the file begin analyzed
    @param expState     the CilQual monad state
    @param solution     the qualifier constraints solution
    @param state        the symbolic execution state
    @param exp          the C expression from which qt was derived
    @param qt           the qualified type to convert
    @return             [(state, bytes)] the updated symbolic execution state and the converted result
*)
let qt_to_bytes file expState solution state exp qt =
    let rec make_pointer expState state exp pointer extra_deferred =
        (* TODO: factor out make_pointer into Otter, since it's common to "start in the middle" executions *)
        (* determine all targets this pointer may point to, and generate a MayBytes tree for it *)
        let targets_map, malloc_list = points_to file exp in

        let state, target_bytes_list = CilTypeMap.fold
            begin fun typ (target_vars, offsets) (state, target_bytes_list) ->
                (* for every type *)
                (* given a block (of type typ), generate a list of bytes pointing to all valid offsets *)
                let map_offsets target_bytes_list block =
                    List.fold_left begin fun target_bytes_list offset ->
                        (* for every offset, generate bytes pointing to each offset *)
                        let offset_bits, _ = Cil.bitsOffset typ offset in
                        let offset_bytes = Bytes.lazy_int_to_bytes (offset_bits / 8) in
                        let target_bytes = Bytes.make_Bytes_Address (block, offset_bytes) in
                        target_bytes::target_bytes_list
                    end target_bytes_list offsets
                in

                (* make an extra block *)
                let state, extra, target_bytes_list =
                    let size = (Cil.bitsSizeOf typ) / 8 in
                    let extra = Bytes.block__make
                        (Format.fprintf Format.str_formatter "%a" Var.printer pointer; Format.flush_str_formatter ())
                        size
                        Bytes.Block_type_Aliased
                    in
                    let deferred state =
                        let bytes = Bytes.bytes__symbolic size in
                        List.fold_left begin fun (state, bytes) extra_offset ->
                            let offset, size = Cil.bitsOffset typ extra_offset in
                            let offset = Bytes.lazy_int_to_bytes (offset / 8) in
                            let size = size / 8 in
                            let (state, offset_bytes) = extra_deferred state in
                            let bytes = Bytes.bytes__write bytes offset size offset_bytes in
                            (state, bytes)
                        end (state, bytes) offsets
                    in
                    let state = MemOp.state__add_deferred_block state extra deferred in
                    (state, extra, map_offsets target_bytes_list extra)
                in

                (* point to mallocs *)
                let state, target_bytes_list = List.fold_left begin fun (state, target_bytes_list) malloc ->
                    let mallocs_map = try Types.VarinfoMap.find malloc state.Types.malloc
                                      with Not_found -> Types.TypeMap.empty
                    in
                    let mallocs = try Types.TypeMap.find typ mallocs_map
                                  with Not_found -> []
                    in
                    let state =
                        let mallocs_map = Types.TypeMap.add typ (extra::mallocs) mallocs_map in
                        { state with Types.malloc=Types.VarinfoMap.add malloc mallocs_map state.Types.malloc }
                    in
                    (state, List.fold_left map_offsets target_bytes_list mallocs)
                end (state, target_bytes_list) malloc_list in

                (* and point to all targets *)
                let state, target_bytes_list = List.fold_left begin fun (state, target_bytes_list) v ->
                    (* for every target variable *)
                    let extras = try Types.VarinfoMap.find v state.Types.extra
                                 with Not_found -> []
                    in
                    let state = { state with Types.extra=Types.VarinfoMap.add v (extra::extras) state.Types.extra } in
                    (state, List.fold_left map_offsets target_bytes_list extras)
                end (state, target_bytes_list) target_vars in
                (state, target_bytes_list)
            end
        targets_map (state, []) in

        (* check to see if there exists a null assignment to this pointer *)
        let has_null = Solution.equal_const pointer "null" solution in

        (* also check exp for the case where the return value is assigned to a variable *)
        let (((((qtexp, _), _), _), _), _) = run (interpret_exp exp) expState in
        let has_null = has_null ||
            match qtexp with
                | Ref (Var v, _) -> Solution.equal_const v "null" solution
                | _ -> false
        in
        let target_bytes_list = if has_null then
            (* if $null, there exists some null assignment to qt *)
            let null_bytes = Bytes.bytes__zero in
            null_bytes::target_bytes_list
        else
            (* if $nonnull or unannotated, then there does not exist some null assignment to qt *)
            target_bytes_list
        in
        (* finally, return a MayBytes pointing to the targets *)
        let target_bytes = match target_bytes_list with
            | [] -> Bytes.conditional__bytes (Bytes.bytes__symbolic Bytes.word__size) (* uninitialized pointer *)
            | _  -> Bytes.conditional__from_list (List.map Bytes.conditional__bytes target_bytes_list)
        in
        (state, Bytes.make_Bytes_Conditional target_bytes)


    and qt_to_bytes expState state exp qt = match (Cil.unrollType (Cil.typeOf exp)), qt with
        | Cil.TPtr (_, _), Ref (Var v, qtarget) ->
            (* for pointers, generate the pointer *)
            let extra_deferred state =
                qt_to_bytes expState state (Cil.Lval (Cil.Mem exp, Cil.NoOffset)) qtarget
            in
            make_pointer expState state exp v extra_deferred

        | Cil.TComp (compinfo, _) as typ, Base (Var v) when compinfo.Cil.cstruct ->
            (* for structs, initialize and iterate over the fields *)
            let size = (Cil.bitsSizeOf typ) / 8 in
            let bytes = Bytes.bytes__symbolic size in
            begin match exp with
                | Cil.Lval lval ->
                    fold_struct begin fun (state, bytes) field ->
                        let (((((qtfield, _), _), _), _), _) = run (get_field qt field) expState in
                        let field_offset = Cil.Field (field, Cil.NoOffset) in
                        let field_lval = Cil.addOffsetLval field_offset lval in
                        let state, field_bytes = qt_to_bytes expState state (Cil.Lval field_lval) (drop_qt qtfield) in
                        let offset, size = Cil.bitsOffset typ field_offset in
                        let offset = Bytes.lazy_int_to_bytes (offset / 8) in
                        let size = size / 8 in
                        let bytes = Bytes.bytes__write bytes offset size field_bytes in
                        (state, bytes)
                    end (state, bytes) compinfo
                | _ ->
                    failwith "are there any other Cil.exp that can have type Cil.TComp?"
            end

        | Cil.TArray (_, len_opt, _) as typ, qt ->
            (* for arrays, initialize each element of the array *)
            let size = (Cil.bitsSizeOf typ) / 8 in
            let bytes = Bytes.bytes__symbolic size in
            begin match exp with
                | Cil.Lval lval ->
                    let result_opt = fold_array begin fun (state, bytes) index ->
                        let el_offset = Cil.Index (index, Cil.NoOffset) in
                        let el_lval = Cil.addOffsetLval el_offset lval in
                        let state, el_bytes = qt_to_bytes expState state (Cil.Lval el_lval) qt in
                        let offset, size = Cil.bitsOffset typ el_offset in
                        let offset = Bytes.lazy_int_to_bytes (offset / 8) in
                        let size = size / 8 in
                        let bytes = Bytes.bytes__write bytes offset size el_bytes in
                        (state, bytes)
                    end (state, bytes) len_opt in
                    begin match result_opt with
                        | Some result -> result
                        | None        -> (state, bytes)
                    end
                | _ ->
                    failwith "are there any other Cil.exp that can have type Cil.TArray?"
            end

        | Cil.TComp (compinfo, _) as typ, Base (Var v) when not compinfo.Cil.cstruct ->
            (* we can't handle unions *)
            failwith begin
                Format.fprintf Format.str_formatter
                    "TODO: qt_to_bytes: handle unions: %s: %s <=> %a"
                    (Pretty.sprint 0 (Cil.d_exp () exp))
                    (Pretty.sprint 0 (Cil.d_type () typ))
                    QualType.printer qt;
                Format.flush_str_formatter ()
            end

        | Cil.TPtr (typtarget, _) as typ, Base (Var v) when Cil.isVoidPtrType typ ->
            (* we can't handle void * either, except that to know that it is a pointer, so generate a pointer that
             * cannot be dereferenced *)
            let extra_deferred state = failwith begin
                Format.fprintf Format.str_formatter
                    "TODO: qt_to_bytes: handle void *: %s: %s <=> %a"
                    (Pretty.sprint 0 (Cil.d_exp () exp))
                    (Pretty.sprint 0 (Cil.d_type () typ))
                    QualType.printer qt;
                Format.flush_str_formatter ()
            end in
            make_pointer expState state exp v extra_deferred

        | Cil.TPtr (Cil.TFun _, _), Fn (Var fnptr, _, _) ->
            (* for function pointers, check to see if there exists a null assignment to this pointer *)
            let has_null = Solution.equal_const fnptr "null" solution in
            (* uninitialized pointer, since Otter doesn't yet handle conditional function pointers *)
            let bytes = Bytes.bytes__symbolic Bytes.word__size in
            let bytes = if has_null then
                let null_bytes = Bytes.bytes__zero in
                Bytes.make_Bytes_Conditional (Bytes.IfThenElse (
                    Bytes.guard__symbolic (), Bytes.conditional__bytes bytes, Bytes.conditional__bytes null_bytes))
            else
                bytes
            in
            (state, bytes)

        | typ, Base (Var v) when Cil.isArithmeticType typ ->
            (* arithmetic values are just that *)
            let size = (Cil.bitsSizeOf typ) / 8 in
            let bytes = Bytes.bytes__symbolic size in
            (state, bytes)

        | typ, qt ->
            failwith begin
                Format.fprintf Format.str_formatter
                    "TODO: qt_to_bytes: handle mismatched types: %s: %s <=> %a"
                    (Pretty.sprint 0 (Cil.d_exp () exp))
                    (Pretty.sprint 0 (Cil.d_plaintype () typ))
                    QualType.printer qt;
                Format.flush_str_formatter ()
            end
    in
    qt_to_bytes expState state exp qt


(** Convert a qualified type to a variable.
    @param file         the file begin analyzed
    @param expState     the CilQual monad state
    @param solution     the qualifier constraints solution
    @param state        the symbolic execution state
    @param v            the C variable from which qt was derived
    @param qt           the qualified type to convert
    @return             [(state, bytes)] the updated symbolic execution state and the converted result
*)
let qt_to_lval_block file expState solution state v qt =
    let deferred_lval_block state =
        (* make an extra block; for the case where the variable is not-aliased *)
        let extra = Bytes.block__make
            (Format.fprintf Format.str_formatter "%s" v.Cil.vname; Format.flush_str_formatter ())
            ((Cil.bitsSizeOf v.Cil.vtype) / 8)
            Bytes.Block_type_Aliased
        in
        let extra_deferred state =
            qt_to_bytes file expState solution state (Cil.Lval (Cil.var v)) (drop_qt qt)
        in
        let state = MemOp.state__add_deferred_block state extra extra_deferred in

        let extras = try Types.VarinfoMap.find v state.Types.extra
                     with Not_found -> []
        in
        let extras = extra::extras in
        let state = { state with
            Types.extra=Types.VarinfoMap.add v extras state.Types.extra
        } in

        (* generate an lval to each block it may alias that was previously initialized via another pointer *)
        let target_list = List.map begin fun block ->
            Bytes.conditional__lval_block (block, Bytes.bytes__zero)
        end extras in

        (* finally, return a Lval_May pointing to the targets *)
        (state, Bytes.conditional__from_list target_list)
    in
    (state, Types.Deferred deferred_lval_block)


(** Re-initialize a symbolic memory frame by coverting all corresponding qualified types to new symbolic values in the
    the symbolic state.
    @param file                 the file begin analyzed
    @param expState             the CilQual monad state
    @param solution             the qualifier constraints solution
    @param state                the symbolic execution state
    @param frame                the symbolic memory frame to re-initialize
    @return                     [(state, frame)] the updated symbolic memory frame
*)
let qt_to_frame file expState solution state frame =
    Types.VarinfoMap.fold begin fun v deferred_lval (state, frame) ->
        let (((((qt, _), _), _), _), _) = run (lookup_var v) expState in
        let state, lval_block = qt_to_lval_block file expState solution state v qt in
        (state, Types.VarinfoMap.add v lval_block frame)
    end frame (state, MemOp.frame__empty)


(** Attempt to dereference an lval, ignoring cases that are uninteresting to Mix:
    - abstract types, since they cannot be read or written by Otter;
    - void * and unions that were initialized by Mix, since they also cannot be read or written by Otter.
    @param ?pre         an optional precondition under which to dereference lval_block
    @param state        the symbolic execution state
    @param lval_block   the lval to dereference
    @param typ          the type to dereference as
    @return             [(state, bytes) option] the updated state and dereferenced value if successful
*)
let attempt_deref ?pre state lval_block typ =
    try
        Some (MemOp.state__deref ?pre state (lval_block, (Cil.bitsSizeOf typ) / 8))
    with
        | Cil.SizeOfError ("abstract type", _) ->
            (* ignore abstract types, they can't have values that come from the program (that is visible) *)
            None
        | Failure s when Str.string_match
                            (Str.regexp "^TODO: qt_to_bytes: handle \\(void \\*\\|union\\)")
                            s 0 ->
            (* don't care if we may alias void */unions, it can't be read/written by the symbolic executor
             * according to our set up in bytes_to_qt, and so can't be assigned any values anyway *)
            None


(** Convert a symbolic value to constraints on a qualified type.
    @param file         the file begin analyzed
    @param expState     the original CilQual state
    @param state        the symbolic execution state
    @param pre          an optional precondition under which the pointer holds
    @param bytes        the symbolic value to convert
    @param exp          the C expression from which bytes was derived
    @param qt           the qualified type to add constraints to
    @return             [state]{i monad} the symbolic execution state in the CilQual monad updated with additional
                        constraints.
*)
let bytes_to_qt file expState state pre bytes exp qt = perform

    (* check a given pointer bytes, adding $null annotations if it is a null pointer, and call check_target with
     * the target bytes if it is a valid pointer *)
    let check_pointer state pre bytes qt typtarget = perform
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
                if Bytes.bytes__equal bytes Bytes.bytes__zero then perform
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
                let state, target_bytes_list = Bytes.conditional__fold ~pre begin fun (state, target_bytes_list) pre lval_block ->
                    match attempt_deref ~pre state lval_block typtarget with
                        | Some (state, target_bytes) -> (state, (pre, target_bytes)::target_bytes_list)
                        | None                       -> (state, target_bytes_list)
                end (state, []) (Bytes.conditional__lval_block target_lvals) in
                return (state, target_bytes_list)
            | _ ->
                (* null pointer *)
                return (state, [])
        end
    in

    let rec bytes_to_qt state pre bytes exp qt = perform
        begin match Cil.unrollType (Cil.typeOf exp), qt with
            | Cil.TPtr (typtarget, _), Ref (Var v, qtarget) -> perform
                (state, target_bytes_list) <-- check_pointer state pre bytes qt typtarget;
                foldM begin fun state (pre, target_bytes) ->
                    bytes_to_qt state pre target_bytes (Cil.Lval (Cil.Mem exp, Cil.NoOffset)) qtarget
                end state target_bytes_list

            | Cil.TComp (compinfo, _) as typ, Base (Var v) -> perform
                (* for structs and unions, iterate over the fields *)
                begin match exp with
                    | Cil.Lval lval ->
                        fold_struct begin fun stateM field -> perform
                            let field_offset = Cil.Field (field, Cil.NoOffset) in
                            let field_lval = Cil.addOffsetLval field_offset lval in

                            let offset, size = Cil.bitsOffset typ field_offset in
                            let offset = Bytes.lazy_int_to_bytes (offset / 8) in
                            let size = size / 8 in
                            let field_bytes = Bytes.bytes__read bytes offset size in

                            state <-- stateM;

                            (* TODO: temporary hack, qt is off by one ref, but getfield doesn't look at qt *)
                            qtf <-- begin if compinfo.Cil.cstruct then perform
                                qtf <-- get_field qt field;
                                deref qtf
                            else
                                (* unions should really be the same as structs, but qt is off by one *)
                                return qt
                            end;

                            bytes_to_qt state pre field_bytes (Cil.Lval field_lval) qtf
                        end (return state) compinfo
                    | _ ->
                        failwith "are there any other Cil.exp that can have type Cil.TComp?"
                end

            | Cil.TArray (el_type, len_opt, _) as typ, qt ->
                (* for arrays, iterate over the elements *)
                begin match exp with
                    | Cil.Lval lval ->
                        let result_opt = fold_array begin fun stateM index -> perform
                            let el_offset = Cil.Index (index, Cil.NoOffset) in
                            let el_lval = Cil.addOffsetLval el_offset lval in

                            let offset, size = Cil.bitsOffset typ el_offset in
                            let offset = Bytes.lazy_int_to_bytes (offset / 8) in
                            let size = size / 8 in
                            let el_bytes = Bytes.bytes__read bytes offset size in

                            state <-- stateM;
                            bytes_to_qt state pre el_bytes (Cil.Lval el_lval) qt
                        end (return state) len_opt in
                        begin match result_opt with
                            | Some result -> result
                            | None        -> return state
                        end
                    | _ ->
                        failwith "are there any other Cil.exp that can have type Cil.TArray?"
                end

            | Cil.TPtr (typtarget, _) as typ, Base (Var v) when Cil.isVoidPtrType typ -> perform
                (* void *, ignore the targets, since they can't correspond to any annotations *)
                (state, target_bytes_list) <-- check_pointer state pre bytes qt typtarget;
                return state

            | Cil.TPtr (typ, _), Fn _ when Cil.isFunctionType typ ->
                (* function pointers can't be dereferenced *)
                return state

            | typ, Base (Var v) when Cil.isArithmeticType typ ->
                (* value types, nothing to dereference too *)
                return state

            | typ, qt ->
                failwith begin
                    Format.fprintf Format.str_formatter
                        "TODO: bytes_to_qt: handle mismatched types: %s <=> %a"
                        (Pretty.sprint 0 (Cil.d_type () typ))
                        QualType.printer qt;
                    Format.flush_str_formatter ()
                end
        end
    in
    bytes_to_qt state pre bytes exp qt


(** Convert a symbolic memory frame to qualifier constraints on all qualified types corresponding to variables in the
    memory frame.
    @param file         the file begin analyzed
    @param expState     the original CilQual state
    @param state        the symbolic execution state to update the constraints with
    @param frame        the symbolic memory frame to re-initialize
    @return             [state]{i monad} the symbolic execution state in the CilQual monad updated with additional
                        constraints.
*)
let frame_to_qt file expState state frame =
    Types.VarinfoMap.fold begin fun v deferred_lval stateM -> perform
        state <-- stateM;
        let state, lval_block = MemOp.state__force state deferred_lval in
        let lval = Cil.var v in
        let exp = Cil.Lval lval in
        qtl <-- access_rval lval;
        match attempt_deref state lval_block v.Cil.vtype with
            | Some (state, bytes) -> bytes_to_qt file expState state Bytes.Guard_True bytes exp qtl
            | None -> return state
    end frame (return state)

