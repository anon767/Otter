(** Module for initializing pointers symbolically using a pointer analysis.

    The symbolic pointer representation should be only as complex as necessary to distinguish between the targets that
    are actually reachable via other variables/pointers, ideally, by incrementally refining the memory at run time as
    additional variables/pointers are seen during symbolic execution.

    For example, assume a pointer p that may point to t or u:
        - p -> \{ x, y \}
    If all variables/pointers are seen or dereferenced, the memory should represent all possible aliasing
    relationships, i.e.:
        - p = IF b THEN &x ELSE &y
        - x = u
        - y = v
    where b is a boolean aliasing condition (i.e., b iff p == &x), and u and v are the values of variables x and y
    respectively. However, for programs with many variables, eagerly initializing the memory to the most general
    representation is quite cost prohibitive.


    Instead, it is desirable to incrementally refine the memory as additional variables/pointers are seen. Consider
    the above example again: when p is first seen, only one location needs to be initialized at first since only one
    location can be reached from p. E.g., it can be initialized to a fresh location z, which represents either x or y:
        - p = &z
    Likewise, when p (i.e., &z) is dereferenced, then z is initialized to a fresh symbolic value w, which represents
    either u or v:
        - z = w
    However, when x is seen, then &z and w needs to be refined to now take into account the possibility that p
    does not alias &x, which can done by:
        - introducing a fresh boolean b that is the aliasing condition (i.e., b iff p aliases &x),
        - replacing z with IF b THEN &x ELSE &y,
        - replacing w with IF b THEN u ELSE v (to account for prior reads via *p),
        - replacing the assignments z = w with x = u and y = v.
    Following this procedure leads to the same memory as in the prior example.

    Unfortunately, Otter's current memory model has some limitations that prevents the above from being implemented
    efficiently:
        1. Otter does not currently provide any way to refine symbolic pointers (other than brute force), so pointer
            pointer values are fixed once initialized and cannot be refined dynamically (non-pointer values can be
            refined by adding an STP constraint);
        2. Otter allocates a contiguous memory block per variable/malloc, and pointers to different field/index
            offsets have different representation (e.g., a pointer to an int vs. a pointer to a field of a struct),
            which means that the pointer value typically cannot be initialized as just a simple pointer to it's target
            type (since it cannot be later refined), but has to be complex enough to represent all possible offset it
            can point to.


    Due to the first limitation where pointer values cannot be refined once initialized, this module takes a different
    approach to introducing disjunctions at newly seen variables/pointers. In the previous incremental example, after
    p is seen and dereferenced, the memory would be initialized to:
        - p = &z
        - z = w
    where again, &z represents either &x or &y and z is initialized to w which represents either u or v. But when x is
    seen, instead of refining the z and w, the address &x is made a disjunction instead:
        - &x == IF b THEN &x' ELSE &z
    and the value of x' initialized as usual:
        - x' = u
    Since Otter treats an occurrence of x as equivalent to *(&x), accessing x is would access either x' or z. Likewise
    y is initialized when seen:
        - &y == IF b' THEN &y' ELSE &z
        - y' = v
    This final memory encodes all possible aliasing relationships, however, it has the drawback of also encoding some
    aliasing relationships that are infeasible, e.g., if b and b' are both false, &x aliases &y, which is not possible
    in C.


    To account for the second limitation, this module first groups all the targets of the pointers by the compound
    type it points into (structs or arrays), and then generates a pointer that may points to each target offset into
    those types.

*)

(* TODO: this module currently assumes that _the entire memory_ is initialized via this module *)

open DataStructures
open OcamlUtilities
open CilUtilities
open OtterBytes


(**/**) (* Various internal helper modules for init_pointer and init_lval_block *)
module OffsetSet = Set.Make (struct
    type t = Cil.offset
    let compare = Pervasives.compare
end)

module VarinfoMap = struct
    include Map.Make (struct
        type t = Cil.varinfo
        let compare x y = Pervasives.compare x.Cil.vid y.Cil.vid
    end)
    let add var offset var_map =
        let offsets = try find var var_map with Not_found -> OffsetSet.empty in
        add var (OffsetSet.add offset offsets) var_map
end

module VarinfoSet = Set.Make (struct
    type t = Cil.varinfo
    let compare x y = Pervasives.compare x.Cil.vid y.Cil.vid
end)

module TypeAndOffsetSetMap = struct
    include Map.Make (struct
        type t = Cil.typ * OffsetSet.t
        let compare (xt, xo) (yt, yo) =
            let canonicalize_type t = Cil.typeSigWithAttrs (fun _ -> []) t in
            match Pervasives.compare (canonicalize_type xt) (canonicalize_type yt) with
                | 0 -> OffsetSet.compare xo yo
                | i -> i
    end)
    let add_empty type_and_offsets type_and_offsets_map =
        if mem type_and_offsets type_and_offsets_map then
            type_and_offsets_map
        else
            add type_and_offsets VarinfoSet.empty type_and_offsets_map
    let add type_and_offsets var type_and_offsets_map =
        let vars = try find type_and_offsets type_and_offsets_map with Not_found -> VarinfoSet.empty in
        add type_and_offsets (VarinfoSet.add var vars) type_and_offsets_map
end

module BytesSet = Set.Make (struct
    type t = Bytes.bytes
    let compare = Pervasives.compare
end)

let fold_struct f acc compinfo =
    List.fold_left f acc compinfo.Cil.cfields

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
(**/**)


(** Initialize pointer values symbolically using a pointer analysis.

    Three parameters are required to determine the pointer value:
        - a [target_type] for the case where the pointer type does not match the target (e.g., void * pointers);
        - a [points_to] function that takes a pointer expression and returns the targets of that expression
            (i.e., the results of a pointer analysis);
        - and [exps] a list of expression representing possible values of the pointer, i.e., aliases of the pointer
            to initialize.

    Pointers are initialized as a conditional tree of concrete pointers to all their targets. When dereferenced, the
    pointer targets are initialized lazily via the [init_target] parameter, given the target type as well as the list
    of variables that are aliases of the target. The list of variables may be empty if the target is only dynamically
    allocated, in which case the target type as well as the pointer itself should be used to initialize the target,
    e.g., take the dereference of the pointer expression as the initial expression for the target.

    Both this function and it's parameter [init_target] take a list of expressions and a list of variables respectively
    so that pointers or their targets may be initialized to represent a number of aliases at once, to minimize the
    size of pointers and targets in the symbolic state.

    Note: this module currently assumes that the initial symbolic state is completely initialized via this module and
    with the same pointer analysis.

        @param state is the symbolic executor state in which to initialize the pointer
        @param target_type is the type of the pointer target (which may not match the pointer type, e.g., for void *
                pointers)
        @param points_to is a function of type [Cil.exp -> (Cil.varinfo * Cil.offset) list * (Cil.varinfo * string) list]
                that takes an expression and returns the points-to targets of that expression as a list of variables
                and offsets pairs, and a list of dynamic allocations sites distinguished by unique string names
        @param exps is list of expressions, which are joined to compute the pointer value
        @param maybe_null optionally indicates whether the pointer should possibly be null (default: true)
        @param maybe_uninit optionally indicates whether the pointer should possible be uninitialized (default: false)
        @param block_name is a name to give the target block of the pointer
        @param init_target is a function of type [Cil.typ -> Cil.varinfo list -> (Types.state -> Types.state * Bytes.bytes)]
                that takes a type and a list of variables that are the targets of the pointer, and initializes a
                deferred symbolic value is the join of the values of all the targets of the pointer; the target list
                may be empty in the case where the target is only dynamically allocated, in which case the target type
                as well as the pointer itself should be used to initialized the target
        @return [(Types.state, Bytes.bytes)] the updated symbolic state and the initialized pointer value
*)
let init_pointer state target_type points_to exps ?(maybe_null=true) ?(maybe_uninit=false) block_name init_target =
    (* find the points to set for this pointer *)
    let target_lvals, malloc_sites = List.fold_left begin fun (target_lvals, malloc_sites) exp ->
        let t, m = points_to exp in
        (t @ target_lvals, m @ malloc_sites)
    end ([], []) exps in

    (* group targets by base varinfo (e.g., root of structs) and offsets, from the list of targets *)
    let var_to_offsets = List.fold_left (fun var_to_offsets (var, offset) -> VarinfoMap.add var offset var_to_offsets) VarinfoMap.empty target_lvals in

    (* group targets by type and the sets of offsets pointed into (i.e., reverse-grouping the above map) *)
    let type_and_offsets_to_vars =
        VarinfoMap.fold
            (fun var offsets -> TypeAndOffsetSetMap.add (var.Cil.vtype, offsets) var)
            var_to_offsets TypeAndOffsetSetMap.empty
    in

    (* if there are malloc targets, then make sure that target_type is always available *)
    let type_and_offsets_to_vars = if malloc_sites <> [] then
        TypeAndOffsetSetMap.add_empty (target_type, OffsetSet.singleton Cil.NoOffset) type_and_offsets_to_vars
    else
        type_and_offsets_to_vars
    in

    let state, target_bytes_set, _ = TypeAndOffsetSetMap.fold
        begin fun (typ, offsets) target_vars (state, target_bytes_set, count) ->
            (* generate a list of bytes pointing to the given offsets in block *)
            let map_offsets target_bytes_set block =
                OffsetSet.fold begin fun offset target_bytes_set ->
                    (* for every offset, generate bytes pointing to each offset *)
                    let offset_bits, _ = Cil.bitsOffset typ offset in
                    let offset_bytes = Bytes.int_to_bytes (offset_bits / 8) in
                    let target_bytes = Bytes.make_Bytes_Address (block, offset_bytes) in
                    BytesSet.add target_bytes target_bytes_set
                end offsets target_bytes_set
            in

            (* conservatively make a fresh block and point to it; though this is really only necessary if we point into:
                    - at least one dynamically allocated block (i.e., malloc);
                    - at least one local variable;
                    - or more than one variable, local or global, that has not been initialized.
            *)
            (* TODO: don't make a fresh block if one of the above conditions aren't fulfilled. *)
            (* TODO: currently, this only initializes a single element, rather than an array; should find some way to
                    figure out if this pointer points to arrays, and initialize it so. *)
            let state, block, target_bytes_set =
                let size = Cil.bitsSizeOf (Cil.unrollType typ) / 8 in
                let block = Bytes.block__make (FormatPlus.sprintf "%s#%d size(%d) type(%a)" block_name count size Printcil.typ typ) size Bytes.Block_type_Aliased in
                let deferred = init_target typ (VarinfoSet.elements target_vars) in
                let state = MemOp.state__add_deferred_block state block deferred in
                (state, block, map_offsets target_bytes_set block)
            in

            (* conservatively point to all allocations in the list of allocations of each malloc site (for each type,
                since we don't know what type is being allocated at each site), and add the above allocated fresh block
                to the list of allocations for each malloc site *)
            let state, target_bytes_set = List.fold_left begin fun (state, target_bytes_set) malloc ->
                let mallocs_map = try Types.MallocMap.find malloc state.Types.mallocs with Not_found -> Types.TypeMap.empty in
                let mallocs = try Types.TypeMap.find typ mallocs_map with Not_found -> [] in
                let state =
                    let mallocs_map = Types.TypeMap.add typ (block::mallocs) mallocs_map in
                    { state with Types.mallocs = Types.MallocMap.add malloc mallocs_map state.Types.mallocs }
                in
                (state, List.fold_left map_offsets target_bytes_set mallocs)
            end (state, target_bytes_set) malloc_sites in

            (* conservatively point to all aliases in the list of aliases of each variable, and add the above allocated
                fresh block to the list of aliases of each variable; though we don't really need to add to global
                variables that have been initialized *)
            (* TODO: mark global variables that have been initialized and avoid adding the fresh block to it *)
            let state, target_bytes_set = VarinfoSet.fold begin fun v (state, target_bytes_set) ->
                let aliases = try Types.VarinfoMap.find v state.Types.aliases with Not_found -> [] in
                let state = { state with Types.aliases = Types.VarinfoMap.add v (block::aliases) state.Types.aliases } in
                (state, List.fold_left map_offsets target_bytes_set aliases)
            end target_vars (state, target_bytes_set) in
            (state, target_bytes_set, count + 1)
        end
    type_and_offsets_to_vars (state, BytesSet.empty, 0) in

    (* add a null pointer *)
    let target_bytes_set = if maybe_null then BytesSet.add Bytes.bytes__zero target_bytes_set else target_bytes_set in

    (* add an uninitialized pointer *)
    let target_bytes_set = if maybe_uninit then BytesSet.add Bytes.bytes__zero target_bytes_set else target_bytes_set in

    (* finally, return a MayBytes pointing to the targets *)
    let target_bytes = match BytesSet.elements target_bytes_set with
        | [] -> Bytes.conditional__bytes (Bytes.bytes__symbolic (Cil.bitsSizeOf Cil.voidPtrType / 8)) (* uninitialized pointer *)
        | target_bytes_list  -> Bytes.conditional__from_list (List.map Bytes.conditional__bytes target_bytes_list)
    in
    (state, Bytes.make_Bytes_Conditional target_bytes)



(** Initialize variables symbolically, taking into account aliases that may have been initialized via {!init_pointer}.

    Note: this module currently assumes that the initial symbolic state is completely initialized via this module and
    with the same pointer analysis.

        @param state is the symbolic executor state in which to initialize the variable
        @param var is the variable to initialize
        @param block_name is a name to give the target block of the variable
        @param deferred is the deferred symbolic value for the variable
        @return [(Types.state, Bytes.bytes)] the updated symbolic state and the initialized variable
*)
let init_lval_block state var block_name deferred =
    let deferred_lval_block state =
        (* make an extra block; for the case where the variable is not-aliased *)
        let block = Bytes.block__make block_name (Cil.bitsSizeOf (Cil.unrollType var.Cil.vtype) / 8) Bytes.Block_type_Aliased in
        let state = MemOp.state__add_deferred_block state block deferred in
        let aliases = block::(try Types.VarinfoMap.find var state.Types.aliases with Not_found -> []) in
        let state = { state with Types.aliases = Types.VarinfoMap.add var aliases state.Types.aliases } in

        (* generate an lval to each block it may alias that was previously initialized via another pointer *)
        let target_list = List.map (fun block -> Bytes.conditional__lval_block (block, Bytes.bytes__zero)) aliases in

        (* finally, return a Lval_May pointing to the targets *)
        (state, Bytes.conditional__from_list target_list)
    in
    (state, Deferred.Deferred deferred_lval_block)



(** Initialize program values symbolically, using {!init_pointer} to initialize pointers.

    Note: this module currently assumes that the initial symbolic state is completely initialized via this module and
    with the same pointer analysis.

        @param state is the symbolic executor state in which to initialize the variable
        @param typ is the type of the value to initialize
        @param points_to is a function for computing pointer targets to be passed to {!init_pointer}
        @param exps is list of expressions, which are joined to compute the program value
        @return [(Types.state, Bytes.bytes)] the updated symbolic state and the initialized variable
*)
let rec init_bytes_with_pointers state typ points_to exps = match Cil.unrollType typ with
    | Cil.TPtr (Cil.TFun _, _) ->
        (* TODO: use a points to analysis to resolve the target functions *)
        let bytes = Bytes.bytes__symbolic (Cil.bitsSizeOf typ / 8) in
        let null_bytes = Bytes.bytes__zero in
        let bytes = Bytes.make_Bytes_Conditional (Bytes.IfThenElse (
                Bytes.guard__symbolic (), Bytes.conditional__bytes bytes, Bytes.conditional__bytes null_bytes))
        in
        (state, bytes)

    | Cil.TPtr (target_type, _) ->
        (* for pointers, generate the pointer *)
        let init_target typ vars state =
            if vars = [] then begin
                let exps = List.map (fun exp -> Cil.Lval (Cil.Mem exp, Cil.NoOffset)) exps in
                if exps <> [] then
                    init_bytes_with_pointers state typ points_to exps
                else
                    let size = Cil.bitsSizeOf typ / 8 in
                    (state, Bytes.bytes__symbolic size)
            end else
                let exps = List.map (fun v -> Cil.Lval (Cil.var v)) vars in
                init_bytes_with_pointers state typ points_to exps
        in

        (* give it a name *)
        let block_name = FormatPlus.as_string Printcil.exp (List.hd exps) in

        (* finally, make the pointer *)
        init_pointer state target_type points_to exps block_name init_target

    | Cil.TComp (compinfo, _) when compinfo.Cil.cstruct ->
        (* for structs, initialize and iterate over the fields *)
        let size = Cil.bitsSizeOf typ / 8 in
        let bytes = Bytes.bytes__symbolic size in
        fold_struct begin fun (state, bytes) field ->
            let field_offset = Cil.Field (field, Cil.NoOffset) in
            let field_exps = List.map begin function
                | Cil.Lval lval -> Cil.Lval (Cil.addOffsetLval field_offset lval)
                | _ -> failwith "are there any other Cil.exp that can have type Cil.TComp?"
            end exps in
            let state, field_bytes = init_bytes_with_pointers state (Cil.typeOffset typ field_offset) points_to field_exps in
            let offset, size = Cil.bitsOffset typ field_offset in
            let offset = Bytes.int_to_bytes (offset / 8) in
            let size = size / 8 in
            let bytes = BytesUtility.bytes__write bytes offset size field_bytes in
            (state, bytes)
        end (state, bytes) compinfo

    | Cil.TArray (el_typ, len_opt, _) ->
        (* for arrays, initialize each element of the array *)
        let size = Cil.bitsSizeOf typ / 8 in
        let bytes = Bytes.bytes__symbolic size in
        let result_opt = fold_array begin fun (state, bytes) index ->
            let el_offset = Cil.Index (index, Cil.NoOffset) in
            let el_exps = List.map begin function
                | Cil.Lval lval -> Cil.Lval (Cil.addOffsetLval el_offset lval)
                | _ -> failwith "are there any other Cil.exp that can have type Cil.TArray?"
            end exps in
            let state, el_bytes = init_bytes_with_pointers state el_typ points_to el_exps in
            let offset, size = Cil.bitsOffset typ el_offset in
            let offset = Bytes.int_to_bytes (offset / 8) in
            let size = size / 8 in
            let bytes = BytesUtility.bytes__write bytes offset size el_bytes in
            (state, bytes)
        end (state, bytes) len_opt in
        begin match result_opt with
            | Some result -> result
            | None -> (state, bytes)
        end

    | Cil.TComp (compinfo, _) when not compinfo.Cil.cstruct ->
        (* we can't handle unions *)
        FormatPlus.failwith "TODO: init_bytes_with_pointers: handle unions: %a" Printcil.typ typ

    | typ when Cil.isArithmeticType typ ->
        (* arithmetic values are just that *)
        let size = Cil.bitsSizeOf typ / 8 in
        let bytes = Bytes.bytes__symbolic size in
        (state, bytes)

    | typ ->
        FormatPlus.failwith "TODO: init_bytes_with_pointers: unhandled type: %a" Printcil.typ typ



(** Create a new symbolic executor job starting at a given function, and using {!init_bytes_with_pointers} to initialize
    the symbolic state.

        @param file is the file to symbolically execute
        @param points_to is a function for computing pointer targets to be passed to {!init_pointer} (default:[CilPtranal.points_to file])
        @param fn is list the function at which to begin symbolic execution
        @return [Types.job] the created job
*)
let job_for_middle file ?(points_to=CilPtranal.points_to file) fn =
    (* initialize the state with symbolic globals *)
    let state = MemOp.state__empty in

    (* first, setup global variables *)
    let state = List.fold_left begin fun state g -> match g with
        | Cil.GVarDecl (v, _) | Cil.GVar (v, _, _)
                when not (Cil.isFunctionType v.Cil.vtype (* skip function prototypes; they're not variables *)
                    || Types.VarinfoMap.mem v state.Types.global) ->
            let deferred state = init_bytes_with_pointers state v.Cil.vtype points_to [ (Cil.Lval (Cil.var v)) ] in
            let state, lval_block = init_lval_block state v v.Cil.vname deferred in
            { state with Types.global=Types.VarinfoMap.add v lval_block state.Types.global }
        | _ ->
            state
    end state file.Cil.globals in

    (* then, setup function arguments *)
    (* TODO: handle varargs *)
    let state, rev_args_bytes = List.fold_left begin fun (state, args_bytes) v ->
        let state, bytes = init_bytes_with_pointers state v.Cil.vtype points_to [ (Cil.Lval (Cil.var v)) ] in
        (state, bytes::args_bytes)
    end (state, []) fn.Cil.sformals in

    (* finally, prepare the function call job *)
    Driver.job_for_function file state fn (List.rev rev_args_bytes)

