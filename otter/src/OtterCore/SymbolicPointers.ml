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
module OffsetSet = Set.Make (CilData.CilOffset)
module VarinfoSet = Set.Make (CilData.CilVar)
module LhostSet = Set.Make (CilData.CilLhost)
module ExpSet = Set.Make (CilData.CilExp)

module VarinfoMap = Map.Make (CilData.CilVar)
module MallocMap = Map.Make (CilData.Malloc)

module VarinfoOffsetSet = Set.Make (struct
    type t = CilData.CilVar.t * CilData.CilOffset.t
    let compare (xv, xo as x) (yv, yo as y) = if x == y then 0 else match CilData.CilVar.compare xv yv with
        | 0 -> CilData.CilOffset.compare xo yo
        | i -> i
end)

module MallocLhostsOffsetSet = Set.Make (struct
    type t = CilData.Malloc.t * CilData.CilLhost.t list  * CilData.CilOffset.t
    let compare (xm, xl, xo as x) (ym, yl, yo as y) = if x == y then 0 else match CilData.Malloc.compare xm ym with
        | 0 ->
            begin match Pervasives.compare xl yl with
                | 0 -> CilData.CilOffset.compare xo yo
                | i -> i
            end
        | i ->
            i
end)

module TypeAndOffsetSetMap = struct
    include Map.Make (struct
        type t = CilData.CilCanonicalType.t * OffsetSet.t
        let compare (xt, xo) (yt, yo) =
            match CilData.CilCanonicalType.compare xt yt with
                | 0 -> OffsetSet.compare xo yo
                | i -> i
    end)
    let add_empty type_and_offsets type_and_offsets_map =
        if mem type_and_offsets type_and_offsets_map then
            type_and_offsets_map
        else
            add type_and_offsets (VarinfoSet.empty, MallocMap.empty) type_and_offsets_map
    let add_varinfo type_and_offsets varinfo type_and_offsets_map =
        let varinfos, mallocs = try find type_and_offsets type_and_offsets_map with Not_found -> (VarinfoSet.empty, MallocMap.empty) in
        add type_and_offsets (VarinfoSet.add varinfo varinfos, mallocs) type_and_offsets_map
    let add_malloc type_and_offsets malloc malloc_lhosts type_and_offsets_map =
        let varinfos, mallocs = try find type_and_offsets type_and_offsets_map with Not_found -> (VarinfoSet.empty, MallocMap.empty) in
        let malloc_lhosts' = try MallocMap.find malloc mallocs with Not_found -> LhostSet.empty in
        let malloc_lhosts = LhostSet.union malloc_lhosts' malloc_lhosts in
        let mallocs = MallocMap.add malloc malloc_lhosts mallocs in
        add type_and_offsets (varinfos, mallocs) type_and_offsets_map
end

module BytesSet = Set.Make (struct
    type t = Bytes.bytes
    let compare = Pervasives.compare
end)
(**/**)


(* Options *)
let schemes = [
    "one-level", `OneLevel;
    "two-level", `TwoLevel;
    "constraint-offset", `ConstraintOffset;
]

let default_scheme = ref `TwoLevel


(** Initialize pointer values symbolically using a pointer analysis.

    Two parameters are required to determine the pointer value:
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

    Note: this module currently assumes that the initial symbolic job is completely initialized via this module and
    with the same pointer analysis.

        @param scheme optionally indicates the scheme to initialize symbolic pointers, which are
                    - [`OneLevel]: conditionals of base-offset pairs,
                    - [`TwoLevel]: conditionals of bases, and conditionals of offsets for each distinct base,
                    - [`ConstraintOffset]: conditionals of bases, and symbolic offsets with constraints for each distinct base;
                (default: [`TwoLevel])
        @param job is the symbolic executor job in which to initialize the pointer
        @param points_to is a function of type [Cil.exp -> (CilData.CilVar.t * CilData.CilOffset) list
                * (CilData.Malloc.t * CilData.CilLhost.t list  * CilData.CilOffset) list]
                that takes an expression and returns the points-to targets of that expression as a list of variables
                and offsets pairs, and a list of dynamic allocations sites distinguished by unique string names along
                with the allocated type, list of potential lhosts, and target offset
        @param exps is list of expressions, which are joined to compute the pointer value
        @param maybe_null optionally indicates whether the pointer should possibly be null (default: true)
        @param maybe_uninit optionally indicates whether the pointer should possible be uninitialized (default: false)
        @param block_name is a name to give the target block of the pointer
        @param init_target is a function of type [Cil.typ -> CilData.CilVar.t list -> CilData.Malloc.t list -> (State.t -> State.t * Bytes.bytes)]
                that takes a type and a list of variables that are the targets of the pointer, and initializes a
                deferred symbolic value is the join of the values of all the targets of the pointer; the target list
                may be empty in the case where the target is only dynamically allocated, in which case the target type
                as well as the pointer itself should be used to initialized the target
        @return [(job, bytes)] the updated symbolic executor job and the initialized pointer value
*)
let init_pointer
            ?(scheme=(!default_scheme))
            job
            points_to
            exps
            ?(maybe_null=true)
            ?(maybe_uninit=false)
            block_name
            init_target
        = Profiler.global#call "SymbolicPointers.init_pointer" begin fun () ->

    (* find the points-to set for this pointer, and separate into lvals, mallocs and function addresses, the latter in
       particular has to be treated separately since function addresses do not refer to storage in memory *)
    let target_functions, target_lvals, target_mallocs = Profiler.global#call "query points-to" begin fun () ->
        (* eliminate duplicates in the input and output; they can add up quickly (>10 times) *)
        let exps = List.fold_left (fun exps exp -> ExpSet.add exp exps) ExpSet.empty exps in
        ExpSet.fold begin fun exp (target_functions, target_lvals, target_mallocs) -> Profiler.global#call "partition" begin fun () ->
            let t, m = points_to exp in
            let target_functions, target_lvals = List.fold_left begin fun (target_functions, target_lvals) (v, o as t) ->
                Profiler.global#call "functions/lvals" begin fun () ->
                    if Cil.isFunctionType v.Cil.vtype then
                        if o <> Cil.NoOffset then
                            FormatPlus.invalid_arg "SymbolicPointers.init_pointer: invalid %a offset from function address %s" Printcil.offset o v.Cil.vname
                        else
                            (VarinfoSet.add v target_functions, target_lvals)
                    else
                        (target_functions, VarinfoOffsetSet.add t target_lvals)
                end
            end (target_functions, target_lvals) t in
            let target_mallocs = List.fold_left begin fun target_mallocs m ->
                Profiler.global#call "mallocs" (fun () -> MallocLhostsOffsetSet.add m target_mallocs)
            end target_mallocs m in
            (target_functions, target_lvals, target_mallocs)
        end end exps (VarinfoSet.empty, VarinfoOffsetSet.empty, MallocLhostsOffsetSet.empty)
    end in

    (* group targets by type and the sets of offsets pointed into *)
    let type_and_offsets_to_targets = Profiler.global#call "coalesce" begin fun () ->
        let type_and_offsets_to_targets = TypeAndOffsetSetMap.empty in

        (* group variable targets by base varinfo (e.g., root of structs) and offsets, then reverse the mapping *)
        let varinfo_to_offsets = VarinfoOffsetSet.fold begin fun (varinfo, offset) varinfo_to_offsets ->
            let offsets = try VarinfoMap.find varinfo varinfo_to_offsets with Not_found -> OffsetSet.empty in
            VarinfoMap.add varinfo (OffsetSet.add offset offsets) varinfo_to_offsets
        end target_lvals VarinfoMap.empty in
        let type_and_offsets_to_targets = VarinfoMap.fold begin fun varinfo offsets ->
            TypeAndOffsetSetMap.add_varinfo (varinfo.Cil.vtype, offsets) varinfo
        end varinfo_to_offsets type_and_offsets_to_targets in

        (* group malloc targets by allocated type and offsets; then reverse the mapping *)
        let malloc_to_offsets = MallocLhostsOffsetSet.fold begin fun (malloc, malloc_lhosts', offset) malloc_to_offsets ->
            let offsets, malloc_lhosts = try MallocMap.find malloc malloc_to_offsets with Not_found -> (OffsetSet.empty, LhostSet.empty) in
            let offsets = OffsetSet.add offset offsets in
            let malloc_lhosts = List.fold_left (fun malloc_lhosts malloc_exp -> LhostSet.add malloc_exp malloc_lhosts) malloc_lhosts malloc_lhosts' in
            MallocMap.add malloc (offsets, malloc_lhosts) malloc_to_offsets
        end target_mallocs MallocMap.empty in
        let type_and_offsets_to_targets = MallocMap.fold begin fun (_, _, typ as malloc) (offsets, malloc_lhosts) ->
            TypeAndOffsetSetMap.add_malloc (typ, offsets) malloc malloc_lhosts
        end malloc_to_offsets type_and_offsets_to_targets in

        type_and_offsets_to_targets
    end in

    let job, target_bytes_set, _ = Profiler.global#call "make bytes" begin fun () ->
        TypeAndOffsetSetMap.fold begin fun (typ, offsets) (varinfos, mallocs) (job, target_bytes_set, count) ->
            let job, map_offsets = match scheme with
                | `OneLevel ->
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
                    (job, map_offsets)

                | `TwoLevel ->
                    (* generate a bytes that represent the given offsets as a conditional value *)
                    let offset_bytes =
                        let offset_bytes_set = OffsetSet.fold begin fun offset offset_bytes_set ->
                            (* generate bytes pointing for each offset *)
                            let offset_bits, _ = Cil.bitsOffset typ offset in
                            let offset_bytes = Bytes.int_to_bytes (offset_bits / 8) in
                            BytesSet.add offset_bytes offset_bytes_set
                        end offsets BytesSet.empty in
                        let conditional_bytes_list = List.map Bytes.conditional__bytes (BytesSet.elements offset_bytes_set) in
                        Bytes.make_Bytes_Conditional (Bytes.conditional__from_list conditional_bytes_list)
                    in

                    (* generate a list of bytes pointing to the given offsets in block *)
                    let map_offsets target_bytes_set block =
                        let target_bytes = Bytes.make_Bytes_Address (block, offset_bytes) in
                        BytesSet.add target_bytes target_bytes_set
                    in
                    (job, map_offsets)

                | `ConstraintOffset ->
                    (* generate a bytes that represent the given offsets as a symbolic value with constraints *)
                    let offset_bytes = Bytes.bytes__symbolic (Cil.bitsSizeOf !Cil.upointType / 8) in

                    (* generate constraints for each offset *)
                    let offset_constraints_set = OffsetSet.fold begin fun offset offset_constraints_set ->
                        let offset_bits, _ = Cil.bitsOffset typ offset in
                        let constraint_bytes = Bytes.make_Bytes_Op (Bytes.OP_EQ,
                            [ (offset_bytes, !Cil.upointType); (Bytes.int_to_offset_bytes (offset_bits / 8), !Cil.upointType) ])
                        in
                        BytesSet.add constraint_bytes offset_constraints_set
                    end offsets BytesSet.empty in

                    (* append the constraints to the path condition *)
                    let offset_constraints = ListPlus.foldm begin fun x y ->
                        Bytes.make_Bytes_Op (Bytes.OP_LOR, [ (x, Cil.intType); (y, Cil.intType) ])
                    end (BytesSet.elements offset_constraints_set) in
                    let job = MemOp.state__add_path_condition job offset_constraints false in

                    (* generate a list of bytes pointing to the given offsets in block *)
                    let map_offsets target_bytes_set block =
                        let target_bytes = Bytes.make_Bytes_Address (block, offset_bytes) in
                        BytesSet.add target_bytes target_bytes_set
                    in
                    (job, map_offsets)
            in

            (* conservatively make a fresh block and point to it; though this is really only necessary if we point into:
                    - a dynamically allocated block (i.e., malloc) that could have been allocated before the symbolic
                        execution entry point, up to the number of pointed-to dynamic allocations that could exist
                        before that point;
                    - or a local variable in a stack frame that could occur in the call stack at the symbolic execution
                        entry point ("below" the call stack), up to the total number of pointed-to-local-variables--by-
                        -stack-frames that can occur below the call stack;
                    - or a global variable, up to the number pointed-to global variables.

                In addition, for local variables that can occur only at most once below the stack, or global variables,
                once they have been allocated a block for storage (i.e., after forcing the deferred allocation from
                {!init_lval_block}), the fresh block no longer needs to be assigned to them.

                An alternative way to think of the above is:
                    - there are N allocation slots, where N corresponds to the total number of dynamic allocation calls
                        that can occur before the symbolic execution entry point, total number of local-variables--by-
                        stack-frames that could occur below the call stack, and the total number global variables, that
                        can be pointed into; where N can be infinite;
                    - the upper bound of the number of fresh blocks that need to be created is the lower of the number
                        of slots N, or the number of roots (pointers or variables) that can reach those blocks; more
                        that that is not necessary to distinguish between possible aliasing relationships.
            *)
            (* TODO: figure out how to implement the above completely; the below assumes that any local variable may
                occur infinitely below the call stack *)

            (* first figure out if we need to make a fresh block *)
            let need_fresh = not begin
                MallocMap.is_empty mallocs
                && VarinfoSet.for_all (fun v -> v.Cil.vglob && Deferred.is_forced (State.VarinfoMap.find v job#state.State.global)) varinfos
            end in

            if need_fresh then
                (* has mallocs or locals or globals without allocated storage *)
                (* TODO: currently, this only initializes a single element, rather than an array; should find some way to
                        figure out if this pointer points to arrays, and initialize it so *)
                let job, block, target_bytes_set =
                    let size = Cil.bitsSizeOf typ / 8 in
                    let block = Bytes.block__make
                        (FormatPlus.sprintf "%s#%d size(%d) type(%a)" block_name count size Printcil.typ typ)
                        (Bytes.int_to_bytes size)
                        Bytes.Block_type_Aliased
                    in
                    let varinfos = VarinfoSet.elements varinfos in
                    let mallocs = MallocMap.fold (fun malloc exps mallocs -> (malloc, LhostSet.elements exps)::mallocs) mallocs [] in
                    let deferred = init_target typ varinfos mallocs in
                    let job = MemOp.state__add_deferred_block job block deferred in
                    (job, block, map_offsets target_bytes_set block)
                in

                (* conservatively point to all allocations in the list of allocations of each malloc site, and add the
                    above allocated fresh block to the list of allocations for each malloc site *)
                let job, target_bytes_set = MallocMap.fold begin fun malloc _ (job, target_bytes_set) ->
                    let blocks = try State.MallocMap.find malloc job#state.State.mallocs with Not_found -> [] in
                    let job = job#with_state { job#state with State.mallocs = State.MallocMap.add malloc (block::blocks) job#state.State.mallocs } in
                    (job, List.fold_left map_offsets target_bytes_set blocks)
                end mallocs (job, target_bytes_set) in

                (* conservatively point to all aliases in the list of aliases of each variable, and add the above allocated
                    fresh block to the list of aliases of each variable, except for global variables with allocated storage *)
                let job, target_bytes_set = VarinfoSet.fold begin fun v (job, target_bytes_set) ->
                    let blocks = try State.VarinfoMap.find v job#state.State.aliases with Not_found -> [] in
                    (* assuming that deferred lval_blocks were set up by this module only *)
                    let job = if v.Cil.vglob && Deferred.is_forced (State.VarinfoMap.find v job#state.State.global) then
                        job
                    else
                        job#with_state { job#state with State.aliases = State.VarinfoMap.add v (block::blocks) job#state.State.aliases }
                    in
                    (job, List.fold_left map_offsets target_bytes_set blocks)
                end varinfos (job, target_bytes_set) in
                (job, target_bytes_set, count + 1)
            else
                (* no mallocs or locals, only globals with allocated storage *)
                let job, target_bytes_set = VarinfoSet.fold begin fun v (job, target_bytes_set) ->
                    let blocks = try State.VarinfoMap.find v job#state.State.aliases with Not_found -> [] in
                    (job, List.fold_left map_offsets target_bytes_set blocks)
                end varinfos (job, target_bytes_set) in
                (job, target_bytes_set, count)
        end type_and_offsets_to_targets (job, BytesSet.empty, 0)
    end in

    (* add a null pointer *)
    let target_bytes_set = if maybe_null then BytesSet.add Bytes.bytes__zero target_bytes_set else target_bytes_set in

    (* add the function addresses *)
    let target_bytes_set = VarinfoSet.fold begin fun fn target_bytes_set ->
        BytesSet.add (Bytes.make_Bytes_FunPtr fn) target_bytes_set
    end target_functions target_bytes_set in

    (* add an uninitialized pointer *)
    let target_bytes_set =
        if BytesSet.is_empty target_bytes_set || maybe_uninit then
            BytesSet.add (Bytes.bytes__symbolic (Cil.bitsSizeOf Cil.voidPtrType / 8)) target_bytes_set (* uninitialized pointer *)
        else
            target_bytes_set
    in

    (* finally, return a Bytes_Conditional pointing to the targets *)
    let target_bytes =
        let conditional_bytes_list = List.map Bytes.conditional__bytes (BytesSet.elements target_bytes_set) in
        Bytes.make_Bytes_Conditional (Bytes.conditional__from_list conditional_bytes_list)
    in
    (job, target_bytes)
end



(** Initialize a variable storage lazily and symbolically, taking into account aliases that may have been initialized
    via {!init_pointer} if necessary. The caller is responsible for assigning the storage to the variable.

    Note: this module currently assumes that the initial symbolic job is completely initialized via this module and
    with the same pointer analysis.

        @param job is the symbolic executor job in which to initialize the variable storage
        @param varinfo is the variable for which to initialize a storage
        @param deferred is the deferred symbolic value for the variable
        @return [(job, lval_block)] the updated symbolic job and the initialized variable storage
*)
let init_lval_block job varinfo deferred = Profiler.global#call "SymbolicPointers.init_lval_block" begin fun () ->
    let deferred_lval_block job =
        (* make an extra block; for the case where the variable is not-aliased *)
        let name = FormatPlus.as_string CilPrinter.varinfo varinfo in
        let size = Bytes.int_to_bytes (Cil.bitsSizeOf varinfo.Cil.vtype / 8) in
        let block = Bytes.block__make name size Bytes.Block_type_Aliased in
        let job = MemOp.state__add_deferred_block job block deferred in
        let aliases = block::(try State.VarinfoMap.find varinfo job#state.State.aliases with Not_found -> []) in
        let job = job#with_state { job#state with State.aliases = State.VarinfoMap.add varinfo aliases job#state.State.aliases } in

        (* generate an lval to each block it may alias that was previously initialized via another pointer *)
        let target_list = List.map (fun block -> Bytes.conditional__lval_block (block, Bytes.bytes__zero)) aliases in

        (* finally, return a Lval_May pointing to the targets *)
        (job, Bytes.conditional__from_list target_list)
    in
    (job, Deferred.Deferred deferred_lval_block)
end



(** Initialize const global variables immediately and concretely.

    Note: this module currently assumes that the initial symbolic job is completely initialized via this module and
    with the same pointer analysis.

        @param job is the symbolic executor job in which to initialize the const global variable
        @param varinfo is the const global variable to initialize
        @param init_opt is either [Some init], an initializer for the const global variable; or [None] for forward
                declarations
        @return [job] the symbolic job updated with the initialized const global variable
*)
let init_const_global job varinfo init_opt = Profiler.global#call "SymbolicPointers.init_const_global" begin fun () ->
    if not (varinfo.Cil.vglob && CilData.CilVar.is_const varinfo) then
        FormatPlus.invalid_arg "SymbolicPointers.init_const_global: %a is not a const global variable" CilPrinter.varinfo varinfo;

    let job, block =
        try
            match State.VarinfoMap.find varinfo job#state.State.aliases with
                | [ block ] when Deferred.is_forced (State.VarinfoMap.find varinfo job#state.State.global) -> (job, block)
                | [] -> raise Not_found
                | _ -> FormatPlus.invalid_arg "SymbolicPointers.init_const_global: %a already initialized" CilPrinter.varinfo varinfo
        with Not_found ->
            (* initialize the block with a dummy value first, for recursive initializations such as 'void * p = &p;' *)
            let job, block = MemOp.state__add_global job varinfo in
            let job = job#with_state { job#state with State.aliases = State.VarinfoMap.add varinfo [ block ] job#state.State.aliases } in
            (job, block)
    in

    match init_opt with
        | None ->
            (* forward declaration; nothing to do *)
            job
        | Some init ->
            let job, init_bytes = Expression.evaluate_initializer job varinfo.Cil.vtype init in
            MemOp.state__add_block job block init_bytes
end


(**/**)
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


(** Initialize program values symbolically, using {!init_pointer} to initialize pointers.

        @param uninit_void optionally indicates whether targets of void * pointers should be uninitialized (default: false)
        @param job is the symbolic executor job in which to initialize the variable
        @param typ is the type of the value to initialize
        @param points_to is a function for computing pointer targets to be passed to {!init_pointer}
        @param exps is list of expressions, which are joined to compute the program value
        @return [(job, bytes)] the updated symbolic job and the initialized variable
*)
let rec init_bytes_with_pointers
            ?(uninit_void=false)
            job
            typ
            points_to
            exps
        = Profiler.global#call "SymbolicPointers.init_bytes_with_pointers" begin fun () ->

    let rec init_bytes_with_pointers_inner job typ points_to exps = match Cil.unrollType typ with
        | Cil.TPtr (Cil.TVoid _, _) when uninit_void ->
            (* for void * pointers that should be uninitialized *)

            (* set up an uninitialized target of the largest target type *)
            let module TypeSet = Set.Make (CilData.CilCanonicalType) in
            let size = List.fold_left begin fun size exp ->
                let target_vars, target_mallocs = points_to exp in
                let size = List.fold_left (fun size (v, _) -> max size (Cil.bitsSizeOf v.Cil.vtype)) size target_vars in
                let size = List.fold_left (fun size ((_, _, typ), _, _) -> max size (Cil.bitsSizeOf typ)) size target_mallocs in
                size
            end 0 exps in
            let block = Bytes.block__make "SymbolicPointers.uninit_void" (Bytes.int_to_bytes size) Bytes.Block_type_Aliased in
            let bytes = Bytes.bytes__symbolic size in
            let job = MemOp.state__add_block job block bytes in

            (* set up the pointer to point to the uninitialized target *)
            (job, Bytes.make_Bytes_Address (block, Bytes.int_to_offset_bytes 0))

        | Cil.TPtr _ ->
            (* for pointers, generate the pointer *)
            let init_target typ vars mallocs job =
                let var_exps = List.map (fun v -> Cil.Lval (Cil.var v)) vars in
                let malloc_exps = List.concat (List.map (fun (_, lhosts) -> List.map (fun lhost -> Cil.Lval (lhost, Cil.NoOffset)) lhosts) mallocs) in
                let exps = var_exps @ malloc_exps in
                if exps <> [] then
                    init_bytes_with_pointers ~uninit_void job typ points_to exps
                else
                    let size = Cil.bitsSizeOf typ / 8 in
                    (job, Bytes.bytes__symbolic size)
            in

            (* give it a name *)
            let block_name = FormatPlus.as_string Printcil.exp (List.hd exps) in

            (* finally, make the pointer *)
            init_pointer job points_to exps block_name init_target

        | Cil.TComp (compinfo, _) when compinfo.Cil.cstruct ->
            (* for structs, initialize and iterate over the fields *)
            let size = Cil.bitsSizeOf typ / 8 in
            let bytes = Bytes.bytes__symbolic size in
            fold_struct begin fun (job, bytes) field ->
                let field_offset = Cil.Field (field, Cil.NoOffset) in
                let field_exps = List.map begin function
                    | Cil.Lval lval -> Cil.Lval (Cil.addOffsetLval field_offset lval)
                    | _ -> failwith "are there any other Cil.exp that can have type Cil.TComp?"
                end exps in
                let job, field_bytes = init_bytes_with_pointers_inner job (Cil.typeOffset typ field_offset) points_to field_exps in
                let offset, size = Cil.bitsOffset typ field_offset in
                let offset = Bytes.int_to_bytes (offset / 8) in
                let size = size / 8 in
                let (), bytes = BytesUtility.bytes__write () bytes offset size field_bytes in
                (job, bytes)
            end (job, bytes) compinfo

        | Cil.TArray (el_typ, len_opt, _) ->
            (* for arrays, initialize each element of the array *)
            let size = Cil.bitsSizeOf typ / 8 in
            let bytes = Bytes.bytes__symbolic size in
            let result_opt = fold_array begin fun (job, bytes) index ->
                let el_offset = Cil.Index (index, Cil.NoOffset) in
                let el_exps = List.map begin function
                    | Cil.Lval lval -> Cil.Lval (Cil.addOffsetLval el_offset lval)
                    | _ -> failwith "are there any other Cil.exp that can have type Cil.TArray?"
                end exps in
                let job, el_bytes = init_bytes_with_pointers_inner job el_typ points_to el_exps in
                let offset, size = Cil.bitsOffset typ el_offset in
                let offset = Bytes.int_to_bytes (offset / 8) in
                let size = size / 8 in
                let (), bytes = BytesUtility.bytes__write () bytes offset size el_bytes in
                (job, bytes)
            end (job, bytes) len_opt in
            begin match result_opt with
                | Some result -> result
                | None -> (job, bytes)
            end

        | Cil.TComp (compinfo, _) when not compinfo.Cil.cstruct ->
            (* for unions, initialize each field and combine them *)
            let job, field_bytes_list = fold_struct begin fun (job, field_bytes_list) field ->
                let field_offset = Cil.Field (field, Cil.NoOffset) in
                let field_exps = List.map begin function
                    | Cil.Lval lval -> Cil.Lval (Cil.addOffsetLval field_offset lval)
                    | _ -> failwith "are there any other Cil.exp that can have type Cil.TComp?"
                end exps in
                let job, field_bytes = init_bytes_with_pointers_inner job (Cil.typeOffset typ field_offset) points_to field_exps in
                (job, (Bytes.conditional__bytes field_bytes)::field_bytes_list)
            end (job, []) compinfo in
            (job, Bytes.make_Bytes_Conditional (Bytes.conditional__from_list field_bytes_list))

        | typ when Cil.isArithmeticType typ ->
            (* arithmetic values are just that *)
            let size = Cil.bitsSizeOf typ / 8 in
            let bytes = Bytes.bytes__symbolic size in
            (job, bytes)

        | typ ->
            FormatPlus.failwith "TODO: init_bytes_with_pointers: unhandled type: %a" Printcil.typ typ
    in
    init_bytes_with_pointers_inner job typ points_to exps
end



(** {1 Command-line options} *)

let options = [
    "--symbolic-pointers-scheme",
        Arg.Symbol (fst (List.split schemes), fun name -> default_scheme := List.assoc name schemes),
        " Set the default offset for symbolic pointers (default: "
            ^ (fst (List.find (fun (_, x) -> x = !default_scheme) schemes)) ^ ")";
]

