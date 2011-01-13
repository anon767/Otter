
open DataStructures


(** Table to track initialized files. *)
let initialized = Hashtbl.create 0


(** Initialize a file for Cil's pointer analysis.
    @param file the Cil file to initialize
*)
let init_file file =
    (* set up to use Cil's pointer analysis *)
    if not (Hashtbl.mem initialized file) then begin
        if Hashtbl.length initialized > 0 then
            (* I don't know if Cil's pointer analysis works reliably for multiple files, e.g.:
             * - it appears to conflate some facts between files, such as in Cil.Ptranal.hose_global;
             * - it has several global hashtables keyed on Cil.varinfo, Cil.exp, Cil.lval, etc.
             *)
            Format.eprintf "Warning: Cil's pointer analysis may not work reliably for multiple files@\n";
        Ptranal.analyze_file file;
        Ptranal.compute_results false;
        Hashtbl.add initialized file true
    end


(** Fold helper for Cil.TComp. *)
let fold_struct f acc compinfo =
    List.fold_left f acc compinfo.Cil.cfields


(** Fold helper for Cil.TArray. *)
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


(** Helper that tests if a pointer can point to a target.
        @param pointer_type is the pointer type
        @param target_type is the target type
        @return whether the pointer can point to the target
*)
let accept_points_to pointer_type =
    (* allow pointers to point only to certain types *)
    let canonicalize_type t = Cil.typeSigWithAttrs (fun _ -> []) t in
    let pointer_typesig = canonicalize_type pointer_type in
    fun target_type ->
        let rec accept_points_to x y = match x, y with
            | Cil.TSPtr (x, _), y when x = y -> (* pointer matches target *)
                true
            | Cil.TSPtr (Cil.TSBase (Cil.TVoid _), _), _ -> (* void * points to anything *)
                true
            | Cil.TSPtr (x, _), Cil.TSArray (y, _, _) when x = y -> (* pointers may point to arrays *)
                true
            | Cil.TSPtr (x, _), Cil.TSPtr (y, _) -> (* peel off a level and recurse *)
                accept_points_to x y
            (*
            | Cil.TInt _, _ -> (* allow int types to be pointers too *)
                true
            *)
            (* TODO: what else? enums with ints? structs to sub-structs? *)
            | _, _ ->
                false
        in
        accept_points_to pointer_typesig (canonicalize_type target_type)


(** Wrapper that converts a points-to function that resolves expressions to variables, to a function that resolves
    expressions to fields in variables as well, conservatively and filtered by type.
        @param points_to_varinfo is the points-to function to wrap
        @param exp is the expression to resolve
        @return [(target_varinfos, target_mallocs)] where [target_varinfos] contains the points to target varinfos
                and offsets; and [target_mallocs] contains a list of dynamic allocation sites and offsets
*)
let wrap_points_to_varinfo points_to_varinfo exp =
    let varinfos, mallocs = points_to_varinfo exp in

    let pointer_type = Cil.unrollType (Cil.typeOf exp) in
    let accept_type = accept_points_to pointer_type in

    (* enumerate all field/array offsets that matches the target type *)
    let to_offsets typ =
        let rec to_offsets offsets typ base =
            let offset_type = Cil.unrollType (Cil.typeOffset typ base) in
            (* collect offsets that matches the target type *)
            let offsets = if accept_type offset_type then
                base::offsets
            else
                offsets
            in
            (* recurse over all field, offsets *)
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
                        | None -> offsets
                    end
                | _ ->
                    offsets
        in
        to_offsets [] (Cil.unrollType typ) Cil.NoOffset
    in

    (* combine the target varinfos with offsets *)
    let target_varinfos = List.fold_left begin fun target_varinfos varinfo ->
        List.fold_left begin fun target_varinfos offset ->
            (varinfo, offset)::target_varinfos
        end target_varinfos (to_offsets varinfo.Cil.vtype)
    end [] varinfos in

    (* combine the target varinfos with offsets *)
    let target_mallocs = List.fold_left begin fun target_mallocs (_, _, typ as malloc) ->
        List.fold_left begin fun target_mallocs offset ->
            (malloc, offset)::target_mallocs
        end target_mallocs (to_offsets typ)
    end [] mallocs in

    (target_varinfos, target_mallocs)


(** Helper that generates a map from untyped dynamic allocation sites ([Cil.fundec * string] tuple) to typed dynamic
    allocation sites ([CilData.Malloc.t]), using a points-to analysis to infer the types from pointers that point to
    those sites.
        @param file is the file for which to generate the mapping
        @param points_to_varinfo is the points-to function which is used to infer the types
        @return [malloc_map] which is a function that maps a list of untyped dynamic allocation sites to typed dynamic
                allocation sites
*)
let add_malloc_types =
    let file_memotable = Hashtbl.create 0 in
    fun file (points_to_varinfo : _ -> Cil.varinfo list * _) ->
        let malloc_memotable =
            try
                Hashtbl.find file_memotable file
            with Not_found ->
                (* build a map from malloc sites to malloc types *)
                let module MallocSet = Set.Make (CilData.Malloc) in
                let module MallocMap = Map.Make (CilData.Malloc) in
                let module LhostMallocSet = Set.Make (struct
                    type t = CilData.CilLhost.t * CilData.Malloc.t
                    let compare (xl, xm) (yl, ym) =
                        match CilData.CilLhost.compare xl yl with
                            | 0 -> CilData.Malloc.compare xm ym
                            | i -> i
                end) in

                let site_to_mallocs = Hashtbl.create 0 in

                (* for each varinfo or malloc pointer given as an lhost, find all malloc targets and infer their types
                 * from the pointer lhost type *)
                let process_lhost updated lhost source_opt =
                    let lhost_type = Cil.typeOf (Cil.Lval (lhost, Cil.NoOffset)) in

                    (* process each offset in the lhost that is a pointer *)
                    let process_offset_points_to updated offset target_type =
                        let pointer = Cil.Lval (lhost, offset) in
                        let _, malloc_sites = points_to_varinfo pointer in

                        List.fold_left begin fun updated (mallocfn, name as site) ->
                            let malloc = (mallocfn, name, target_type) in
                            let malloc_types = try Hashtbl.find site_to_mallocs site with Not_found -> MallocMap.empty in
                            let malloc_sources = try MallocMap.find malloc malloc_types with Not_found -> MallocSet.empty in
                            let updated, malloc_sources = begin match source_opt with
                                | Some source when not (MallocSet.mem source malloc_sources) ->
                                    (LhostMallocSet.add (Cil.Mem pointer, malloc) updated, MallocSet.add source malloc_sources)
                                | Some _ ->
                                    (updated, malloc_sources)
                                | None ->
                                    (LhostMallocSet.add (Cil.Mem pointer, malloc) updated, malloc_sources)
                            end in
                            let malloc_types = MallocMap.add malloc malloc_sources malloc_types in
                            Hashtbl.replace site_to_mallocs site malloc_types;
                            updated
                        end updated malloc_sites
                    in

                    (* find all offsets in the lhost that is a pointer *)
                    let rec fold_pointer_offsets updated offset = match Cil.typeOffset lhost_type offset with
                        | Cil.TPtr (target_type, _) ->
                            process_offset_points_to updated offset target_type
                        | Cil.TComp (compinfo, _) ->
                            fold_struct begin fun updated field ->
                                fold_pointer_offsets updated (Cil.addOffset (Cil.Field (field, Cil.NoOffset)) offset)
                            end updated compinfo
                        | Cil.TArray (target_type, len_opt, _) ->
                            let updated =
                                if len_opt = None then
                                    process_offset_points_to updated offset target_type
                                else
                                    updated
                            in
                            fold_pointer_offsets updated (Cil.addOffset (Cil.Index (Cil.zero, Cil.NoOffset)) offset)
                        | _ ->
                            updated
                    in
                    fold_pointer_offsets updated Cil.NoOffset
                in

                let updated = LhostMallocSet.empty in

                (* first infer malloc types from varinfo pointers *)
                let updated = List.fold_left begin fun updated varinfo ->
                    process_lhost updated (Cil.Var varinfo) None
                end updated (FindCil.all_varinfos file) in

                (* then infer malloc types from malloc pointers, iteratively to a fixpoint *)
                let rec do_fixpoint updated =
                    if not (LhostMallocSet.is_empty updated) then begin
                        let updated = LhostMallocSet.fold begin fun (lhost, source) updated ->
                            process_lhost updated lhost (Some source)
                        end updated LhostMallocSet.empty in
                        do_fixpoint updated
                    end
                in
                do_fixpoint updated;

                (* finally, build the map from malloc sites to malloc types *)
                let malloc_memotable = Hashtbl.create (Hashtbl.length site_to_mallocs) in
                Hashtbl.iter begin fun malloc malloc_types ->
                    let malloc_types = MallocMap.fold (fun malloc _ malloc_types -> malloc::malloc_types) malloc_types [] in
                    Hashtbl.replace malloc_memotable malloc malloc_types
                end site_to_mallocs;

                Hashtbl.replace file_memotable file malloc_memotable;
                malloc_memotable
        in
        fun mallocs ->
            List.concat (List.map (Hashtbl.find malloc_memotable) mallocs)


(** Wrapper for Cil's {!Ptranal.resolve_exp} that resolves to fields in variables as well, conservatively and
    filtered by type.
        @param file is the file being analyzed
        @param exp is the expression to resolve
        @return [(target_varinfos, target_mallocs)] where [target_varinfos] contains the points to target varinfos
                and offsets; and [target_mallocs] contains a list of dynamic allocation sites and offsets
*)
let points_to file exp =
    init_file file;
    let add_malloc_types = add_malloc_types file Ptranal.resolve_exp in
    let points_to exp =
        let varinfos, sites = Ptranal.resolve_exp exp in
        (varinfos, add_malloc_types sites)
    in
    wrap_points_to_varinfo points_to exp


(** Wrapper for Cil's {!Ptranal.resolve_fundec}.
        @param file is the file being analyzed
        @param exp is the expression to resolve
        @return [fundec_list] which is a list of target functions
*)
let points_to_fundec file exp =
    init_file file;
    Ptranal.resolve_funptr exp


(** Naive point-to that maps anything to everything (including [malloc]), filtered by type.
        @param file is the file being analyzed
        @param exp is the expression to resolve
        @return [(targets_list, target_mallocs)] where [target_list] contains the points to target lvals and offset;
                and [target_mallocs] contains a list of dynamic allocation sites and offsets
*)
let naive_points_to =
    let memotable = Hashtbl.create 0 in
    fun file exp ->
        try
            Hashtbl.find memotable file
        with Not_found ->
            let malloc = FindCil.global_varinfo_by_name file "malloc" in
            let varinfos = FindCil.all_varinfos file in
            let mallocs = List.map (fun typ -> (malloc, "malloc", typ)) (FindCil.all_types file) in
            let targets = wrap_points_to_varinfo (fun _ -> (varinfos, mallocs)) exp in
            Hashtbl.add memotable file targets;
            targets


(** Unsound point-to that maps anything to just a distinct [malloc].
        @param file is the file being analyzed
        @param exp is the expression to resolve
        @return [(targets_list, target_mallocs)] where [target_list] is empty and [target_mallocs] contains a single
                dynamic allocation site
*)
let unsound_points_to =
    let counter = Counter.make () in
    fun file exp ->
        match Cil.unrollType (Cil.typeOf exp) with
            | Cil.TPtr (typ, _) ->
                let malloc = FindCil.global_varinfo_by_name file "malloc" in
                let name = "malloc" ^ string_of_int (Counter.next counter) in
                wrap_points_to_varinfo (fun _ -> ([], [ (malloc, name, typ) ])) exp
            | _ ->
                ([], [])

