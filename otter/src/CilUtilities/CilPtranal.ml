
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


(** Wrapper that converts a points-to function that resolves expressions to variables, to a function that resolves
    expressions to fields in variables as well, conservatively and filtered by type.
        @param points_to_varinfo is the points-to function to wrap : [Cil.exp -> Cil.varinfo list * (Cil.varinfo * string) list]
        @param exp is the expression to resolve
        @return [(targets_list, mallocs)] where [target_list] contains the points to target lvals and offset;
                and [mallocs] contains a list of malloc targets
*)
let wrap_points_to_varinfo points_to_varinfo exp =
    let targets, mallocs = points_to_varinfo exp in

    let canonicalize_type t = Cil.typeSigWithAttrs (fun _ -> []) t in

    (* allow pointers to point only to certain types *)
    let accept_type =
        let pointer_typesig = canonicalize_type (Cil.typeOf exp) in
        fun typ ->
            let rec accept_type x y = match x, y with
                | Cil.TSPtr (x, _), y when x = y -> (* pointer matches target *)
                    true
                | Cil.TSPtr (Cil.TSBase (Cil.TVoid _), _), _ -> (* void * points to anything *)
                    true
                | Cil.TSPtr (x, _), Cil.TSArray (y, _, _) when x = y -> (* pointers may point to arrays *)
                    true
                | Cil.TSPtr (x, _), Cil.TSPtr (y, _) -> (* peel off a level and recurse *)
                    accept_type x y
                (*
                | Cil.TInt _, _ -> (* allow int types to be pointers too *)
                    true
                *)
                (* TODO: what else? enums with ints? structs to sub-structs? *)
                | _, _ ->
                    false
            in
            accept_type pointer_typesig (canonicalize_type typ)
    in

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
    let target_lvals = List.fold_left begin fun target_lvals v ->
        List.fold_left begin fun target_lvals o ->
            (v, o)::target_lvals
        end target_lvals (to_offsets v.Cil.vtype)
    end [] targets in
    (target_lvals, mallocs)


(** Wrapper for Cil's {!Ptranal.resolve_exp} that resolves to fields in variables as well, conservatively and
    filtered by type.
        @param file is the file being analyzed
        @param exp is the expression to resolve
        @return [(targets_list, mallocs)] where [target_list] contains the points to target lvals and offset;
                and [mallocs] contains a list of malloc targets
*)
let points_to file exp =
    init_file file;
    wrap_points_to_varinfo Ptranal.resolve_exp exp


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
        @return [(targets_list, mallocs)] where [target_list] contains the points to target lvals and offset;
                and [mallocs] contains a list of malloc targets
*)
let naive_points_to =
    let memotable = Hashtbl.create 0 in
    fun file exp ->
        try
            Hashtbl.find memotable file
        with Not_found ->
            let malloc = FindCil.global_varinfo_by_name file "malloc" in
            let targets = wrap_points_to_varinfo (fun _ -> (FindCil.all_varinfos file, [ (malloc, "malloc") ])) exp in
            Hashtbl.add memotable file targets;
            targets


(** Unsound point-to that maps anything to just a distinct [malloc].
        @param file is the file being analyzed
        @param exp is the expression to resolve
        @return [(targets_list, mallocs)] where [target_list] is empty and [mallocs] contains a single malloc target
*)
let naive_points_to =
    let counter = Counter.make () in
    fun file exp ->
        let malloc = FindCil.global_varinfo_by_name file "malloc" in
        let name = "malloc" ^ string_of_int (Counter.next counter) in
        wrap_points_to_varinfo (fun _ -> ([], [ (malloc, name) ])) exp

