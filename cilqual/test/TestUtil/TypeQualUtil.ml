open MyOUnit


module Setup (QT : TypeQual.QualType.QualTypeMonad) = struct
    open QT

    module QPaths = struct
        module G = Ocamlgraph.Persistent.Digraph.Concrete (Constraints.Qual)
        include G
        include Ocamlgraph.Oper.P (G)
    end

    let path_printer ff (x, y) =
        Format.fprintf ff "%a@ <= %a" Constraints.vertex_printer x Constraints.vertex_printer y

    let path_list_printer ff list =
        ignore (List.fold_left (fun b p -> Format.fprintf ff "%(%)@[%a@]" b path_printer p; ",@ ") "" list)

    let assert_qualtype_match expected_match actual =
        assert_match ~printer:TypeQual.QualType.QualType.printer expected_match actual

    let assert_paths list graph =
        let pc = Constraints.create_path_checker graph in
        let bad = List.filter (fun (x, y) -> not (Constraints.check_path pc x y)) list in
        if bad != [] then begin
            assert_failure "@[<v2>Should be in:@ %a@]" path_list_printer bad
        end

    let assert_some_paths list graph =
        let pc = Constraints.create_path_checker graph in
        let has_path = List.exists (fun (x, y) -> (Constraints.check_path pc x y)) list in
        if not has_path then begin
            assert_failure "@[<v2>One should be in:@ %a@]" path_list_printer list
        end

    let assert_no_paths list graph =
        let pc = Constraints.create_path_checker graph in
        let bad = List.filter (fun (x, y) -> (Constraints.check_path pc x y)) list in
        if bad != [] then begin
            assert_failure "@[<v2>Should not be in:@ %a@]" path_list_printer bad
        end

    let assert_only_paths list ?(others=[]) graph =
        let pathin =
            (* add the list of expected paths *)
            let pathin = List.fold_left (fun g (x, y) -> QPaths.add_edge g x y) QPaths.empty list in
            (* add other qualifiers between which to check for paths *)
            (* all constant qualifiers *)
            let pathin = Constraints.fold_vertex begin fun x g -> match x with
                | Constraints.Qual.Const _ -> QPaths.add_vertex g x
                | _ -> g
            end graph pathin in
            (* other specified qualifiers *)
            List.fold_left QPaths.add_vertex pathin others
        in
        (* the set of unexpected paths is simply the complement of the set of expected paths *)
        let pathout = QPaths.complement pathin in

        let pc = Constraints.create_path_checker graph in

        (* check for expected paths *)
        let notin = QPaths.fold_edges begin fun x y notin ->
            if not (Constraints.Qual.equal x y) && Constraints.check_path pc x y then notin else (x, y)::notin
        end pathin [] in
        if notin != [] then assert_log "@[<v2>Should be in:@ %a@]@\n" path_list_printer notin;

        (* check for unexpected paths *)
        let notout = QPaths.fold_edges begin fun x y notout ->
            if not (Constraints.Qual.equal x y) && Constraints.check_path pc x y then (x, y)::notout else notout
        end pathout [] in
        if notout != [] then assert_log "@[<v2>Should not be in:@ %a@]@\n" path_list_printer notout;

        if notin != [] || notout != [] then assert_failure "Paths do not match"

    let assert_nb_edges expected graph =
        assert_equal
            ~printer:(fun ff -> Format.fprintf ff "%d")
            ~msg:"Wrong number of edges"
            expected (Constraints.nb_edges graph)
end

