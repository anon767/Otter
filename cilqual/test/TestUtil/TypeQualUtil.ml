open MyOUnit
open Control.Monad


module Setup (QT : TypeQual.UnionQualType.UnionQualTypeMonad) = struct
    include QT
    module Ops = MonadOps (QT)
    include Ops

    module QualTypeOps = MonadOps (QualType)

    module QPaths = struct
        module G = Ocamlgraph.Persistent.Digraph.Concrete (QualGraph.Qual)
        include G
        include Ocamlgraph.Oper.P (G)
    end


    (*
     * solvers
     *)

    module SourceSink = TypeQual.QualSolver.Reachability.SourceSink (QualGraph)
    module DiscreteSolver = TypeQual.QualSolver.DiscreteOrder (QualGraph)


    (*
     * printers
     *)

    let path_printer ff (x, y) =
        Format.fprintf ff "%a@ <= %a" QualGraph.vertex_printer x QualGraph.vertex_printer y

    let path_list_printer ff list =
        ignore (List.fold_left (fun b p -> Format.fprintf ff "%(%)@[%a@]" b path_printer p; ",@ ") "" list)


    (*
     * helpers
     *)

    let permute list =
        if List.length list > 4 then invalid_arg "permutation limited to 4 items";
        (* from: http://caml.inria.fr/pub/ml-archives/caml-list/2001/06/d4059d1cf784e6eeff6978245ffcb319.fr.html *)
        let rec distribute elt = function
            | (hd::tl) as list -> (elt::list)::(List.map (fun x -> hd::x) (distribute elt tl))
            | [] -> [ [elt] ]
        and permute = function
            | x::rest -> List.flatten (List.map (distribute x) (permute rest))
            | [] -> [ [] ]
        in
        permute list


    (*
     * assertions
     *)

    let assert_qualtype_match expected_match actual =
        assert_match ~printer:QT.QualType.printer expected_match actual

    let assert_paths list graph =
        let pc = SourceSink.create graph in
        let bad = List.filter (fun (x, y) -> not (SourceSink.check pc x y)) list in
        if bad != [] then begin
            assert_failure "@[<v2>Should be in:@ %a@]" path_list_printer bad
        end

    let assert_some_paths list graph =
        let pc = SourceSink.create graph in
        let has_path = List.exists (fun (x, y) -> (SourceSink.check pc x y)) list in
        if not has_path then begin
            assert_failure "@[<v2>One should be in:@ %a@]" path_list_printer list
        end

    let assert_no_paths list graph =
        let pc = SourceSink.create graph in
        let bad = List.filter (fun (x, y) -> (SourceSink.check pc x y)) list in
        if bad != [] then begin
            assert_failure "@[<v2>Should not be in:@ %a@]" path_list_printer bad
        end

    let assert_only_paths list ?(others=[]) graph =
        let pathin =
            (* add the list of expected paths *)
            let pathin = List.fold_left (fun g (x, y) -> QPaths.add_edge g x y) QPaths.empty list in
            (* add other qualifiers between which to check for paths *)
            (* all constant qualifiers *)
            let pathin = QualGraph.fold_vertex begin fun x g -> match x with
                | QualGraph.Qual.Const _ -> QPaths.add_vertex g x
                | _ -> g
            end graph pathin in
            (* other specified qualifiers *)
            List.fold_left QPaths.add_vertex pathin others
        in
        (* the set of unexpected paths is simply the complement of the set of expected paths *)
        let pathout = QPaths.complement pathin in

        let pc = SourceSink.create graph in

        (* check for expected paths *)
        let notin = QPaths.fold_edges begin fun x y notin ->
            if not (QualGraph.Qual.equal x y) && SourceSink.check pc x y then notin else (x, y)::notin
        end pathin [] in
        if notin != [] then assert_log "@[<v2>Should be in:@ %a@]@\n" path_list_printer notin;

        (* check for unexpected paths *)
        let notout = QPaths.fold_edges begin fun x y notout ->
            if not (QualGraph.Qual.equal x y) && SourceSink.check pc x y then (x, y)::notout else notout
        end pathout [] in
        if notout != [] then assert_log "@[<v2>Should not be in:@ %a@]@\n" path_list_printer notout;

        if notin != [] || notout != [] then assert_failure "Paths do not match"

    let assert_nb_edges expected graph =
        assert_equal
            ~printer:(fun ff -> Format.fprintf ff "%d")
            ~msg:"Wrong number of edges"
            expected (QualGraph.nb_edges graph)

    let assert_discrete_satisfiable solution =
        if DiscreteSolver.Solution.is_unsatisfiable solution then
            assert_failure "@[<v2>Should be satisfiable:@\n%a@]" DiscreteSolver.Solution.printer solution

    let assert_discrete_unsatisfiable solution =
        if not (DiscreteSolver.Solution.is_unsatisfiable solution) then
            assert_failure "@[<v2>Should be unsatisfiable:@\n%a@]" DiscreteSolver.Solution.printer solution


    (*
     * QualTypeT monad programming
     *)

    module Programming = struct
        module ProgrammingQualTypeM = EnvT (String) (QualType) (QT)
        include ProgrammingQualTypeM
        module Ops = MonadOps (ProgrammingQualTypeM)
        open Ops

        let emptyUnionTable = QT.UnionTable.empty
        let emptyEnv = empty

        (* lift monad operations *)
        let assign x y = lift (QT.assign x y)
        let join x y = lift (QT.join x y)
        let meet x y = lift (QT.meet x y)
        let fresh qt = lift (QT.fresh qt)
        let annot qt clist = lift (QT.annot qt clist)
        let deref qt = lift (QT.deref qt)
        let app qtf qta = lift (QT.app qtf qta)
        let retval qtf = lift (QT.retval qtf)
        let args qtf = lift (QT.args qtf)

        (* printers *)
        let env_printer ff env = ignore begin
            let kvprinter ff (k, v) = Format.fprintf ff "@[\"%s\"@]@ => @[%a@]" k QualType.printer v in
            Env.fold (fun k v b -> Format.fprintf ff "%(%)@[%a@]" b kvprinter (k, v); "@\n") env ""
        end

        (* operations *)
        let var x t = perform
            let rec embed_t = function
                | `Ref v -> perform with module QualType in
                    qt <-- embed_t v;
                    QualType.ref qt
                | `Fun (r, a) -> perform with module QualType in
                    qtr <-- embed_t r;
                    qta <-- QualTypeOps.mapM embed_t a;
                    QualType.fn qtr qta
                | `Base -> perform with module QualType in
                    QualType.base
                | `Annot (_, v) -> perform with module QualType in
                    embed_t v
            in
            let rec annot_qt qt = function
                | `Ref v -> perform
                    qt' <-- deref qt;
                    annot_qt qt' v
                | `Fun (r, a) -> perform
                    qtr <-- retval qt;
                    annot_qt qtr r;
                    qta <-- args qt;
                    zipWithM_ annot_qt qta a
                | `Base ->
                    return ()
                | `Annot (c, v) -> perform
                    annot qt c;
                    annot_qt qt v
            in
            qt <-- fresh (embed_t t);
            annot_qt qt t;
            update x qt

        let (<==) x y = perform
            vx <-- lookup x;
            vy <-- lookup y;
            match vx, vy with
                | Some vx, Some vy -> assign vx vy
                | None _, None _ -> assert_failure "\"%s\" and \"%s\" have not been declared" x y
                | None _, _ -> assert_failure "\"%s\" has not been declared" x
                | _, None _ -> assert_failure "\"%s\" has not been declared" y
    end
end

