open TestUtil
open MyOUnit
open Control.Data
open Control.Monad
open Control.Graph

open TypeQual.Qual
open TypeQual.UnionQualType


(* setup CilQual constraint graph *)
module TQV = TypedQualVar (String)
module QT = Qual (TQV) (String)
module QC = Constraint (struct include Unit let default = () let printer ff () = () end)

(* setup CilQual monad stack *)
module GraphM = BidirectionalGraphT (QT) (QC) (Identity)
module QualGraphM = QualT (QT) (QC) (GraphM)
module UnionQualTypeQualGraphM = UnionQualTypeT (TQV) (QualGraphM)

module Setup = TypeQualUtil.Setup (UnionQualTypeQualGraphM)
open Setup
open Setup.Programming


(* test helper *)
let test_discrete_solver consts expM test = fun () ->
    let (((((), constraints), _), _), env) = run expM (((((), QualGraph.empty), 0), emptyUnionTable), emptyEnv) in
    let solution = DiscreteSolver.solve consts constraints in
    let explanation = if DiscreteSolver.Solution.is_unsatisfiable solution then
        DiscreteSolver.explain solution
    else
        DiscreteSolver.Explanation.empty
    in

    assert_log "@[<v>";
    assert_log "@[<v2>Constraints:@ %a@]@\n" QualGraph.printer constraints;
    assert_log "@[<v2>Environment:@ %a@]@\n" env_printer env;
    assert_log "@]";

    (* finally, run the test *)
    test env solution explanation


(*
 * OUnit test suite
 *)

let discrete_solver_testsuite = "Discrete Order" >::: [
    "Simple" >::: [
        "($a) x = ($a) y" >:: test_discrete_solver ["a"; "b"]
            begin perform
                var "x" (`Annot ("a", `Base));
                var "y" (`Annot ("a", `Base));
                "x" <== "y";
            end
            begin fun env solution explanation ->
                assert_discrete_satisfiable solution explanation
            end;

        "($a) x = ($b) y" >:: test_discrete_solver ["a"; "b"]
            begin perform
                var "x" (`Annot ("a", `Base));
                var "y" (`Annot ("b", `Base));
                "x" <== "y";
            end
            begin fun env solution explanation ->
                assert_discrete_unsatisfiable solution explanation
            end;
    ];

    "Multiple" >::: [
        "($a) x = ($a) y; x = ($b) r" >:: test_discrete_solver ["a"; "b"]
            begin perform
                var "x" (`Annot ("a", `Base));
                var "y" (`Annot ("a", `Base));
                var "r" (`Annot ("b", `Base));
                "x" <== "y";
                "x" <== "r";
            end
            begin fun env solution explanation ->
                assert_discrete_unsatisfiable solution explanation
            end;

        "($a) x = ($a) y; ($b) r = ($b) s; x = r" >:: test_discrete_solver ["a"; "b"]
            begin perform
                var "x" (`Annot ("a", `Base));
                var "y" (`Annot ("a", `Base));
                var "r" (`Annot ("b", `Base));
                var "s" (`Annot ("b", `Base));
                "x" <== "y";
                "r" <== "s";
                "x" <== "r";
            end
            begin fun env solution explanation ->
                assert_discrete_unsatisfiable solution explanation
            end;
    ];

    "Ref" >::: [
        "* ($a) x = * ($a) y" >:: test_discrete_solver ["a"; "b"]
            begin perform
                var "x" (`Ref (`Annot ("a", `Base)));
                var "y" (`Ref (`Annot ("a", `Base)));
                "x" <== "y";
            end
            begin fun env solution explanation ->
                assert_discrete_satisfiable solution explanation
            end;

        "* ($a) x = * ($b) y" >:: test_discrete_solver ["a"; "b"]
            begin perform
                var "x" (`Ref (`Annot ("a", `Base)));
                var "y" (`Ref (`Annot ("b", `Base)));
                "x" <== "y";
            end
            begin fun env solution explanation ->
                assert_discrete_unsatisfiable solution explanation
            end;

        "* ($a) x = t; t = * ($a) y" >:: test_discrete_solver ["a"; "b"]
            begin perform
                var "x" (`Ref (`Annot ("a", `Base)));
                var "t" (`Base);
                var "y" (`Ref (`Annot ("a", `Base)));
                "x" <== "t";
                "t" <== "y";
            end
            begin fun env solution explanation ->
                assert_discrete_satisfiable solution explanation
            end;

        "* ($a) x = t; t = * ($b) y" >:: test_discrete_solver ["a"; "b"]
            begin perform
                var "x" (`Ref (`Annot ("a", `Base)));
                var "t" (`Base);
                var "y" (`Ref (`Annot ("b", `Base)));
                "x" <== "t";
                "t" <== "y";
            end
            begin fun env solution explanation ->
                assert_discrete_unsatisfiable solution explanation
            end;
    ];

    "Cyclic Ref" >::: [
        "* ($a) x = t; t = u; u = v; v = w; w = t; v = * ($a) y" >:: test_discrete_solver ["a"; "b"]
            begin perform
                var "x" (`Ref (`Annot ("a", `Base)));
                var "t" (`Base);
                var "u" (`Base);
                var "v" (`Base);
                var "w" (`Base);
                var "y" (`Ref (`Annot ("a", `Base)));
                "x" <== "t";
                "t" <== "u";
                "u" <== "v";
                "v" <== "w";
                "w" <== "t";
                "v" <== "y";
            end
            begin fun env solution explanation ->
                assert_discrete_satisfiable solution explanation
            end;

        "* ($a) x = t; t = u; u = v; v = w; w = t; v = * ($b) y" >:: test_discrete_solver ["a"; "b"]
            begin perform
                var "x" (`Ref (`Annot ("a", `Base)));
                var "t" (`Base);
                var "u" (`Base);
                var "v" (`Base);
                var "w" (`Base);
                var "y" (`Ref (`Annot ("b", `Base)));
                "x" <== "t";
                "t" <== "u";
                "u" <== "v";
                "v" <== "w";
                "w" <== "t";
                "v" <== "y";
            end
            begin fun env solution explanation ->
                assert_discrete_unsatisfiable solution explanation
            end;
    ];
]

let testsuite = "QualSolver" >::: [
    discrete_solver_testsuite;
]

