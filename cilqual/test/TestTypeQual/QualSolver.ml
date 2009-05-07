open TestUtil
open MyOUnit
open Control.Data
open Control.Monad
open Control.Graph

open TypeQual.Qual
open TypeQual.UnionQualType


(* setup CilQual constraint graph *)
module TQV = TypedQualVar (Unit)
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
let test_solver solver consts expM test = fun () ->
    let solver = solver consts in
    let (((((), constraints), _), _), env) = run expM (((((), QualGraph.empty), 0), emptyUnionTable), emptyEnv) in
    let solution = solver constraints in

    assert_log "@[<v>";
    assert_log "@[<v2>Constraints:@ %a@]@\n" QualGraph.printer constraints;
    assert_log "@[<v2>Environment:@ %a@]@\n" env_printer env;
    assert_log "@]";

    (* finally, run the test *)
    test env solution


(*
 * OUnit test suite
 *)

let discrete_solver_testsuite = "Discrete Order" >::: [
    "Simple" >::: [
        "($a) x = ($a) y" >:: test_solver DiscreteSolver.solve ["a"; "b"]
            begin perform
                var "x" (`Annot ("a", `Base));
                var "y" (`Annot ("a", `Base));
                "x" <== "y";
            end
            begin fun env solution ->
                assert_discrete_satisfiable solution
            end;

        "($a) x = ($b) y" >:: test_solver DiscreteSolver.solve ["a"; "b"]
            begin perform
                var "x" (`Annot ("a", `Base));
                var "y" (`Annot ("b", `Base));
                "x" <== "y";
            end
            begin fun env solution ->
                assert_discrete_unsatisfiable solution
            end;
    ];

    "Ref" >::: [
        "* ($a) x = * ($a) y" >:: test_solver DiscreteSolver.solve ["a"; "b"]
            begin perform
                var "x" (`Ref (`Annot ("a", `Base)));
                var "y" (`Ref (`Annot ("a", `Base)));
                "x" <== "y";
            end
            begin fun env solution ->
                assert_discrete_satisfiable solution
            end;

        "* ($a) x = * ($b) y" >:: test_solver DiscreteSolver.solve ["a"; "b"]
            begin perform
                var "x" (`Ref (`Annot ("a", `Base)));
                var "y" (`Ref (`Annot ("b", `Base)));
                "x" <== "y";
            end
            begin fun env solution ->
                assert_discrete_unsatisfiable solution
            end;

        "* ($a) x = t; t = * ($a) y" >:: test_solver DiscreteSolver.solve ["a"; "b"]
            begin perform
                var "x" (`Ref (`Annot ("a", `Base)));
                var "t" (`Base);
                var "y" (`Ref (`Annot ("a", `Base)));
                "x" <== "t";
                "t" <== "y";
            end
            begin fun env solution ->
                assert_discrete_satisfiable solution
            end;

        "* ($a) x = t; t = * ($b) y" >:: test_solver DiscreteSolver.solve ["a"; "b"]
            begin perform
                var "x" (`Ref (`Annot ("a", `Base)));
                var "t" (`Base);
                var "y" (`Ref (`Annot ("b", `Base)));
                "x" <== "t";
                "t" <== "y";
            end
            begin fun env solution ->
                assert_discrete_unsatisfiable solution
            end;
    ];

    "Cyclic Ref" >::: [
        "* ($a) x = t; t = u; u = v; v = w; w = t; v = * ($a) y" >:: test_solver DiscreteSolver.solve ["a"; "b"]
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
            begin fun env solution ->
                assert_discrete_satisfiable solution
            end;

        "* ($a) x = t; t = u; u = v; v = w; w = t; v = * ($b) y" >:: test_solver DiscreteSolver.solve ["a"; "b"]
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
            begin fun env solution ->
                assert_discrete_unsatisfiable solution
            end;
    ];
]

let testsuite = "QualSolver" >::: [
    discrete_solver_testsuite;
]

