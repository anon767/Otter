open TestUtil.MyOUnit
open Control.Monad

(* setup CilQual interpreter monad stack *)
module T =
    CilQual.Type.InterpreterT
    (CilQual.CilUnionQualType.CilUnionQualTypeT (CilQual.Environment.CilFieldOrVar) (TestUtil.CilQualUtil.DummyContext)
    (Identity))
open T.QualType.Var
open T.QualType.Qual
open T.QualType
open T

module Setup1 = TestUtil.CilQualUtil.SetupType (T)
open Setup1
module Setup2 = TestUtil.TypeQualUtil.Setup (T)
open Setup2


(* test helper for types *)
let test_type typestr ?(label=typestr) test =
    label >:: begin fun () ->
        (* create and embed CilQual type *)
        let typ = create_type typestr in
        let typM = embed_rval typ in

        (* run interpreter *)
        let ((((result, constraints), _), _), _) =
            run typM (((((), QualGraph.empty), emptyContext), 0), emptyUnionTable) in

        (* print the result and constraints *)
        assert_log "@[<v>";
        assert_log "@[<v2>Result:@ %a@]@\n" QualType.printer result;
        assert_log "@[<v2>Constraints:@ %a@]@\n" QualGraph.printer constraints;
        assert_log "@]";

        (* finally run the test *)
        test result constraints
    end


(*
 * OUnit test suite
 *)

let functions_testsuite = "Functions" >::: [
    (*
     * function types
     *)
    test_type "void ($r * $s)(void)"
        begin fun result constraints ->
            assert_qualtype_match begin
                fun (Fn (Var (Fresh x1) as qvr, Base (Var (FnRet (Fresh x2))), [])) ->
                    assert_equal x1 x2;
                    assert_only_paths [ (Const "r", qvr); (qvr, Const "r") ] constraints;
            end result
        end;

    test_type "void ($r * $s)(int $a)"
        begin fun result constraints ->
            assert_qualtype_match begin
                fun (Fn (Var (Fresh x1) as qvr,
                         Base (Var (FnRet (Fresh x2))),
                         [ Base (Var (FnArg (0, Fresh x3)) as qva) ])) ->
                    assert_equal x1 x2;
                    assert_equal x1 x3;
                    assert_only_paths [ (Const "r", qvr); (qvr, Const "r");
                                        (Const "a", qva); (qva, Const "a") ] constraints;
            end result
        end;

    test_type "void ($r * $s)(int $b * $a)"
        begin fun result constraints ->
            assert_qualtype_match begin
                fun (Fn (Var (Fresh x1) as qvr,
                         Base (Var (FnRet (Fresh x2))),
                         [ Ref (Var (FnArg (0, Fresh x3)) as qva,
                                Base (Var (Deref (FnArg (0, Fresh x4))) as qvb)) ])) ->
                    assert_equal x1 x2;
                    assert_equal x1 x3;
                    assert_equal x1 x4;
                    assert_only_paths [ (Const "r", qvr); (qvr, Const "r");
                                        (Const "a", qva); (qva, Const "a");
                                        (Const "b", qvb); (qvb, Const "b") ] constraints;
            end result
        end;

    test_type "void ($r * $s)(int $c * $b * $a)"
        begin fun result constraints ->
            assert_qualtype_match begin
                fun (Fn (Var (Fresh x1) as qvr,
                         Base (Var (FnRet (Fresh x2))),
                         [ Ref (Var (FnArg (0, Fresh x3)) as qva,
                                Ref (Var (Deref (FnArg (0, Fresh x4))) as qvb,
                                     Base (Var (Deref (Deref (FnArg (0, Fresh x5)))) as qvc))) ])) ->
                    assert_equal x1 x2;
                    assert_equal x1 x3;
                    assert_equal x1 x4;
                    assert_equal x1 x5;
                    assert_only_paths [ (Const "r", qvr); (qvr, Const "r");
                                        (Const "a", qva); (qva, Const "a");
                                        (Const "b", qvb); (qvb, Const "b");
                                        (Const "c", qvc); (qvc, Const "c") ] constraints;
            end result
        end;

    test_type "void ($r * $s)(void ($t * $u)(void))"
        begin fun result constraints ->
            assert_qualtype_match begin
                fun (Fn (Var (Fresh x1) as qvr,
                         Base (Var (FnRet (Fresh x2))),
                         [ Fn (Var (FnArg (0, Fresh x3)) as qvt,
                               Base (Var (FnRet (FnArg (0, Fresh x4)))),
                               []) ])) ->
                    assert_equal x1 x2;
                    assert_equal x1 x3;
                    assert_equal x1 x4;
                    assert_only_paths [ (Const "r", qvr); (qvr, Const "r");
                                        (Const "t", qvt); (qvt, Const "t") ] constraints;
            end result
        end;

]

let testsuite = "Type" >::: [
    functions_testsuite;
]

