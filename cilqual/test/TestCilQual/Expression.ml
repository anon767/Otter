open TestUtil.MyOUnit
open Control.Monad

open TypeQual.QualType
open TypeQual.QualType.QualType
open TypeQual.QualType.QualTypeConstraints.Qual

module E =
    CilQual.Expression.InterpreterT
        (CilQual.Environment.InterpreterT
            (CilQual.Type.InterpreterT
                (CilQual.Config.InterpreterT
                    (TypeQual.QualType.QualTypeT (Identity)))))
open E

module Setup1 = TestUtil.CilQualUtil.Setup (E)
open Setup1
module Setup2 = TestUtil.TypeQualUtil.Setup (E)
open Setup2


(* test helper for expressions *)
let test_exp exp ?(label=exp) ?(typedecls=[]) vardecls test =
    label >:: begin fun () ->
        let cilenv, formatcilenv = create_env typedecls vardecls in

        (* Cil expression to interpret *)
        let exp = Formatcil.cExp (preprocess_cilqual exp) formatcilenv in

        assert_log "@[<v>";
        (* show Cil environment and expression structure *)
        assert_log "@[<v2>Environment:@ %a@]@\n" (env_printer Cil.plainCilPrinter) cilenv;
        assert_log "@[<v2>Expression:@ %a@]@\n" (adapt_cil_printer Cil.plainCilPrinter#pExp) exp;

        (* CilQual interpreter for expression *)
        let expM = interpret_exp exp in

        (* run interpreter *)
        let (((result, env), constraints), _) = run expM emptyEnv Constraints.empty 0 in

        (* print the result, environment and constraints *)
        assert_log "@[<v2>Result:@ %a@]@\n" QualType.printer result;
        assert_log "@[<v2>Environment:@ %a@]@\n" cilqual_env_printer env;
        assert_log "@[<v2>Constraints:@ %a@]@\n" Constraints.printer constraints;
        assert_log "@]";

        (* finally run the test *)
        test (cilenv, env) result constraints
    end


(*
 * OUnit test suite
 *)

let constants_testsuite = "Constants" >::: [
    (*
     * expressions without variables
     *)
    test_exp "1"
        []
        begin fun env result constraints ->
            assert_qualtype_match (fun Empty -> ()) result
        end;

    test_exp "(int $a) 1"
        []
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "a", qv); (qv, Const "a"); ] constraints
            end result
        end;

    test_exp "(int $b * $a) 1"
        []
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Ref (Var _ as qva, Base (Var _ as qvb))) ->
                assert_only_paths [ (Const "a", qva); (qva, Const "a"); (Const "b", qvb); (qvb, Const "b"); ] constraints
            end result
        end;

    test_exp "1 + 2"
        []
        begin fun env result constraints ->
            assert_qualtype_match (fun Empty -> ()) result
        end;

    test_exp "1 + (int $a) 2"
        []
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "a", qv); (qv, Const "a") ] constraints
            end result
        end;

    test_exp "(int $a) 1 + (int $b) 2"
        []
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "a", qv); (Const "b", qv) ] constraints
            end result
        end;

    test_exp "(int $a) 1 + (int $b) 2 + (int $c) 3"
        []
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "a", qv); (Const "b", qv); (Const "c", qv) ] constraints
            end result
        end;
]

let variables_testsuite = "Variables" >::: [
    (*
     * expressions with variables
     *)
    test_exp "x"
        [ ("x", "int $a") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "a", qv); (qv, Const "a") ] constraints
            end result
        end;

    test_exp "&x"
        [ ("x", "int $a") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Ref (Var _, (Base (Var _ as qv)))) ->
                assert_only_paths [ (Const "a", qv); (qv, Const "a") ] constraints
            end result
        end;

    test_exp "x + y"
        [ ("x", "int $xa"); ("y", "int $ya") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "xa", qv); (Const "ya", qv) ] constraints
            end result
        end;
]

let pointers_testsuite = "Pointers" >::: [
    (*
     * expressions with pointers
     *)
    test_exp "x_ptr"
        [ ("x_ptr", "int $b * $a") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Ref (Var _ as qva, (Base (Var _ as qvb)))) ->
                assert_only_paths [ (Const "a", qva); (qva, Const "a"); (Const "b", qvb); (qvb, Const "b") ] constraints
            end result
        end;

    test_exp "&x_ptr"
        [ ("x_ptr", "int $b * $a") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Ref (Var _, Ref (Var _ as qva, (Base (Var _ as qvb))))) ->
                assert_only_paths [ (Const "a", qva); (qva, Const "a"); (Const "b", qvb); (qvb, Const "b") ] constraints
            end result
        end;

    test_exp "*x_ptr"
        [ ("x_ptr", "int $b * $a") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qvb)) ->
                assert_only_paths [ (Const "b", qvb); (qvb, Const "b") ] constraints
            end result
        end;

    test_exp "*x_ptr + *y_ptr"
        [ ("x_ptr", "int $xb * $xa"); ("y_ptr", "int $yb * $ya") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "xb", qv); (Const "yb", qv) ] constraints
            end result
        end;

    test_exp "x_ptr + 1"
        [ ("x_ptr", "int $b * $a") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Ref (Var _ as qva, (Base (Var _ as qvb)))) ->
                assert_only_paths [ (Const "a", qva); (qva, Const "a"); (Const "b", qvb); (qvb, Const "b") ] constraints
            end result
        end;
]

let arrays_testsuite = "Arrays" >::: [
    (*
     * expressions with arrays
     *)
    test_exp "xs"
        [ ("xs", "int $a []") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qva)) ->
                assert_only_paths [ (Const "a", qva); (qva, Const "a") ] constraints
            end result
        end;

    test_exp "xs[0]"
        [ ("xs", "int $a []") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qva)) ->
                assert_only_paths [ (Const "a", qva); (qva, Const "a") ] constraints
            end result
        end;

    test_exp "xs + 1"
        [ ("xs", "int $a []") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qva)) ->
                assert_only_paths [ (Const "a", qva); (qva, Const "a") ] constraints
            end result
        end;

    test_exp "&xs" (* pointer to array *)
        [ ("xs", "int $a []") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Ref (Var _, (Base (Var _ as qva)))) ->
                assert_only_paths [ (Const "a", qva); (qva, Const "a") ] constraints
            end result
        end;

    test_exp "&xs[0]"
        [ ("xs", "int $a []") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Ref (Var _, (Base (Var _ as qva)))) ->
                assert_only_paths [ (Const "a", qva); (qva, Const "a") ] constraints
            end result
        end;

    test_exp "xss"
        [ ("xss", "int $a [][]") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qva)) ->
                assert_only_paths [ (Const "a", qva); (qva, Const "a") ] constraints
            end result
        end;

    test_exp "&xss"
        [ ("xss", "int $a [][]") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Ref (Var _, (Base (Var _ as qva)))) ->
                assert_only_paths [ (Const "a", qva); (qva, Const "a") ] constraints
            end result
        end;

    test_exp "&xss[0]"
        [ ("xss", "int $a [][]") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Ref (Var _, (Base (Var _ as qva)))) ->
                assert_only_paths [ (Const "a", qva); (qva, Const "a") ] constraints
            end result
        end;

    test_exp "&xss[0][0]"
        [ ("xss", "int $a [][]") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Ref (Var _, (Base (Var _ as qva)))) ->
                assert_only_paths [ (Const "a", qva); (qva, Const "a") ] constraints
            end result
        end;

    test_exp "xps"
        [ ("xps", "int $a (* $b)[]") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Ref (Var _ as qvb, (Base (Var _ as qva)))) ->
                assert_only_paths [ (Const "a", qva); (qva, Const "a"); (Const "b", qvb); (qvb, Const "b") ] constraints
            end result
        end;
]

let structs_testsuite = "Structs" >::: [
    (*
     * expressions with structs and fields
     *)
    test_exp "c"
        ~typedecls:[ `Struct ("c_t", [ ("f1", "int $a"); ("f2", "int $b") ]) ]
        [ ("c", "struct %c:c_t $a") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "a", qv); (qv, Const "a") ] constraints
            end result
        end;

    test_exp "c.f1"
        ~typedecls:[ `Struct ("c_t", [ ("f1", "int $a"); ("f2", "int $b") ]) ]
        [ ("c", "struct %c:c_t") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "a", qv); (qv, Const "a") ] constraints
            end result
        end;

    test_exp "c.f2"
        ~typedecls:[ `Struct ("c_t", [ ("f1", "int $a"); ("f2", "int $b") ]) ]
        [ ("c", "struct %c:c_t") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "b", qv); (qv, Const "b") ] constraints
            end result
        end;

    test_exp "c->f1"
        ~typedecls:[ `Struct ("c_t", [ ("f1", "int $a"); ("f2", "int $b") ]) ]
        [ ("c", "struct %c:c_t *") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "a", qv); (qv, Const "a") ] constraints
            end result
        end;

    test_exp "c->f2"
        ~typedecls:[ `Struct ("c_t", [ ("f1", "int $a"); ("f2", "int $b") ]) ]
        [ ("c", "struct %c:c_t *") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "b", qv); (qv, Const "b") ] constraints
            end result
        end;

    test_exp "c[1].f1"
        ~typedecls:[ `Struct ("c_t", [ ("f1", "int $a"); ("f2", "int $b") ]) ]
        [ ("c", "struct %c:c_t [2]") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "a", qv); (qv, Const "a") ] constraints
            end result
        end;

    test_exp "c.f2->f1"
        ~typedecls:[ `Struct ("c_t", [ ("f1", "int $a"); ("f2", "struct %c:c_t $b * $c") ]) ]
        [ ("c", "struct %c:c_t") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "a", qv); (qv, Const "a") ] constraints
            end result
        end;

    test_exp "c.f2->f2->f2"
        ~typedecls:[ `Struct ("c_t", [ ("f1", "int $a"); ("f2", "struct %c:c_t $b * $c") ]) ]
        [ ("c", "struct %c:c_t") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Ref (Var _ as qvc, Base (Var _ as qvb))) ->
                assert_only_paths [ (Const "b", qvb); (qvb, Const "b"); (Const "c", qvc); (qvc, Const "c") ] constraints
            end result
        end;

    test_exp "d.g1.f1"
        ~typedecls:[ `Struct ("c_t", [ ("f1", "int $a") ]);
                     `Struct ("d_t", [ ("g1", "struct %c:c_t $b") ]) ]
        [ ("d", "struct %c:d_t") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "a", qv); (qv, Const "a"); ] constraints
            end result
        end;
]

let functions_testsuite = "Functions" >::: [
    (*
     * expressions with function types
     *
     * Note: Formatcil does not exactly have the same syntax as C, so the & operator is required for functions
     *)
    test_exp "&void_fn_empty"
        [ ("void_fn_empty", "void ()()") ] (* void void_fn_empty() *)
        begin fun env result constraints ->
            assert_qualtype_match (fun (Fn (Var _, (Base (Var _)), [])) -> ()) result
        end;

    test_exp "&void_fn_void"
        [ ("void_fn_void", "void ()(void)") ] (* void void_fn_void(void) *)
        begin fun env result constraints ->
            assert_qualtype_match (fun (Fn (Var _, (Base (Var _)), [])) -> ()) result
        end;

    test_exp "&int_fn_int"
        [ ("int_fn_int", "int $a ($b)(int $c)") ] (* int $a int_fn_int(int $c) $b *)
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Fn (Var _ as qvb, (Base (Var _ as qva)), [ (Base (Var _ as qvc)) ])) ->
                assert_only_paths
                    [ (Const "a", qva); (Const "b", qvb); (Const "c", qvc);
                      (qva, Const "a"); (qvb, Const "b"); (qvc, Const "c") ] constraints
            end result
        end;

    test_exp "int_fn_ptr_int"
        [ ("int_fn_ptr_int", "int $a ($b * $c)(int $d)") ] (* int $a ($b * $c int_fn_int)(int $d) *)
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Fn (Var _ as qvb, (Base (Var _ as qva)), [ (Base (Var _ as qvd)) ])) ->
                assert_only_paths
                    [ (Const "a", qva); (Const "b", qvb); (Const "d", qvd);
                      (qva, Const "a"); (qvb, Const "b"); (qvd, Const "d") ] constraints
            end result
        end;
]

let voidptr_testsuite = "void *" >::: [
    (*
     * expressions with void *
     *)
    test_exp "void_ptr"
        [ ("void_ptr", "void $b * $a") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Ref (Var _ as qva, (Base (Var _ as qvb)))) ->
                assert_only_paths [ (Const "a", qva); (qva, Const "a"); (Const "b", qvb); (qvb, Const "b") ] constraints
            end result
        end;

    test_exp "*void_ptr"
        [ ("void_ptr", "void $b * $a") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qvb)) ->
                assert_only_paths [ (Const "b", qvb); (qvb, Const "b") ] constraints
            end result
        end;
]

let testsuite = "Expression" >::: [
    constants_testsuite;
    variables_testsuite;
    pointers_testsuite;
    arrays_testsuite;
    structs_testsuite;
    functions_testsuite;
    voidptr_testsuite;
]

