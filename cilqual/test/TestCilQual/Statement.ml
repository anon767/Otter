open TestUtil.MyOUnit
open Control.Monad

(* setup CilQual interpreter monad stack *)
module S =
    CilQual.Statement.InterpreterT
    (CilQual.Instruction.InterpreterT
    (CilQual.Expression.InterpreterT
    (CilQual.Environment.InterpreterT
    (CilQual.Type.InterpreterT
    (CilQual.CilUnionQualType.CilUnionQualTypeT (CilQual.Environment.CilFieldOrVar) (TestUtil.CilQualUtil.DummyContext)
    (Identity))))))
open S.QualType.Qual
open S.QualType
open S

module Setup1 = TestUtil.CilQualUtil.Setup (S)
open Setup1
module Setup2 = TestUtil.TypeQualUtil.Setup (S)
open Setup2


(* test helper for statements *)
let test_stmt stmt ?(label=stmt) ?(typedecls=[]) vardecls test =
    label >:: begin fun () ->
        let cilenv, formatcilenv = create_env typedecls vardecls in

        (* Cil statements to interpret *)
        let stmt = Formatcil.cStmt (preprocess stmt) Cil.makeGlobalVar Cil.locUnknown formatcilenv in

        assert_log "@[<v>";
        (* show Cil environment and statement structure *)
        assert_log "@[<v2>Environment:@ %a@]@\n" (env_printer Cil.plainCilPrinter) cilenv;
        assert_log "@[<v2>Statement:@ %a@]@\n" (adapt_cil_printer Cil.plainCilPrinter#pStmt) stmt;

        (* CilQual interpreter for statements *)
        let expM = interpret_stmt stmt in

        (* run interpreter *)
        let (((((result, constraints), _), _), _), env) =
            run expM ((((((), QualGraph.empty), emptyContext), 0), emptyUnionTable), emptyEnv) in

        (* print the result, environment and constraints *)
        assert_log "@[<v2>Result:@ %a@]@\n" QualType.printer result;
        assert_log "@[<v2>Environment:@ %a@]@\n" cilqual_env_printer env;
        assert_log "@[<v2>Constraints:@ %a@]@\n" QualGraph.printer constraints;
        assert_log "@]";

        (* finally run the test *)
        test (cilenv, env) result constraints
    end

(* test helper that tests all permutations of statements *)
let test_permute_stmt stmtlist ?(typedecls=[]) vardecls test =
    List.map begin fun ss ->
        test_stmt ("{ " ^ (String.concat "; " ss) ^ "; }") ~typedecls vardecls test
    end (permute stmtlist)


(*
 * OUnit test suite
 *)

let sequence_testsuite = "Instruction Sequence" >::: [
    (*
     * Statements that do not include branches/loops
     *)
    test_stmt "x = (int $a) 0;"
        [ ("x", "int") ]
        begin fun env result constraints ->
            let x = lookup_env "x" env in
            assert_qualtype_match begin fun (Ref (_, Base (Var _ as qv))) ->
                assert_only_paths [ (Const "a", qv) ] constraints
            end x
        end;

    (* Formatcil: statement sequences must be enclosed in { ... } *)
    test_stmt "{ x = (int $a) 0; return x; }"
        [ ("x", "int") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "a", qv) ] constraints
            end result
        end;

    test_stmt "y = x;"
        [ ("x", "int $a"); ("y", "int $b") ]
        begin fun env result constraints ->
            assert_only_paths [ (Const "a", Const "b") ] constraints
        end;

    test_stmt "return (int $a)0;"
        []
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "a", qv); (qv, Const "a") ] constraints
            end result
        end;

    (* Formatcil: statement sequences must be enclosed in { ... } *)
    test_stmt "{ c.f1 = (int $a)0; return d.f1; }"
        ~typedecls:[ `Struct ("c_t", [ ("f1", "int") ] ) ]
        [ ("c", "struct %c:c_t"); ("d", "struct %c:c_t") ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "a", qv) ] constraints
            end result
        end;

    "int *** to void * to int ***" >::: test_permute_stmt
        [ "void_ptr = x_ptr_ptr_ptr"; "y_ptr_ptr_ptr = void_ptr" ]
        [ ("void_ptr", "void $a * $b");
          ("x_ptr_ptr_ptr", "int $c * $d * $e * $f"); ("y_ptr_ptr_ptr", "int $g * $h * $i * $j") ]
        begin fun env result constraints -> assert_only_paths
            [ (Const "f", Const "b"); (* void_ptr/x_ptr_ptr_ptr *)
              (Const "b", Const "j"); (* y_ptr_ptr_ptr/void_ptr *)
              (Const "c", Const "g"); (Const "g", Const "c"); (* y_ptr_ptr_ptr/x_ptr_ptr_ptr *)
              (Const "d", Const "h"); (Const "h", Const "d");
              (Const "e", Const "i"); (Const "i", Const "e");
              (Const "f", Const "j") ] constraints
        end;

    "void * to int ***, void * to int ***" >::: test_permute_stmt
        [ "x_ptr_ptr_ptr = void_ptr"; "y_ptr_ptr_ptr = void_ptr" ]
        [ ("void_ptr", "void $a * $b");
          ("x_ptr_ptr_ptr", "int $c * $d * $e * $f"); ("y_ptr_ptr_ptr", "int $g * $h * $i * $j") ]
        begin fun env result constraints -> assert_only_paths
            [ (Const "b", Const "f"); (* x_ptr_ptr_ptr/void_ptr *)
              (Const "b", Const "j"); (* y_ptr_ptr_ptr/void_ptr *)
              (Const "c", Const "g"); (Const "g", Const "c"); (* x_ptr_ptr_ptr/y_ptr_ptr_ptr *)
              (Const "d", Const "h"); (Const "h", Const "d");
              (Const "e", Const "i"); (Const "i", Const "e") ] constraints
        end;

    "int *** to void *, int *** to void *" >::: test_permute_stmt
        [ "void_ptr = x_ptr_ptr_ptr"; "void_ptr = y_ptr_ptr_ptr" ]
        [ ("void_ptr", "void $a * $b");
          ("x_ptr_ptr_ptr", "int $c * $d * $e * $f"); ("y_ptr_ptr_ptr", "int $g * $h * $i * $j") ]
        begin fun env result constraints -> assert_only_paths
            [ (Const "f", Const "b"); (* void_ptr/x_ptr_ptr_ptr *)
              (Const "j", Const "b"); (* void_ptr/y_ptr_ptr_ptr *)
              (Const "c", Const "g"); (Const "g", Const "c"); (* x_ptr_ptr_ptr/y_ptr_ptr_ptr *)
              (Const "d", Const "h"); (Const "h", Const "d");
              (Const "e", Const "i"); (Const "i", Const "e") ] constraints
        end;

    "int *** to void * to void * to int ***" >::: test_permute_stmt
        [ "void_ptr1 = x_ptr_ptr_ptr"; "void_ptr2 = void_ptr1"; "y_ptr_ptr_ptr = void_ptr2" ]
        [ ("void_ptr1", "void $a * $b"); ("void_ptr2", "void $c * $d");
          ("x_ptr_ptr_ptr", "int $e * $f * $g * $h"); ("y_ptr_ptr_ptr", "int $i * $j * $k * $l") ]
        begin fun env result constraints -> assert_only_paths
            [ (Const "b", Const "d"); (* void_ptr2/void_ptr1 *)
              (Const "h", Const "b"); (* void_ptr1/x_ptr_ptr_ptr *)
              (Const "b", Const "l"); (* y_ptr_ptr_ptr/void_ptr1 *)
              (Const "h", Const "d"); (* void_ptr2/x_ptr_ptr_ptr *)
              (Const "d", Const "l"); (* y_ptr_ptr_ptr/void_ptr2 *)
              (Const "e", Const "i"); (Const "i", Const "e"); (* x_ptr_ptr_ptr/y_ptr_ptr_ptr *)
              (Const "f", Const "j"); (Const "j", Const "f");
              (Const "g", Const "k"); (Const "k", Const "g");
              (Const "h", Const "l") ] constraints
        end;

    "void * to void *, void * to int ***, void * to int ***" >::: test_permute_stmt
        [ "void_ptr2 = void_ptr1"; "x_ptr_ptr_ptr = void_ptr1"; "y_ptr_ptr_ptr = void_ptr2" ]
        [ ("void_ptr1", "void $a * $b"); ("void_ptr2", "void $c * $d");
          ("x_ptr_ptr_ptr", "int $e * $f * $g * $h"); ("y_ptr_ptr_ptr", "int $i * $j * $k * $l") ]
        begin fun env result constraints -> assert_only_paths
            [ (Const "b", Const "d"); (* void_ptr2/void_ptr1 *)
              (Const "b", Const "h"); (* x_ptr_ptr_ptr/void_ptr1 *)
              (Const "b", Const "l"); (* y_ptr_ptr_ptr/void_ptr1 *)
              (Const "d", Const "l"); (* y_ptr_ptr_ptr/void_ptr2 *)
              (Const "e", Const "i"); (Const "i", Const "e"); (* x_ptr_ptr_ptr/y_ptr_ptr_ptr *)
              (Const "f", Const "j"); (Const "j", Const "f");
              (Const "g", Const "k"); (Const "k", Const "g") ] constraints
        end;

    "void * to void *, int *** to void *, int *** to void *" >::: test_permute_stmt
        [ "void_ptr2 = void_ptr1"; "void_ptr1 = x_ptr_ptr_ptr"; "void_ptr2 = y_ptr_ptr_ptr" ]
        [ ("void_ptr1", "void $a * $b"); ("void_ptr2", "void $c * $d");
          ("x_ptr_ptr_ptr", "int $e * $f * $g * $h"); ("y_ptr_ptr_ptr", "int $i * $j * $k * $l") ]
        begin fun env result constraints -> assert_only_paths
            [ (Const "b", Const "d"); (* void_ptr2/void_ptr1 *)
              (Const "h", Const "b"); (* void_ptr1/x_ptr_ptr_ptr *)
              (Const "h", Const "d"); (* void_ptr2/x_ptr_ptr_ptr *)
              (Const "l", Const "d"); (* void_ptr2/y_ptr_ptr_ptr *)
              (Const "e", Const "i"); (Const "i", Const "e"); (* x_ptr_ptr_ptr/y_ptr_ptr_ptr *)
              (Const "f", Const "j"); (Const "j", Const "f");
              (Const "g", Const "k"); (Const "k", Const "g") ] constraints
        end;

    "int *** to void * to int to void * to int ***" >::: test_permute_stmt
        [ "void_ptr1 = x_ptr_ptr_ptr"; "z = void_ptr1"; "void_ptr2 = z"; "y_ptr_ptr_ptr = void_ptr2" ]
        [ ("void_ptr1", "void $a * $b"); ("void_ptr2", "void $c * $d");
          ("x_ptr_ptr_ptr", "int $e * $f * $g * $h"); ("y_ptr_ptr_ptr", "int $i * $j * $k * $l"); ("z", "int") ]
        begin fun env result constraints -> assert_only_paths
            [ (Const "b", Const "d"); (* void_ptr2/void_ptr1 *)
              (Const "h", Const "b"); (* void_ptr1/x_ptr_ptr_ptr *)
              (Const "b", Const "l"); (* y_ptr_ptr_ptr/void_ptr1 *)
              (Const "h", Const "d"); (* void_ptr2/x_ptr_ptr_ptr *)
              (Const "d", Const "l"); (* y_ptr_ptr_ptr/void_ptr2 *)
              (Const "e", Const "i"); (Const "i", Const "e"); (* x_ptr_ptr_ptr/y_ptr_ptr_ptr *)
              (Const "f", Const "j"); (Const "j", Const "f");
              (Const "g", Const "k"); (Const "k", Const "g");
              (Const "h", Const "l") ] constraints
        end;

    "int(*)(int) to void * to int(*)(int)" >::: test_permute_stmt
        [ "void_ptr = foo"; "bar = void_ptr" ]
        [ ("void_ptr", "void $a * $b"); ("foo", "int $c ($d * $e)(int $f)"); ("bar", "int $g ($h * $i)(int $j)") ]
        begin fun env result constraints -> assert_only_paths
            [ (Const "d", Const "b"); (* void_ptr1/foo *)
              (Const "b", Const "h"); (* bar/void_ptr1 *)
              (Const "d", Const "h"); (* foo/bar *)
              (Const "c", Const "g"); (Const "j", Const "f") ] constraints
        end;

    "void * to int(*)(int), void * to int(*)(int)" >::: test_permute_stmt
        [ "foo = void_ptr"; "bar = void_ptr" ]
        [ ("void_ptr", "void $a * $b"); ("foo", "int $c ($d * $e)(int $f)"); ("bar", "int $g ($h * $i)(int $j)") ]
        begin fun env result constraints -> assert_only_paths
            [ (Const "b", Const "d");  (* void_ptr1/foo *)
              (Const "b", Const "h") ] (* void_ptr1/bar *)
            constraints
        end;

    "int(*)(int) to void *, int(*)(int) to void * " >::: test_permute_stmt
        [ "void_ptr = foo"; "void_ptr = bar" ]
        [ ("void_ptr", "void $a * $b"); ("foo", "int $c ($d * $e)(int $f)"); ("bar", "int $g ($h * $i)(int $j)") ]
        begin fun env result constraints -> assert_only_paths
            [ (Const "d", Const "b");  (* foo/void_ptr1 *)
              (Const "h", Const "b") ] (* bar/void_ptr1 *)
            constraints
        end;

    "int(*)(int, int) to void * to void * to int(*)(int, int)" >::: test_permute_stmt
        [ "void_ptr1 = foo"; "void_ptr2 = void_ptr1"; "bar = void_ptr2" ]
        [ ("void_ptr1", "void $a * $b"); ("void_ptr2", "void $c * $d");
          ("foo", "int $e ($f * $g)(int $h, int $i)"); ("bar", "int $j ($k * $l)(int $m, int $n)") ]
        begin fun env result constraints -> assert_only_paths
            [ (Const "b", Const "d"); (* void_ptr1/void_ptr2 *)
              (Const "f", Const "b"); (* void_ptr1/foo *)
              (Const "b", Const "k"); (* void_ptr1/bar *)
              (Const "f", Const "d"); (* void_ptr2/foo *)
              (Const "d", Const "k"); (* void_ptr2/bar *)
              (Const "f", Const "k"); (* foo/bar *)
              (Const "e", Const "j"); (Const "m", Const "h"); (Const "n", Const "i") ] constraints
        end;

    "int(*)(int, int) to void * to int to void * to int(*)(int, int)" >::: test_permute_stmt
        [ "void_ptr1 = foo"; "x = void_ptr1"; "void_ptr2 = x"; "bar = void_ptr2" ]
        [ ("void_ptr1", "void $a * $b"); ("void_ptr2", "void $c * $d"); ("x", "int");
          ("foo", "int $e ($f * $g)(int $h, int $i)"); ("bar", "int $j ($k * $l)(int $m, int $n)") ]
        begin fun env result constraints -> assert_only_paths
            [ (Const "b", Const "d"); (* void_ptr1/void_ptr2 *)
              (Const "f", Const "b"); (* void_ptr1/foo *)
              (Const "b", Const "k"); (* void_ptr1/bar *)
              (Const "f", Const "d"); (* void_ptr2/foo *)
              (Const "d", Const "k"); (* void_ptr2/bar *)
              (Const "f", Const "k"); (* foo/bar *)
              (Const "e", Const "j"); (Const "m", Const "h"); (Const "n", Const "i") ] constraints
        end;
]

let branches_testsuite = "Conditional Branches" >::: [
    (*
     * Statements with branches
     *)
    test_stmt "if (0) return (int $a) 0; else return (int $b) 0;"
        []
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Base (Var _ as qv)) ->
                assert_only_paths [ (Const "a", qv); (Const "b", qv) ] constraints
            end result
        end;

    test_stmt "if (0) return foo; else return bar;"
        [ ("foo", "int $fooa ($foob *)(int $fooc)");
          ("bar", "int $bara ($barb *)(int $barc)"); ]
        begin fun env result constraints ->
            assert_qualtype_match begin fun (Fn (Var _ as qvb, Base (Var _ as qva), [ Base (Var _ as qvc) ])) ->
                assert_only_paths
                    [ (Const "fooa", qva); (Const "bara", qva);
                      (Const "foob", qvb); (Const "barb", qvb);
                      (qvc, Const "fooc"); (qvc, Const "barc") ] constraints
            end result
        end;
]

let testsuite = "Statement" >::: [
    sequence_testsuite;
    branches_testsuite;
]

