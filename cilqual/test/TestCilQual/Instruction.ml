open TestUtil.MyOUnit
open Control.Monad

open TypeQual.QualType.QualType
open TypeQual.QualType.QualTypeConstraints.Qual

module I =
    CilQual.Instruction.InterpreterT
        (CilQual.Expression.InterpreterT
            (CilQual.Environment.InterpreterT
                (CilQual.Type.InterpreterT
                    (CilQual.Config.InterpreterT
                        (TypeQual.QualType.QualTypeT (Identity))))))
open I

module Setup1 = TestUtil.CilQualUtil.Setup (I)
open Setup1
module Setup2 = TestUtil.TypeQualUtil.Setup (I)
open Setup2


(* test helper for instructions *)
let test_instr instr ?(label=instr) ?(typedecls=[]) vardecls test =
    label >:: begin fun () ->
        let cilenv, formatcilenv = create_env typedecls vardecls in

        (* Cil instruction to interpret *)
        let instr = Formatcil.cInstr (preprocess_cilqual instr) dummy_loc formatcilenv in

        assert_log "@[<v>";
        (* show Cil environment and instruction structure *)
        assert_log "@[<v2>Environment:@ %a@]@\n" (env_printer Cil.plainCilPrinter) cilenv;
        assert_log "@[<v2>Instruction:@ %a@]@\n" (adapt_cil_printer Cil.plainCilPrinter#pInstr) instr;

        (* CilQual interpreter for instruction *)
        let expM = interpret_instr instr in

        (* run interpreter *)
        let ((((), env), constraints), _) = run expM emptyEnv Constraints.empty 0 in

        (* print the environment and constraints *)
        assert_log "@[<v2>Environment:@ %a@]@\n" cilqual_env_printer env;
        assert_log "@[<v2>Constraints:@ %a@]@\n" Constraints.printer constraints;
        assert_log "@]";

        (* finally run the test *)
        test (cilenv, env) constraints
    end


(*
 * OUnit test suite
 *)

let variable_assignments_testsuite = "Variable Assignments" >::: [
    (*
     * variable assignments
     *)
    test_instr "x = (int $a) 0;"
        [ ("x", "int") ]
        begin fun env constraints ->
            let x = lookup_env "x" env in
            assert_qualtype_match begin fun (Ref (_, Base (Var _ as qv))) ->
                assert_only_paths [ (Const "a", qv) ] constraints
            end x
        end;

    test_instr "y = x;"
        [ ("x", "int $a"); ("y", "int $b") ]
        begin fun env constraints ->
            assert_only_paths [ (Const "a", Const "b") ] constraints
        end;

    test_instr "*y = x;"
        [ ("x", "int $a"); ("y", "int *") ]
        begin fun env constraints ->
            let y = lookup_env "y" env in
            assert_qualtype_match begin fun (Ref (_, Ref (_, Base (Var _ as qv)))) ->
                assert_only_paths [ (Const "a", qv) ] constraints
            end y
        end;

    test_instr "y = &x;"
        [ ("x", "int $a"); ("y", "int $b *") ]
        begin fun env constraints ->
            let y = lookup_env "y" env in
            assert_qualtype_match begin fun (Ref (_, Ref (_, Base (Var _ as qv)))) ->
                assert_only_paths
                    [ (Const "a", qv); (qv, Const "a");
                      (Const "b", qv); (qv, Const "b");
                      (Const "a", Const "b"); (Const "b", Const "a") ] constraints
            end y
        end;

    (* Formatcil: & operator is explicitly required to take address of arrays *)
    test_instr "ys = &xs;"
        [ ("xs", "int $a []"); ("ys", "int $b (*)[]"); ]
        begin fun env constraints ->
            assert_only_paths [ (Const "a", Const "b"); (Const "b", Const "a") ] constraints
        end;

    (* Formatcil: & operator is explicitly required to take address of functions *)
    test_instr "bar = &int_fn_int;"
        [ ("int_fn_int", "int $a ($b)(int $c)"); ("bar", "int (*)(int)") ]
        begin fun env constraints ->
            let bar = lookup_env "bar" env in
            assert_qualtype_match begin
                fun (Ref (_, Fn (Var _ as qvb, Base (Var _ as qva), [ (Base (Var _ as qvc)) ]))) ->
                    assert_only_paths
                        [ (Const "a", qva);  (* return value is co-variant *)
                          (Const "b", qvb);  (* function qualifier is co-variant *)
                          (qvc, Const "c") ] (* arguments are contra-variant *)
                        constraints
            end bar
        end;

    test_instr "bar = int_fn_ptr_int;"
        [ ("int_fn_ptr_int", "int $a ($b *)(int $c)"); ("bar", "int (*)(int)") ]
        begin fun env constraints ->
            let bar = lookup_env "bar" env in
            assert_qualtype_match begin
                fun (Ref (_, Fn (Var _ as qvb, Base (Var _ as qva), [ (Base (Var _ as qvc)) ]))) ->
                    assert_only_paths
                        [ (Const "a", qva);  (* return value is co-variant *)
                          (Const "b", qvb);  (* function qualifier is co-variant *)
                          (qvc, Const "c") ] (* arguments are contra-variant *)
                        constraints
            end bar
        end;

    test_instr "bar = ((int $a ($b *)(int $c))0);"
        [ ("bar", "int (*)(int)") ]
        begin fun env constraints ->
            let bar = lookup_env "bar" env in
            assert_qualtype_match begin
                fun (Ref (_, Fn (Var _ as qvb, Base (Var _ as qva), [ (Base (Var _ as qvc)) ]))) ->
                    assert_only_paths
                        [ (Const "a", qva);  (* return value is co-variant *)
                          (Const "b", qvb);  (* function qualifier is co-variant *)
                          (qvc, Const "c") ] (* arguments are contra-variant *)
                        constraints
            end bar
        end;

    test_instr "bar = &int_fn_ptr_int;"
        [ ("int_fn_ptr_int", "int $a ($b *)(int $c)"); ("bar", "int (**)(int)") ]
        begin fun env constraints ->
            let bar = lookup_env "bar" env in
            assert_qualtype_match begin
                fun (Ref (_, Ref (_, Fn (Var _ as qvb, Base (Var _ as qva), [ (Base (Var _ as qvc)) ])))) ->
                    assert_only_paths
                        [ (Const "a", qva);                   (* return value is co-variant *)
                          (Const "b", qvb); (qvb, Const "b"); (* function qualifier is non-variant because of ref *)
                          (qvc, Const "c") ]                  (* arguments are contra-variant *)
                        constraints
            end bar
        end;

    test_instr "c = d;"
        ~typedecls:[ `Struct ("c_t", [ ("f1", "int $a"); ("f2", "int $b") ]) ]
        [ ("c", "struct %c:c_t $c"); ("d", "struct %c:c_t $d") ]
        begin fun env constraints ->
            assert_only_paths [ (Const "d", Const "c") ] constraints
        end;

    test_instr "void_ptr = x;"
        [ ("void_ptr", "void $a * $b"); ("x", "int $c") ]
        begin fun env constraints ->
            assert_only_paths [ (Const "c", Const "b") ] constraints
        end;

    test_instr "void_ptr = x_ptr;"
        [ ("void_ptr", "void $a * $b"); ("x_ptr", "int $c * $d") ]
        begin fun env constraints ->
            assert_only_paths [ (Const "c", Const "a"); (Const "a", Const "c"); (Const "d", Const "b") ] constraints
        end;

    test_instr "void_ptr = x_ptr_ptr;"
        [ ("void_ptr", "void $a * $b"); ("x_ptr_ptr", "int $c * $d * $e") ]
        begin fun env constraints ->
            assert_only_paths [ (Const "d", Const "a"); (Const "a", Const "d"); (Const "e", Const "b") ] constraints
        end;

    test_instr "void_ptr = foo;"
        [ ("void_ptr", "void $a * $b"); ("foo", "int $c($d * $e)(int $f)") ]
        begin fun env constraints ->
            assert_only_paths [ (Const "d", Const "b") ] constraints
        end;
]

let function_calls_testsuite = "Function Calls" >::: [
    (*
     * function calls
     *)
    test_instr "void_fn_void();"
        [ ("void_fn_void", "void ()(void)") ]
        begin fun env constraints ->
            ()
        end;

    (* Formatcil: function pointers need to be explicitly deref'ed *)
    test_instr "(*void_fn_ptr_void)();"
        [ ("void_fn_ptr_void", "void (*)(void)") ]
        begin fun env constraints ->
            ()
        end;

    test_instr "(*((void (*)(void))0))();"
        []
        begin fun env constraints ->
            ()
        end;

    test_instr "x = int_fn_void();"
        [ ("x", "int $a"); ("int_fn_void", "int $b ()(void)") ]
        begin fun env constraints ->
            assert_only_paths [ (Const "b", Const "a") ] constraints
        end;

    test_instr "x = int_fn_int(y);"
        [ ("x", "int $a"); ("y", "int $b"); ("int_fn_int", "int $c ()(int $d)") ]
        begin fun env constraints ->
            assert_only_paths [ (Const "c", Const "a"); (Const "b", Const "d") ] constraints
        end;

    test_instr "void_fn_int_vararg(x, y, z);"
        [ ("x", "int $a"); ("y", "int $b"); ("z", "int $c * $d"); ("void_fn_int_vararg", "void ()(int, ...)") ]
        begin fun env constraints ->
            let f = lookup_env "void_fn_int_vararg" env in
            assert_qualtype_match begin
                fun (Fn (_, Base (Var _), [ Base (Var _ as qv1); Ref (Var _ as qv2, Base (Var _ as qv3)) ])) ->
                    assert_only_paths
                        [ (Const "a", qv1); (Const "b", qv2);
                          (Const "c", qv3); (qv3, Const "c"); (Const "d", qv2) ] constraints
            end f;
        end;
]

let testsuite = "Instruction" >::: [
    variable_assignments_testsuite;
    function_calls_testsuite;
]


