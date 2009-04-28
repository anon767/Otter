open TestUtil.MyOUnit
open Control.Monad

(* setup CilQual interpreter monad stack *)
module G =
    CilQual.Global.InterpreterT
    (CilQual.Statement.InterpreterT
    (CilQual.Instruction.InterpreterT
    (CilQual.Expression.InterpreterT
    (CilQual.Environment.InterpreterT
    (CilQual.Type.InterpreterT
    (CilQual.CilQualType.CilQualTypeT (CilQual.Environment.CilFieldOrVar) (TestUtil.CilQualUtil.DummyContext)
    (Identity)))))))
open G.QualType.Qual
open G.QualType
open G

module Setup1 = TestUtil.CilQualUtil.Setup (G)
open Setup1
module Setup2 = TestUtil.TypeQualUtil.Setup (G)
open Setup2


(* test helper for compilation units (files) *)
let test_file content ?(label=content) test =
    label >:: bracket begin fun () ->
        let filename, fileout = Filename.open_temp_file "test_cilqual_global." ".c" in
        output_string fileout (preprocess content);
        close_out fileout;
        filename
    end begin fun filename ->
        Errormsg.hadErrors := false;
        let file = Frontc.parse filename () in
        assert_bool "Cil parse error" (not !Errormsg.hadErrors);

        CilQual.Feature.prepare_file file;

        assert_log "@[<v>";
        (* show Cil file structure *)
        assert_log "@[<v2>Compilation unit:@ %a@]@\n" (file_printer Cil.plainCilPrinter) file;

        (* CilQual interpreter for file *)
        let expM = interpret_file file in

        (* run interpreter *)
        let (((((), constraints), _), _), env) =
            run expM (((((), QualGraph.empty), (fileContext file)), 0), emptyEnv) in

        (* print the environment and constraints *)
        assert_log "@[<v2>Environment:@ %a@]@\n" cilqual_env_printer env;
        assert_log "@[<v2>Constraints:@ %a@]@\n" QualGraph.printer constraints;
        assert_log "@]";

        (* finally run the test *)
        test file env constraints
    end begin fun filename ->
        Unix.unlink filename
    end

let test_main_function body ?(label=body) test =
    let content = Format.sprintf "int main(void) { %s; return 0; }" body in
    test_file content ~label test


(*
 * OUnit test suite
 *)

let simple_testsuite = "Simple" >::: [
    test_file
        "int main(void) { return 0; }"
        begin fun file env constraints ->
            assert_nb_edges 0 constraints
        end;
]

let variables_testsuite = "Variables" >::: [
    test_main_function
        "int $a x; int $b y; y = x;"
        begin fun file env constraints ->
            assert_only_paths [ (Const "a", Const "b") ] constraints
        end;
]

let arrays_testsuite = "Arrays" >::: [
    test_main_function
        "int x, xs[2], *ys; ys = xs; ys = &xs[0]; ys = xs + 1;"
        begin fun file env constraints ->
            ()
        end;

    test_main_function
        "int x, xss[2][2], *ys, (*zs)[2]; \
         ys = xss[0]; ys = &xss[0][0]; zs = xss; zs = &xss[0]; \
         x = *ys; x = **xss; x = **zs;"
        begin fun file env constraints ->
            ()
        end;
]

let structs_testsuite = "Structs" >::: [
    test_main_function
        "struct c_t { int f1; }; \
         struct c_t c, d; \
         int $b x; \
         c.f1 = (int $a)0; \
         x = d.f1;"
        begin fun file env constraints ->
            assert_only_paths [ (Const "a", Const "b") ] constraints
        end;
]

let initializers_testsuite = "Initializers" >::: [
    test_file "int i = (int $a)0; int $b j = i;"
        begin fun file env constraints ->
            assert_only_paths [ (Const "a", Const "b") ] constraints
        end;

    test_file "int i = (int $a)0; int *j = &i; void foo() { int $b k = *j; }"
        begin fun file env constraints ->
            assert_only_paths [ (Const "a", Const "b") ] constraints
        end;

    test_file "int i[2] = { (int $a)0, (int $b)0 }; int $c j = i[0];"
        begin fun file env constraints ->
            assert_only_paths [ (Const "a", Const "c"); (Const "b", Const "c") ] constraints
        end;

    test_file "int i[2] = { (int $a)0, (int $b)0 }; int *j = i; int $c k = j[0];"
        begin fun file env constraints ->
            assert_only_paths [ (Const "a", Const "c"); (Const "b", Const "c") ] constraints
        end;

    test_file "struct s { int $a f; }; \
               struct s s1 = { (int $b) 0 };"
        begin fun file env constraints ->
            assert_only_paths [ (Const "b", Const "a") ] constraints
        end;

    test_file "struct s { int $a f; }; struct t { struct s f; int g; }; \
               struct t t1 = { .f.f=(int $b)0 };"
        begin fun file env constraints ->
            assert_only_paths [ (Const "b", Const "a") ] constraints
        end;

    test_file "struct s { int $a f; }; struct t { struct s f; }; struct u { struct t f; }; \
               struct u u1 = { .f.f.f=(int $b)0 };"
        begin fun file env constraints ->
            assert_only_paths [ (Const "b", Const "a") ] constraints
        end;

    test_file "struct s { int $a f; }; struct t { struct s f[2]; }; \
               struct t t1[2] = { { { { (int $b)0 }, { (int $c)0 } } }, { { { (int $d)0 }, { (int $e)0 } } } };"
        begin fun file env constraints ->
            assert_only_paths
                [ (Const "b", Const "a"); (Const "c", Const "a");
                  (Const "d", Const "a"); (Const "e", Const "a") ] constraints
        end;

    test_file "char $a * $b str = (char $c * $d)\"qwerty\";"
        begin fun file env constraints ->
            assert_only_paths [ (Const "c", Const "a"); (Const "a", Const "c"); (Const "d", Const "b") ] constraints
        end;

    test_file "char str[] = \"qwerty\";" (* can't annotate char-array initializer *)
        begin fun file env constraints ->
            ()
        end;
]

let functions_testsuite = "Functions" >::: [
    (* this uses the Frontc parser, which, unlike Formatcil, is faithful to C *)
    test_main_function
        "int (*foo)(void); foo = main;"
        begin fun file env constraints ->
            ()
        end;

    test_main_function
        "int (*foo)(void); foo = &main;"
        begin fun file env constraints ->
            ()
        end;

    test_main_function
        "int (*foo)(void); foo = *main;"
        begin fun file env constraints ->
            ()
        end;

    test_main_function
        "int (*foo)(void); foo = **main;"
        begin fun file env constraints ->
            ()
        end;

    test_main_function
        "int (*foo)(void); foo = foo;"
        begin fun file env constraints ->
            ()
        end;

    test_main_function
        "int (*foo)(void); foo = (int (*)(void))0;"
        begin fun file env constraints ->
            ()
        end;

    test_main_function
        "main();"
        begin fun file env constraints ->
            ()
        end;

    test_main_function
        "int (*foo)(void); foo();"
        begin fun file env constraints ->
            ()
        end;

    test_main_function
        "((int (*)(void))0)();"
        begin fun file env constraints ->
            ()
        end;

    test_file
        "int $b foo(int $a x) { return x; }"
        begin fun file env constraints ->
            assert_only_paths [ (Const "a", Const "b") ] constraints;
        end;

    test_file
        "void foo(int $b x) { } void bar(void) { int $a x; foo(x); }"
        begin fun file env constraints ->
            assert_only_paths [ (Const "a", Const "b") ] constraints;
        end;

    test_file
        "void foo(int x) { int $b y = x; } void bar(void) { int $a x; foo(x); }"
        begin fun file env constraints ->
            assert_only_paths [ (Const "a", Const "b") ] constraints;
        end;

    test_file
        "int $b foo(int x) { return x; } void bar(void) { int $a x; foo(x); }"
        begin fun file env constraints ->
            assert_only_paths [ (Const "a", Const "b") ] constraints;
        end;

    test_file
        "int foo(int x) { return x; } void bar(void) { int $a x; int $b y; y = foo(x); }"
        begin fun file env constraints ->
            assert_only_paths [ (Const "a", Const "b") ] constraints;
        end;
]

let testsuite = "Global" >::: [
    simple_testsuite;
    variables_testsuite;
    arrays_testsuite;
    structs_testsuite;
    initializers_testsuite;
    functions_testsuite;
]

