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
                            (CilQual.CilQualType.CilQualTypeT (CilQual.Environment.CilFieldOrVar) (Identity)))))))
open G.QualType.Qual
open G.QualType
open G

module Setup1 = TestUtil.CilQualUtil.Setup (G)
open Setup1
module Setup2 = TestUtil.TypeQualUtil.Setup (G)
open Setup2


(* test helper for compilation units (files) *)
let test_cilqual content ?(label=content) lattice expected_result =
    label >:: bracket begin fun () ->
        let filename, fileout = Filename.open_temp_file "test_cilqual_integration." ".c" in
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

        (* check the result against the lattice *)
        let assert_test = match expected_result with
            | `Ok -> assert_no_paths
            | `Fail -> assert_some_paths
        in
        assert_test lattice constraints
    end begin fun filename ->
        Unix.unlink filename
    end

(*
 * OUnit test suite
 *)

let cqual_testsuite = "CQual" >::: [
    test_cilqual ~label:"strcpy of $tainted to $untainted should fail" "
        char * strcpy(char * s1, const char * s2) { *s1 = *s2; return s1; }

        void foo(void) {
            char $tainted * a;
            char $untainted * b;

            strcpy(b, a);
        }
    " [ (Const "tainted", Const "untainted") ] `Fail;

    test_cilqual ~label:"$tainted value casted to $untainted then assigned to $untainted should succeed" "
        void bar(void) {
            int $tainted a;
            int $untainted b;

            b = (int $untainted) a;
        }
    " [ (Const "tainted", Const "untainted") ] `Ok;

    test_cilqual ~label:"assign struct type with $untainted field to struct literal with $tainted field should fail" "
        struct foo { int $untainted x; int y; };
        void bar(void) {
            struct foo a = (struct foo) { (int $tainted) 1, 2 };
        }
    " [ (Const "tainted", Const "untainted") ] `Fail;

    test_cilqual ~label:"initialize struct type with $untainted field using $tainted designator should fail" "
        struct foo { int x; int $untainted y; };
        void bar(void) {
            struct foo a = { .y=(int $tainted) 3, .x=4 };
        }
    " [ (Const "tainted", Const "untainted") ] `Fail;

    test_cilqual ~label:"initialize struct type with $untainted field using $tainted flattened initializer \
                         should fail" "
        struct foo { int x; int $untainted y; int z; };
        struct bar { int r; struct foo s; int t; };
        void baz(void) {
            struct bar a = { 1, 2, (int $tainted) 3, 4, 5 };
        }
    " [ (Const "tainted", Const "untainted") ] `Fail;

    test_cilqual ~label:"initialize $untainted array type with $tainted designator should fail" "
        void bar(void) {
            int $untainted a[32] = { [0 ... 32-1] = (int $tainted) 3 };
        }
    " [ (Const "tainted", Const "untainted") ] `Fail;

    test_cilqual ~label:"initialize $untainted n-d array type with $tainted initializer should fail" "
        void bar(void) {
            int $untainted a[3][5] = {{1, 2, 3, 4, 5}, {6, 7, (int $tainted) 8, 9, 10}, {11, 12, 13, 15}};
        }
    " [ (Const "tainted", Const "untainted") ] `Fail;

    test_cilqual ~label:"initialize $untainted n-d array type with $tainted flattened initializer should fail" "
        void bar(void) {
            int $untainted a[3][5] = { 1, 2, 3, 4, 5, 6, 7, (int $tainted) 8, 9, 10, 11, 12, 13, 15};
        }
    " [ (Const "tainted", Const "untainted") ] `Fail;

    test_cilqual ~label:"function call with $tainted/$untainted argument should succeed due to contra-variance" "
        void foo(char);
        void bar(void) {
            char $tainted a;
            char $untainted b;

            foo(a);
            foo(b);
        }
    " [ (Const "tainted", Const "untainted") ] `Ok;

    test_cilqual ~label:"function call with pointer to $tainted/$untainted argument should fail due to \
                         non-variance and lack of context-sensitivity" "
        void foo(char *);
        void bar(void) {
            char $tainted * a;
            char $untainted * b;

            foo(a);
            foo(b);
        }
    " [ (Const "tainted", Const "untainted") ] `Fail;

    test_cilqual ~label:"one function call that violates $user/$kernel and another side function call should fail" "
        void func1(int $kernel a);

        void func2(void) {
            int $user b;
            func1(b);
        }

        void func3(void) {
            int a;
            func1(a);
        }
    " [ (Const "kernel", Const "user"); (Const "user", Const "kernel") ] `Fail;

(* (* currently an error because the constraint solver is incomplete *)
    test_cilqual ~label:"variable used as both $user and $kernel should fail" "
        void foo(char $kernel y) {}
        void foo2(char $user x) {}

        int main(void) {
            char x;

            foo2(x);
            foo(x);
        }
    " [ (Const "kernel", Const "user"); (Const "user", Const "kernel") ] `Fail;
*)

    test_cilqual ~label:"using a $tainted string from getenv() as an $untainted format string should fail" "
        $tainted char *getenv(const char *name);
        int printf($untainted const char *fmt, ...);

        int main(void) {
            char *s, *t;
            s = getenv(\"LD_LIBRARY_PATH\");
            t = s;
            printf(t);
            return 0;
        }
    " [ (Const "tainted", Const "untainted") ] `Fail;

    test_cilqual ~label:"using a $tainted string from getenv() as an $untainted format string, via several \
                         function calls, should fail" "
        $tainted char *getenv(const char *name);
        int printf($untainted char *fmt, ...);

        char *f3(char *t) { return t; }
        char *f2(char *u) { return f3(u); }
        char *f1(char *v) { return f2(v); }

        int main(void) {
            char *s, *unclean;
            unclean = getenv(\"PATH\");
            s = f1(unclean);
            printf(s);
            return 0;
        }
    " [ (Const "tainted", Const "untainted") ] `Fail;

    test_cilqual ~label:"using 4-digit year format $YYYY in place of 2-digit year format $YY should fail" "
        int printf(const char * format, ...);
        void pr_year(char * $YY year) {
            printf(\"The year is 19%s\", year);
        }
        int main() {
            pr_year((char * $YY) \"99\");
            pr_year((char * $YYYY) \"2000\");
            return 0;
        }
    " [ (Const "YY", Const "YYYY"); (Const "YYYY", Const "YY") ] `Fail;
]

let testsuite = "Integration" >::: [
    cqual_testsuite;
]

