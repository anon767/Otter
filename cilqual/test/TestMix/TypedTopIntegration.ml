open TestUtil.MyOUnit
open TestUtil.CilQualUtil
open TestUtil.MixUtil

open Mix.Feature

(* test helper for compilation units (files) *)
let test_mix content ?(label=content) test =
    label >:: bracket begin fun () ->
        let filename, fileout = Filename.open_temp_file "test_mix_typedtopintegration." ".c" in
        output_string fileout (preprocess content);
        close_out fileout;
        filename
    end begin fun filename ->
        Errormsg.hadErrors := false;
        let file = Frontc.parse filename () in
        assert_bool "Cil parse error" (not !Errormsg.hadErrors);
        Rmtmps.removeUnusedTemps file;

        prepare_file file;

        let file, solution = dispatch_loop (TypedInterpreter.exec file) in
        assert_log "@[<v>";
        assert_log "@[<v2>Constraints:@ %a@]@\n" constraints_printer solution;
        assert_log "@]";

        test file solution
    end begin fun filename ->
        Unix.unlink filename
    end

(*
 * OUnit test suite
 *)

let typed_only_testsuite = "Typed-only" >::: [
    test_mix "
        int main(void) { return 0; }
    " begin fun file solution ->
        assert_discrete_satisfiable solution
    end;

    test_mix ~label:"global variable set null locally" "
        int x = 0;
        int * $(nonnull) y = &x;
        void foo(void) {
            y = NULL;
            y = &x;
        }
        int main(void) {
            foo();
            return 0;
        }
    " begin fun file solution ->
        assert_discrete_unsatisfiable solution
    end;
]

let leaf_symbolic_testsuite = "Leaf Symbolic" >::: [
    test_mix ~label:"global variable set null locally" "
        int x = 0;
        int * $(nonnull) y = &x;
        void foo(void) MIX(symbolic) {
            y = NULL;
            y = &x;
        }
        int main(void) {
            foo();
            return 0;
        }
    " begin fun file solution ->
        assert_discrete_satisfiable solution
    end;

    test_mix ~label:"global variable set null" "
        int x = 0;
        int * $(nonnull) y = &x;
        void foo(void) MIX(symbolic) {
            y = NULL;
        }
        int main(void) {
            foo();
            return 0;
        }
    " begin fun file solution ->
        assert_discrete_unsatisfiable solution
    end;

    test_mix ~label:"output argument set null locally" "
        int x = 0;
        void foo(void ** y) MIX(symbolic) {
            *y = NULL;
            *y = &x;
        }
        int main(void) {
            int * $(nonnull) z = &x;
            foo(&z);
            return 0;
        }
    " begin fun file solution ->
        assert_discrete_satisfiable solution
    end;

    test_mix ~label:"output argument set null" "
        int x = 0;
        void foo(void ** y) MIX(symbolic) {
            *y = NULL;
        }
        int main(void) {
            int * $(nonnull) z = &x;
            foo(&z);
            return 0;
        }
    " begin fun file solution ->
        assert_discrete_unsatisfiable solution
    end;

    test_mix ~label:"field set null locally" "
        int x = 0;
        struct a { int * $(nonnull) i; } y = { &x };
        void foo(void) MIX(symbolic) {
            y.i = NULL;
            y.i = &x;
        }
        int main(void) {
            foo();
            return 0;
        }
    " begin fun file solution ->
        assert_discrete_satisfiable solution
    end;

    test_mix ~label:"field set null" "
        int x = 0;
        struct a { int * $(nonnull) i; } y = { &x };
        void foo(void) MIX(symbolic) {
            y.i = NULL;
        }
        int main(void) {
            foo();
            return 0;
        }
    " begin fun file solution ->
        assert_discrete_unsatisfiable solution
    end;
]

let testsuite = "TypedTopIntegration" >::: [
    typed_only_testsuite;
    leaf_symbolic_testsuite;
]

