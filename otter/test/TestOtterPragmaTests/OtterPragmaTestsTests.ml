open TestUtil.MyOUnit
open TestUtil.OtterPragmaTests
open Otter


(* Test helpers that checks that pragma tests passes or fails as expected *)
let test_otter_pragma_tests ~label ~should_fail code =
    label >:: test_string_as_file "TestOtterPragmaTest." ".c" code begin fun path ->
        let success = try
            let () = test_otter_with_pragma path () in
            true
        with MyOUnitFailure ->
            false
        in
        if success && should_fail then assert_failure "@.Pragma test should have failed but did not.";
        if not success && not should_fail then assert_failure "@.Pragma test should not have failed but did."
    end

let should_pass_pragma_tests = test_otter_pragma_tests ~should_fail:false
let should_fail_pragma_tests = test_otter_pragma_tests ~should_fail:true


(*
 * OUnit test suite
 *)

let has_failing_assertions_testsuite = "has_failing_assertions" >::: [
    should_fail_pragma_tests ~label:"No assertions"
        "#pragma has_failing_assertions
        int main(void) {
            return 0;
        }";

    should_fail_pragma_tests ~label:"One passing assertion"
        "#pragma has_failing_assertions
        int main(void) {
            __ASSERT(1);
            return 0;
        }";

    should_pass_pragma_tests ~label:"One failing assertion"
        "#pragma has_failing_assertions
        int main(void) {
            __ASSERT(0);
            return 0;
        }";

    should_fail_pragma_tests ~label:"One passing assertion on one path, no assertions on the other path"
        "#pragma has_failing_assertions
        int main(void) {
            if (__SYMBOLIC()) __ASSERT(1);
            return 0;
        }";

    should_pass_pragma_tests ~label:"One failing assertion on one path, no assertions on the other path"
        "#pragma has_failing_assertions
        int main(void) {
            if (__SYMBOLIC()) __ASSERT(0);
            return 0;
        }";

    should_pass_pragma_tests ~label:"One passing assertion on one path, one failing assertion on the other path"
        "#pragma has_failing_assertions
        int main(void) {
            if (__SYMBOLIC()) __ASSERT(1);
            else __ASSERT(0);
            return 0;
        }";

    should_fail_pragma_tests ~label:"One passing assertion on both paths"
        "#pragma has_failing_assertions
        int main(void) {
            if (__SYMBOLIC()) __ASSERT(1);
            else __ASSERT(1);
            return 0;
        }";

    should_pass_pragma_tests ~label:"One failing assertion on both paths"
        "#pragma has_failing_assertions
        int main(void) {
            if (__SYMBOLIC()) __ASSERT(0);
            else __ASSERT(0);
            return 0;
        }";

    should_pass_pragma_tests ~label:"Assertion on symbolic value"
        "#pragma has_failing_assertions
        int main(void) {
            __ASSERT(__SYMBOLIC());
            return 0;
        }";
]

let command_line_testsuite = "command_line" >::: [
    should_fail_pragma_tests ~label:"Invalid no-argument command_line"
        "#pragma command_line()
        int main(int argc, char *argv[]) {
            return 0;
        }";

    should_pass_pragma_tests ~label:"One argument command_line"
        "#pragma command_line(\"a\")
        int main(int argc, char *argv[]) {
            __ASSERT(argc == 1);
            __ASSERT(argv[0][0] == 'a');
            return 0;
        }";

    should_pass_pragma_tests ~label:"Two arguments command_line"
        "#pragma command_line(\"a\", \"b\")
        int main(int argc, char *argv[]) {
            __ASSERT(argc == 2);
            __ASSERT(argv[0][0] == 'a');
            __ASSERT(argv[1][0] == 'b');
            return 0;
        }";
]

let expect_return_testsuite = "expect_return" >::: [
    should_pass_pragma_tests ~label:"One return"
        "#pragma expect_return()
        int main(void) {
            return 0;
        }";

    should_fail_pragma_tests ~label:"No parameter list"
        "#pragma expect_return
        int main(void) {
            return 0;
        }";

    should_fail_pragma_tests ~label:"One return but exit"
        "#pragma expect_return()
        int main(void) {
            _exit(0);
            return 0;
        }";

    should_fail_pragma_tests ~label:"One return but abandoned"
        "#pragma expect_return()
        int main(void) {
            __ASSERT(0);
            return 0;
        }";

    should_pass_pragma_tests ~label:"One return checking __return_code__"
        "#pragma expect_return(__return_code__ == 0)
        int main(void) {
            return 0;
        }";

    should_fail_pragma_tests ~label:"One return checking invalid __exit_code__"
        "#pragma expect_return(__exit_code__ == 0)
        int main(void) {
            return 0;
        }";

    should_pass_pragma_tests ~label:"One return checking global variable"
        "#pragma expect_return(x == 1)
        int x;
        int main(void) {
            x = 1;
            return 0;
        }";

    should_fail_pragma_tests ~label:"One return checking global variable but fails"
        "#pragma expect_return(x == 2)
        int x;
        int main(void) {
            x = 1;
            return 0;
        }";

    should_fail_pragma_tests ~label:"One return checking invalid global variable"
        "#pragma expect_return(x == 1)
        int main(void) {
            return 0;
        }";

    should_pass_pragma_tests ~label:"One return checking __return_code__ and global variable"
        "#pragma expect_return(__return_code__ == 0, x == 1)
        int x;
        int main(void) {
            x = 1;
            return 0;
        }";

    should_fail_pragma_tests ~label:"One return checking __return_code__ and global variable but fails"
        "#pragma expect_return(__return_code__ == 0, x == 2)
        int x;
        int main(void) {
            x = 1;
            return 0;
        }";

    should_pass_pragma_tests ~label:"One return checking __return_code__ and two global variables"
        "#pragma expect_return(__return_code__ == 0, x == 1, y == 2)
        int x, y;
        int main(void) {
            x = 1;
            y = 2;
            return 0;
        }";

    should_pass_pragma_tests ~label:"One return checking __return_code__ and comparing two global variables"
        "#pragma expect_return(__return_code__ == 0, x < y)
        int x, y;
        int main(void) {
            x = 1;
            y = 2;
            return 0;
        }";

    should_fail_pragma_tests ~label:"One return checking __return_code__ and comparing two global variables but fails"
        "#pragma expect_return(__return_code__ == 0, x > y)
        int x, y;
        int main(void) {
            x = 1;
            y = 2;
            return 0;
        }";

    should_pass_pragma_tests ~label:"One return checking one global variable shadowed by local variable"
        "#pragma expect_return(x == 1)
        int x = 1;
        int main(void) {
            int x = 2;
            return 0;
        }";

    should_fail_pragma_tests ~label:"One return but expecting two"
        "#pragma expect_return()
        #pragma expect_return()
        int main(void) {
            return 0;
        }";

    should_pass_pragma_tests ~label:"Two return"
        "#pragma expect_return()
        #pragma expect_return()
        int main(void) {
            if (__SYMBOLIC()) return 0;
            return 1;
        }";

    should_pass_pragma_tests ~label:"Two returns checking __return_code__"
        "#pragma expect_return(__return_code__ == 0)
        #pragma expect_return(__return_code__ == 1)
        int main(void) {
            if (__SYMBOLIC()) return 0;
            return 1;
        }";

    should_pass_pragma_tests ~label:"Two returns checking __return_code__ (swapped)"
        "#pragma expect_return(__return_code__ == 1)
        #pragma expect_return(__return_code__ == 0)
        int main(void) {
            if (__SYMBOLIC()) return 0;
            return 1;
        }";

    should_fail_pragma_tests ~label:"Two returns checking __return_code__ but one fails"
        "#pragma expect_return(__return_code__ == 0)
        #pragma expect_return(__return_code__ == 2)
        int main(void) {
            if (__SYMBOLIC()) return 0;
            return 1;
        }";

    should_fail_pragma_tests ~label:"Two returns checking __return_code__ but one fails (swapped)"
        "#pragma expect_return(__return_code__ == 2)
        #pragma expect_return(__return_code__ == 0)
        int main(void) {
            if (__SYMBOLIC()) return 0;
            return 1;
        }";

    should_fail_pragma_tests ~label:"Two returns checking __return_code__ but both fails"
        "#pragma expect_return(__return_code__ == 2)
        #pragma expect_return(__return_code__ == 3)
        int main(void) {
            if (__SYMBOLIC()) return 0;
            return 1;
        }";

    should_pass_pragma_tests ~label:"Two returns, one checking __return_code__, other checking global variable"
        "#pragma expect_return(__return_code__ == 0)
        #pragma expect_return(x == 1)
        int x;
        int main(void) {
            if (__SYMBOLIC()) return 0;
            x = 1;
            return 1;
        }";

    should_pass_pragma_tests ~label:"Two returns, one checking __return_code__, other checking global variable (swapped)"
        "#pragma expect_return(x == 1)
        #pragma expect_return(__return_code__ == 0)
        int x;
        int main(void) {
            if (__SYMBOLIC()) return 0;
            x = 1;
            return 1;
        }";

    should_fail_pragma_tests ~label:"Two returns, one checking __return_code__, other checking global variable, but matching the same path"
        "#pragma expect_return(x == 1)
        #pragma expect_return(__return_code__ == 1)
        int x;
        int main(void) {
            if (__SYMBOLIC()) return 0;
            x = 1;
            return 1;
        }";

    should_fail_pragma_tests ~label:"Two returns, one checking __return_code__, other checking global variable, but matching the same path (swapped)"
        "#pragma expect_return(__return_code__ == 1)
        #pragma expect_return(x == 1)
        int x;
        int main(void) {
            if (__SYMBOLIC()) return 0;
            x = 1;
            return 1;
        }";
]

let expect_exit_testsuite = "expect_exit" >::: [
    should_pass_pragma_tests ~label:"One exit"
        "#pragma expect_exit()
        int main(void) {
            _exit(0);
            return 0;
        }";

    should_fail_pragma_tests ~label:"No parameter list"
        "#pragma expect_exit
        int main(void) {
            _exit(0);
            return 0;
        }";

    should_fail_pragma_tests ~label:"One exit but return"
        "#pragma expect_return()
        int main(void) {
            _exit(0);
            return 0;
        }";

    should_fail_pragma_tests ~label:"One exit but abandoned"
        "#pragma expect_return()
        int main(void) {
            __ASSERT(0);
            exit(0);
            return 0;
        }";

    should_pass_pragma_tests ~label:"One exit checking __exit_code__"
        "#pragma expect_exit(__exit_code__ == 0)
        int main(void) {
            _exit(0);
            return 0;
        }";

    should_fail_pragma_tests ~label:"One exit checking invalid __return_code__"
        "#pragma expect_exit(__return_code__ == 0)
        int main(void) {
            _exit(0);
            return 0;
        }";

    should_pass_pragma_tests ~label:"One exit checking global variable"
        "#pragma expect_exit(x == 1)
        int x;
        int main(void) {
            x = 1;
            _exit(0);
            return 0;
        }";

    should_fail_pragma_tests ~label:"One exit checking global variable but fails"
        "#pragma expect_exit(x == 2)
        int x;
        int main(void) {
            x = 1;
            _exit(0);
            return 0;
        }";

    should_fail_pragma_tests ~label:"One exit checking invalid global variable"
        "#pragma expect_exit(x == 1)
        int main(void) {
            _exit(0);
            return 0;
        }";

    should_pass_pragma_tests ~label:"One exit checking __exit_code__ and global variable"
        "#pragma expect_exit(__exit_code__ == 0, x == 1)
        int x;
        int main(void) {
            x = 1;
            _exit(0);
            return 0;
        }";

    should_fail_pragma_tests ~label:"One exit checking __exit_code__ and global variable but fails"
        "#pragma expect_exit(__exit_code__ == 0, x == 2)
        int x;
        int main(void) {
            x = 1;
            _exit(0);
            return 0;
        }";

    should_pass_pragma_tests ~label:"One exit checking __exit_code__ and two global variables"
        "#pragma expect_exit(__exit_code__ == 0, x == 1, y == 2)
        int x, y;
        int main(void) {
            x = 1;
            y = 2;
            _exit(0);
            return 0;
        }";

    should_pass_pragma_tests ~label:"One exit checking __exit_code__ and comparing two global variables"
        "#pragma expect_exit(__exit_code__ == 0, x < y)
        int x, y;
        int main(void) {
            x = 1;
            y = 2;
            _exit(0);
            return 0;
        }";

    should_fail_pragma_tests ~label:"One exit checking __exit_code__ and comparing two global variables but fails"
        "#pragma expect_exit(__exit_code__ == 0, x > y)
        int x, y;
        int main(void) {
            x = 1;
            y = 2;
            _exit(0);
            return 0;
        }";

    should_pass_pragma_tests ~label:"One exit checking one global variable shadowed by local variable"
        "#pragma expect_exit(x == 1)
        int x = 1;
        int main(void) {
            int x = 2;
            _exit(0);
            return 0;
        }";

    should_fail_pragma_tests ~label:"One exit but expecting two"
        "#pragma expect_exit()
        #pragma expect_exit()
        int main(void) {
            _exit(0);
            return 0;
        }";

    should_pass_pragma_tests ~label:"Two exits"
        "#pragma expect_exit()
        #pragma expect_exit()
        int main(void) {
            if (__SYMBOLIC()) _exit(0);
            _exit(1);
            return 2;
        }";

    should_pass_pragma_tests ~label:"Two exits checking __exit_code__"
        "#pragma expect_exit(__exit_code__ == 0)
        #pragma expect_exit(__exit_code__ == 1)
        int main(void) {
            if (__SYMBOLIC()) _exit(0);
            _exit(1);
            return 2;
        }";

    should_pass_pragma_tests ~label:"Two exits checking __exit_code__ (swapped)"
        "#pragma expect_exit(__exit_code__ == 1)
        #pragma expect_exit(__exit_code__ == 0)
        int main(void) {
            if (__SYMBOLIC()) _exit(0);
            _exit(1);
            return 2;
        }";

    should_fail_pragma_tests ~label:"Two exits checking __exit_code__ but one fails"
        "#pragma expect_exit(__exit_code__ == 0)
        #pragma expect_exit(__exit_code__ == 2)
        int main(void) {
            if (__SYMBOLIC()) _exit(0);
            _exit(1);
            return 2;
        }";

    should_fail_pragma_tests ~label:"Two exits checking __exit_code__ but one fails (swapped)"
        "#pragma expect_exit(__exit_code__ == 2)
        #pragma expect_exit(__exit_code__ == 0)
        int main(void) {
            if (__SYMBOLIC()) _exit(0);
            _exit(1);
            return 2;
        }";

    should_fail_pragma_tests ~label:"Two exits checking __exit_code__ but both fails"
        "#pragma expect_exit(__exit_code__ == 2)
        #pragma expect_exit(__exit_code__ == 3)
        int main(void) {
            if (__SYMBOLIC()) _exit(0);
            _exit(1);
            return 2;
        }";

    should_pass_pragma_tests ~label:"Two exits, one checking __exit_code__, other checking global variable"
        "#pragma expect_exit(__exit_code__ == 0)
        #pragma expect_exit(x == 1)
        int x;
        int main(void) {
            if (__SYMBOLIC()) _exit(0);
            x = 1;
            _exit(1);
            return 2;
        }";

    should_pass_pragma_tests ~label:"Two exits, one checking __exit_code__, other checking global variable (swapped)"
        "#pragma expect_exit(x == 1)
        #pragma expect_exit(__exit_code__ == 0)
        int x;
        int main(void) {
            if (__SYMBOLIC()) _exit(0);
            x = 1;
            _exit(1);
            return 2;
        }";

    should_fail_pragma_tests ~label:"Two exits, one checking __exit_code__, other checking global variable, but matching the same path"
        "#pragma expect_exit(x == 1)
        #pragma expect_exit(__exit_code__ == 1)
        int x;
        int main(void) {
            if (__SYMBOLIC()) _exit(0);
            x = 1;
            _exit(1);
            return 2;
        }";

    should_fail_pragma_tests ~label:"Two exits, one checking __exit_code__, other checking global variable, but matching the same path (swapped)"
        "#pragma expect_exit(__exit_code__ == 1)
        #pragma expect_exit(x == 1)
        int x;
        int main(void) {
            if (__SYMBOLIC()) _exit(0);
            x = 1;
            _exit(1);
            return 2;
        }";
]

let expect_abandoned_testsuite = "expect_abandoned" >::: [
    should_pass_pragma_tests ~label:"One abandoned due to failing assertion"
        "#pragma has_failing_assertions
        #pragma expect_abandoned(\"Assertion was false\")
        int main(void) {
            __ASSERT(0);
            return 0;
        }";

    should_pass_pragma_tests ~label:"One abandoned due to bad dereference"
        "#pragma expect_abandoned(\"Dereference something not an address\")
        int main(void) {
            int *x = 0, y = *x;
            return 0;
        }";

    should_pass_pragma_tests ~label:"One abandoned due to missing function"
        "#pragma expect_abandoned(\"Function .* not found\")
        int main(void) {
            foo();
            return 0;
        }";

    should_fail_pragma_tests ~label:"No parameter list"
        "#pragma expect_abandoned
        int main(void) {
            foo();
            return 0;
        }";

    should_fail_pragma_tests ~label:"Empty parameter list"
        "#pragma expect_abandoned()
        int main(void) {
            foo();
            return 0;
        }";

    should_fail_pragma_tests ~label:"One abandoned but exit"
        "#pragma expect_abandoned(\"\")
        int main(void) {
            _exit(0);
            return 0;
        }";

    should_fail_pragma_tests ~label:"One abandoned but return"
        "#pragma expect_abandoned(\"\")
        int main(void) {
            return 0;
        }";

    should_fail_pragma_tests ~label:"One abandoned checking invalid __return_code__"
        "#pragma expect_abandoned(\"\", __return_code__ == 0)
        int main(void) {
            foo();
            return 0;
        }";

    should_fail_pragma_tests ~label:"One abandoned checking invalid __exit_code__"
        "#pragma expect_abandoned(\"\", __exit_code__ == 0)
        int main(void) {
            foo();
            _exit(0);
            return 0;
        }";

    should_pass_pragma_tests ~label:"One abandoned checking global variable"
        "#pragma expect_abandoned(\"\", x == 1)
        int x;
        int main(void) {
            x = 1;
            foo();
            return 0;
        }";

    should_fail_pragma_tests ~label:"One abandoned checking global variable but fails"
        "#pragma expect_abandoned(\"\", x == 2)
        int x;
        int main(void) {
            x = 1;
            foo();
            return 0;
        }";

    should_fail_pragma_tests ~label:"One abandoned checking invalid global variable"
        "#pragma expect_abandoned(\"\", x == 1)
        int main(void) {
            foo();
            return 0;
        }";

    should_pass_pragma_tests ~label:"One abandoned checking two global variables"
        "#pragma expect_abandoned(\"\", x == 1, y == 2)
        int x, y;
        int main(void) {
            x = 1;
            y = 2;
            foo();
            return 0;
        }";

    should_pass_pragma_tests ~label:"One abandoned comparing two global variables"
        "#pragma expect_abandoned(\"\", x < y)
        int x, y;
        int main(void) {
            x = 1;
            y = 2;
            foo();
            return 0;
        }";

    should_fail_pragma_tests ~label:"One abandoned comparing two global variables but fails"
        "#pragma expect_abandoned(\"\", x > y)
        int x, y;
        int main(void) {
            x = 1;
            y = 2;
            foo(0);
            return 0;
        }";

    should_pass_pragma_tests ~label:"One abandoned checking one global variable shadowed by local variable"
        "#pragma expect_abandoned(\"\", x == 1)
        int x = 1;
        int main(void) {
            int x = 2;
            foo(0);
            return 0;
        }";

    should_fail_pragma_tests ~label:"One abandoned but expecting two"
        "#pragma expect_abandoned(\"\")
        #pragma expect_abandoned(\"\")
        int main(void) {
            foo();
            return 0;
        }";

    should_pass_pragma_tests ~label:"Two abandoned"
        "#pragma expect_abandoned(\"\")
        #pragma expect_abandoned(\"\")
        int main(void) {
            if (__SYMBOLIC()) foo();
            foo();
            return 2;
        }";

    should_pass_pragma_tests ~label:"Two abandoned checking global variable"
        "#pragma expect_abandoned(\"\", x == 0)
        #pragma expect_abandoned(\"\", x == 1)
        int x;
        int main(void) {
            if (__SYMBOLIC()) x = 0;
            else x = 1;
            foo();
            return 2;
        }";

    should_pass_pragma_tests ~label:"Two abandoned checking global variable (swapped)"
        "#pragma expect_abandoned(\"\", x == 1)
        #pragma expect_abandoned(\"\", x == 0)
        int x;
        int main(void) {
            if (__SYMBOLIC()) x = 0;
            else x = 1;
            foo();
            return 2;
        }";

    should_fail_pragma_tests ~label:"Two abandoned checking global variable but one fails"
        "#pragma expect_abandoned(\"\", x == 0)
        #pragma expect_abandoned(\"\", x == 2)
        int x;
        int main(void) {
            if (__SYMBOLIC()) x = 0;
            else x = 1;
            foo();
            return 2;
        }";

    should_fail_pragma_tests ~label:"Two abandoned checking global variable but one fails (swapped)"
        "#pragma expect_abandoned(x == 2)
        #pragma expect_abandoned(x == 0)
        int x;
        int main(void) {
            if (__SYMBOLIC()) x = 0;
            else x = 1;
            foo();
            return 2;
        }";

    should_fail_pragma_tests ~label:"Two abandoned checking global variable but both fails"
        "#pragma expect_abandoned(x == 2)
        #pragma expect_abandoned(x == 3)
        int x;
        int main(void) {
            if (__SYMBOLIC()) x = 0;
            else x = 1;
            foo();
            return 2;
        }";

    should_fail_pragma_tests ~label:"Two exits, checking different global variables, but matching the same path"
        "#pragma expect_abandoned(x == 0)
        #pragma expect_abandoned(y == 1)
        int x, y;
        int main(void) {
            if (__SYMBOLIC()) { x = 0; y = 1; }
            else { x = 2; y = 3; }
            foo();
            return 4;
        }";

    should_fail_pragma_tests ~label:"Two exits, checking different global variables, but matching the same path (swapped)"
        "#pragma expect_abandoned(y == 1)
        #pragma expect_abandoned(x == 0)
        int x, y;
        int main(void) {
            if (__SYMBOLIC()) { x = 0; y = 1; }
            else { x = 2; y = 3; }
            foo();
            return 4;
        }";
]

let no_other_return_testsuite = "no_other_return" >::: [
    should_pass_pragma_tests ~label:"No returns"
        "#pragma no_other_return
        int main(void) {
            _exit(0);
            return 0;
        }";

    should_pass_pragma_tests ~label:"One return only"
        "#pragma expect_return()
        #pragma no_other_return
        int main(void) {
            return 0;
        }";

    should_fail_pragma_tests ~label:"One return but got two"
        "#pragma expect_return()
        #pragma no_other_return
        int main(void) {
            if (__SYMBOLIC()) return 1;
            return 0;
        }";

    should_fail_pragma_tests ~label:"Expected no other return before one return"
        "#pragma no_other_return
        #pragma expect_return()
        int main(void) {
            return 0;
        }";

    should_pass_pragma_tests ~label:"Two return only"
        "#pragma expect_return()
        #pragma expect_return()
        #pragma no_other_return
        int main(void) {
            if (__SYMBOLIC()) return 1;
            return 0;
        }";
]

let no_other_exit_testsuite = "no_other_exit" >::: [
    should_pass_pragma_tests ~label:"No exits"
        "#pragma no_other_exit
        int main(void) {
            return 0;
        }";

    should_pass_pragma_tests ~label:"One exit only"
        "#pragma expect_exit()
        #pragma no_other_exit
        int main(void) {
            _exit(0);
            return 0;
        }";

    should_fail_pragma_tests ~label:"One exit but got two"
        "#pragma expect_exit()
        #pragma no_other_exit
        int main(void) {
            if (__SYMBOLIC()) _exit(0);
            _exit(1);
            return 2;
        }";

    should_fail_pragma_tests ~label:"Expected no other exit before one exit"
        "#pragma no_other_exit
        #pragma expect_exit()
        int main(void) {
            _exit(0);
            return 1;
        }";

    should_pass_pragma_tests ~label:"Two exits only"
        "#pragma expect_exit()
        #pragma expect_exit()
        #pragma no_other_exit
        int main(void) {
            if (__SYMBOLIC()) _exit(0);
            _exit(1);
            return 2;
        }";
]

let no_other_abandoned_testsuite = "no_other_abandoned" >::: [
    should_pass_pragma_tests ~label:"No abandoned"
        "#pragma no_other_abandoned
        int main(void) {
            return 0;
        }";

    should_pass_pragma_tests ~label:"One abandoned only"
        "#pragma expect_abandoned(\"\")
        #pragma no_other_abandoned
        int main(void) {
            foo();
            return 0;
        }";

    should_fail_pragma_tests ~label:"One abandoned but got two"
        "#pragma expect_abandoned(\"\")
        #pragma no_other_abandoned
        int main(void) {
            if (__SYMBOLIC()) foo();
            foo();
            return 0;
        }";

    should_fail_pragma_tests ~label:"Expected no other abandoned before one abandoned"
        "#pragma no_other_abandoned
        #pragma expect_abandoned(\"\")
        int main(void) {
            foo();
            return 0;
        }";

    should_pass_pragma_tests ~label:"Two abandoned only"
        "#pragma expect_abandoned(\"\")
        #pragma expect_abandoned(\"\")
        #pragma no_other_abandoned
        int main(void) {
            if (__SYMBOLIC()) foo();
            foo();
            return 0;
        }";
]

let no_other_results_testsuite = "no_other_results" >::: [
    should_fail_pragma_tests ~label:"No results"
        "#pragma no_other_results
        int main(void) {
            return 0;
        }";

    should_pass_pragma_tests ~label:"One return only"
        "#pragma expect_return()
        #pragma no_other_results
        int main(void) {
            return 0;
        }";

    should_pass_pragma_tests ~label:"One exit only"
        "#pragma expect_exit()
        #pragma no_other_results
        int main(void) {
            _exit(0);
            return 1;
        }";

    should_pass_pragma_tests ~label:"One abandoned only"
        "#pragma expect_abandoned(\"\")
        #pragma no_other_results
        int main(void) {
            foo();
            return 0;
        }";

    should_fail_pragma_tests ~label:"One return but got two"
        "#pragma expect_return()
        #pragma no_other_results
        int main(void) {
            if (__SYMBOLIC()) return 1;
            return 0;
        }";

    should_fail_pragma_tests ~label:"One exit but got two"
        "#pragma expect_exit()
        #pragma no_other_results
        int main(void) {
            if (__SYMBOLIC()) _exit(0);
            _exit(1);
            return 2;
        }";

    should_fail_pragma_tests ~label:"One abandoned but got two"
        "#pragma expect_abandoned(\"\")
        #pragma no_other_results
        int main(void) {
            if (__SYMBOLIC()) foo();
            foo();
            return 0;
        }";

    should_fail_pragma_tests ~label:"Expected no other results before one exit"
        "#pragma no_other_results
        #pragma expect_return()
        int main(void) {
            return 0;
        }";

    should_fail_pragma_tests ~label:"Expected no other results before one exit"
        "#pragma no_other_results
        #pragma expect_exit()
        int main(void) {
            _exit(0);
            return 1;
        }";

    should_fail_pragma_tests ~label:"Expected no other results before one abandoned"
        "#pragma no_other_results
        #pragma expect_abandoned(\"\")
        int main(void) {
            foo();
            return 0;
        }";

    should_pass_pragma_tests ~label:"One return, one exit only"
        "#pragma expect_return()
        #pragma expect_exit()
        #pragma no_other_results
        int main(void) {
            if (__SYMBOLIC()) _exit(0);
            return 1;
        }";

    should_pass_pragma_tests ~label:"One exit, one abandoned only"
        "#pragma expect_exit()
        #pragma expect_abandoned(\"\")
        #pragma no_other_results
        int main(void) {
            if (__SYMBOLIC()) _exit(0);
            foo();
            return 1;
        }";

    should_pass_pragma_tests ~label:"One return, one abandoned only"
        "#pragma expect_return()
        #pragma expect_abandoned(\"\")
        #pragma no_other_results
        int main(void) {
            if (__SYMBOLIC()) foo();
            return 0;
        }";

    should_pass_pragma_tests ~label:"One return, one exit, one abandoned only"
        "#pragma expect_return()
        #pragma expect_exit()
        #pragma expect_abandoned(\"\")
        #pragma no_other_results
        int main(void) {
            if (__SYMBOLIC()) foo();
            if (__SYMBOLIC()) _exit(0);
            return 1;
        }";
]

let testsuite = "OtterPragmaTests" >::: [
    has_failing_assertions_testsuite;
    command_line_testsuite;
    expect_return_testsuite;
    expect_exit_testsuite;
    expect_abandoned_testsuite;
    no_other_return_testsuite;
    no_other_exit_testsuite;
    no_other_abandoned_testsuite;
    no_other_results_testsuite;
]

