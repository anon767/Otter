open TestUtil.MyOUnit
open TestUtil.OtterUtil
open Otter
open Bytes
open Types


(*
 * OUnit test suite
 *)

let command_line_testsuite = "Command Line" >::: [
    test_otter
        ~label:"No arguments"
        ~command_line:["a"]
        "int main(int argc, char *argv[]) {
            __ASSERT(argc == 1);
            __ASSERT(argv[0][0] == 'a');
            return 0;
        }"
        begin fun file results ->
            ()
        end;

    test_otter
        ~label:"One argument"
        ~command_line:["a"; "b"]
        "int main(int argc, char *argv[]) {
            __ASSERT(argc == 2);
            __ASSERT(argv[0][0] == 'a');
            __ASSERT(argv[1][0] == 'b');
            return 0;
        }"
        begin fun file results ->
            ()
        end;
]

let exit_code_testsuite = "Exit Code" >::: [
    test_otter
        ~label:"return 0 from main"
        "int main(void) {
            return 0;
        }"
        begin fun file results ->
            assert_match begin fun [ Return (Some actual, _) ] ->
                assert_equal ~cmp:bytes__equal bytes__zero actual
            end results
        end;

    test_otter
        ~label:"exit(0) from main"
        "int main(void) {
            exit(0);
            return 1;
        }"
        begin fun file results ->
            assert_match begin fun [ Exit (Some actual, _) ] ->
                assert_equal ~cmp:bytes__equal bytes__zero actual
            end results
        end;

    test_otter
        ~label:"exit() from main"
        "int main(void) {
            exit();
            return 1;
        }"
        begin fun file results ->
            assert_match begin fun [ Exit (None, _) ] ->
                ()
            end results
        end;
]

let testsuite = "FileJobs" >::: [
    command_line_testsuite;
    exit_code_testsuite;
]

