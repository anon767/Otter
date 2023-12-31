open TestUtil.MyOUnit
open TestUtil.OtterUtil
open OtterBytes
open OtterCore
open Bytes
open State


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
        begin fun results ->
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
        begin fun results ->
            ()
        end;
]

let exit_code_testsuite = "Exit Code" >::: [
    test_otter
        ~label:"return 0 from main"
        "int main(void) {
            return 0;
        }"
        begin fun results ->
            assert_match begin fun [ (Job.Return (Some actual), _) ] ->
                assert_equal ~eq:bytes__equal bytes__zero actual
            end results
        end;

    test_otter
        ~label:"exit(0) from main"
        "int main(void) {
            _exit(0);
            return 1;
        }"
        begin fun results ->
            assert_match begin fun [ (Job.Exit (Some actual), _) ] ->
                assert_equal ~eq:bytes__equal bytes__zero actual
            end results
        end;

    test_otter
        ~label:"exit() from main"
        "int main(void) {
            _exit();
            return 1;
        }"
        begin fun results ->
            assert_match begin fun [ (Job.Exit None, _) ] ->
                ()
            end results
        end;
]

let testsuite = "FileJobs" >::: [
    command_line_testsuite;
    exit_code_testsuite;
]

