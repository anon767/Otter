open MyOUnit
open Bytes
open Types

(* test helper that runs the symbolic executor on a file given a source code as a string *)
let test_file_job cmdline content ?(label=content) test =
    label >:: bracket begin fun () ->
        let filename, fileout = Filename.open_temp_file "OUnitSymexeFileJobs." ".c" in
        output_string fileout content;
        close_out fileout;
        filename
    end begin fun filename ->
        (* Suppress all output from the symbolic executor *)
        Executeargs.print_args.Executeargs.arg_print_nothing <- true;

        (* reset error flag *)
        Errormsg.hadErrors := false;
        let file = Frontc.parse filename () in
        assert_bool "Cil parse error" (not !Errormsg.hadErrors);

        (* prepare the file and run the symbolic executor *)
        Executemain.prepare_file file;
        let job = Executemain.job_for_file file cmdline in
        let results = Driver.main_loop job in

        (* test that no assertions failed *)
        assert_string (Executedebug.get_log ());

        (* finally run the test *)
        test file results
    end begin fun filename ->
        Unix.unlink filename
    end


(*
 * OUnit test suite
 *)

let command_line_testsuite = "Command Line" >::: [
    test_file_job ~label:"No arguments"
    ["a"] "
        int main(int argc, char *argv[]) {
            __ASSERT(argc == 1);
            __ASSERT(argv[0][0] == 'a');
            return 0;
        }
    " begin fun file results ->
        ()
    end;

    test_file_job ~label:"One argument"
    ["a"; "b"] "
        int main(int argc, char *argv[]) {
            __ASSERT(argc == 2);
            __ASSERT(argv[0][0] == 'a');
            __ASSERT(argv[1][0] == 'b');
            return 0;
        }
    " begin fun file results ->
        ()
    end;
]

let exit_code_testsuite = "Exit Code" >::: [
    test_file_job ~label:"return 0 from main"
    ["a"] "
        int main(void) {
            return 0;
        }
    " begin fun file results ->
        assert_match begin fun [ Return (Some actual, _) ] ->
            assert_equal ~cmp:same_bytes bytes__zero actual
        end results
    end;

    test_file_job ~label:"exit(0) from main"
    ["a"] "
        int main(void) {
            exit(0);
            return 1;
        }
    " begin fun file results ->
        assert_match begin fun [ Exit (Some actual, _) ] ->
            assert_equal ~cmp:same_bytes bytes__zero actual
        end results
    end;

    test_file_job ~label:"exit() from main"
    ["a"] "
        int main(void) {
            exit();
            return 1;
        }
    " begin fun file results ->
        assert_match begin fun [ Exit (None, _) ] ->
            ()
        end results
    end;
]

let testsuite = "OUnitSymexeFileJobs" >::: [
    command_line_testsuite;
    exit_code_testsuite;
]

