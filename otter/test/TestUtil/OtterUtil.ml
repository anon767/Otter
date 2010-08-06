(** Utilities for testing Otter. *)

open MyOUnit
open Otter


(** Test helper that runs Otter on a file.
            @param path is the path to the file
            @param setup is an optional setup function to be called before parsing
            @param main_loop is the Otter main loop to use (default: {!Driver.init})
            @param command_line is an optional command line to provide to the executed file
            @param has_failing_assertions indicates whether failing assertions are expected (default: [false])
            @param test is the test to apply to the result from Otter
            @return a {!TestCase} that runs Otter
*)
let test_otter_on_file
        path
        ?(setup=(fun _ -> ()))
        ?(main_loop=Driver.init)
        ?(command_line=[])
        ?(has_failing_assertions=false)
        (test : Types.job_completion list -> unit) =
    fun () ->
        (* reset the error flag and suppress all output from the symbolic executor *)
        Errormsg.hadErrors := false;
        Output.arg_print_nothing := true;

        (* additional setup before parsing *)
        setup path;

        (* parse and ensure no errors *)
        let file = Frontc.parse path () in
        assert_bool "Cil parse error" (not !Errormsg.hadErrors);

        (* prepare the file and run the symbolic executor *)
        Driver.prepare_file file;
        let job = Driver.job_for_file file command_line in
        let results = main_loop job in

        (* perform tests in order of expressiveness of potential errors *)
        (* first, test if assertions passed *)
        let log = Executedebug.get_log () in
        if not has_failing_assertions then
            assert_string log;

        (* then, run the given test *)
        test results;

        (* finally, test if assertions passed *)
        if has_failing_assertions then
            assert_bool "Expected some failing assertions but got none." (log <> "")


(** As with {!test_otter_on_file}, but on a source code given as a string, with the same argument defaults.
            @param code is the source code
            @param label is the label for the test (default: [code])
            @param setup is an optional setup function to be called before parsing
            @param main_loop is the Otter main loop to use
            @param command_line is an optional command line to provide to the executed file
            @param has_failing_assertions indicates whether failing assertions are expected
            @param test is the test to apply to the result from Otter
            @return a {!TestCase} that runs Otter
*)
let test_otter
        code
        ?(label=code)
        ?setup
        ?main_loop
        ?command_line
        ?has_failing_assertions
        test =
    label >:: test_string_as_file "OtterTest." ".c" code begin fun filename ->
        test_otter_on_file filename ?setup ?main_loop ?command_line ?has_failing_assertions test ()
    end


(** As with {!test_otter}, but sets [main_loop] to call only {!Core.step} and no interceptors.
            @param code is the source code
            @param label is the label for the test (default: [code])
            @param setup is an optional setup function to be called before parsing
            @param command_line is an optional command line to provide to the executed file
            @param has_failing_assertions indicates whether failing assertions are expected
            @param test is the test to apply to the result from Otter
            @return a {!MyOUnit.TestCase} that runs Otter
*)
let test_otter_core = test_otter ~main_loop:(fun job -> Driver.main_loop Driver.get_job_list Core.step Driver.process_result [job])

