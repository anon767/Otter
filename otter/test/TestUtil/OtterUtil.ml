(** Utilities for testing Otter. *)

open MyOUnit
open OcamlUtilities
open CilUtilities
open OtterCore
open OtterJob
open OtterReporter
open OtterDriver


(** Test helper that first runs [otter.pl] on a file and passes the preprocessed file to another test. The
    preprocessed file will be removed after the test completes.
            @param path is the path to the file to preprocess
            @param test is the test to apply to the preprocessed file
            @return a {!TestCase} that preprocesses the file
*)
let test_with_preprocessed_file path test =
    test_with_temp_file "OtterTest." ".i" begin fun (temp_path, temp_out) ->
        let pid, err_in =
            (* create a pipe to capture stderr *)
            let err_fdin, err_fdout = Unix.pipe () in
            try
                (* TODO: add standard search paths to otter.pl, rather than hard-coding it here *)
                (* launch the preprocessor, redirecting the output to the temporary file *)
                let pid = Unix.create_process
                    "./otter.pl" [| "./otter.pl"; "-nostdinc"; "-isystem"; "./libc"; "-include"; "./libc/__otter/all.h"; "-E"; path |]
                    Unix.stdin (Unix.descr_of_out_channel temp_out) err_fdout
                in
                (* make sure to not leak and run out of file descriptors *)
                Unix.close err_fdout;
                (pid, Unix.in_channel_of_descr err_fdin)
            with e ->
                Unix.close err_fdin;
                Unix.close err_fdout;
                raise e
        in

        (* capture the stderr until the preprocessor exits *)
        try
            begin
                let buffer = Buffer.create 4096 in
                try
                    while true do
                        Buffer.add_string buffer (input_line err_in);
                        Buffer.add_char buffer '\n'
                    done
                with End_of_file ->
                    assert_log "%s@." (Buffer.contents buffer)
            end;

            close_in err_in;
            let _, status = Unix.waitpid [] pid in
            match status with
                | Unix.WEXITED 0 ->
                    let () = test temp_path () in ()
                | Unix.WEXITED i ->
                    assert_failure "Preprocesser exited with error code %d.@." i
                | Unix.WSIGNALED i ->
                    assert_failure "Preprocesser killed with signal %d.@." i
                | Unix.WSTOPPED i ->
                    (* this should never occur since waitpid wasn't given the WUNTRACED flag *)
                    assert_failure "Preprocesser stopped with signal %d.@." i
        with e ->
            close_in err_in;
            raise e
    end

(** Test helper that runs Otter on a file.
            @param path is the path to the file
            @param setup is an optional setup function to be called before parsing
            @param driver is the Otter main loop to use (default: {!Driver.run_basic})
            @param entry_function is the function at which to begin symbolic execution; if not "main", pointers will
                    be initialized via {!SymbolicPointers.job_for_middle} (default: ["main"])
            @param command_line is an optional command line to provide to the executed file; ignored if
                    [entry_function] is not "main"
            @param has_failing_assertions indicates whether failing assertions are expected (default: [false])
            @param test is the test to apply to the result from Otter
            @return a {!TestCase} that runs Otter
*)
let test_otter_on_file
        path
        ?(setup=(fun _ -> ()))
        ?(driver=Driver.run_basic)
        ?(entry_function="main")
        ?(command_line=[])
        ?(has_failing_assertions=false)
        test =
    fun () ->
        (* reset the error flag and suppress all output from the symbolic executor *)
        Errormsg.hadErrors := false;
        Output.arg_print_mute := 1;

        (* additional setup before parsing *)
        setup path;

        (* parse and ensure no errors *)
        let file = Frontc.parse path () in
        assert_bool "Cil parse error" (not !Errormsg.hadErrors);

        (* prepare the file and run the symbolic executor *)
        Core.prepare_file file;
        let job =
            if entry_function = "main" then
                FileJob.make file command_line
            else
                FunctionJob.make file (FindCil.fundec_by_name file entry_function)
        in
        let results = driver (new BasicReporter.t ()) job in

        (* perform tests in order of expressiveness of potential errors *)
        (* first, test if assertions passed *)
        let log = Executedebug.get_log () in
        if not has_failing_assertions then
            assert_string log;

        (* then, run the given test *)
        let () = test results#completed in

        (* finally, test if assertions passed *)
        if has_failing_assertions then
            assert_bool "Expected some failing assertions but got none." (log <> "")


(** As with {!test_otter_on_file}, but on a source code given as a string, with the same argument defaults.
            @param code is the source code
            @param label is the label for the test (default: [code])
            @param setup is an optional setup function to be called before parsing
            @param driver is the Otter main loop to use
            @param entry_function is the function at which to begin symbolic execution
            @param command_line is an optional command line to provide to the executed file
            @param has_failing_assertions indicates whether failing assertions are expected
            @param test is the test to apply to the result from Otter
            @return a {!TestCase} that runs Otter
*)
let test_otter
        code
        ?(label=code)
        ?setup
        ?driver
        ?entry_function
        ?command_line
        ?has_failing_assertions
        test =
    label >:: test_string_as_file "OtterTest." ".i" code begin fun filename ->
        test_otter_on_file filename ?setup ?driver ?entry_function ?command_line ?has_failing_assertions test ()
    end


(** As with {!test_otter}, but sets [driver] to call only {!Statement.step} and no interceptors.
            @param code is the source code
            @param label is the label for the test (default: [code])
            @param setup is an optional setup function to be called before parsing
            @param entry_function is the function at which to begin symbolic execution
            @param command_line is an optional command line to provide to the executed file
            @param has_failing_assertions indicates whether failing assertions are expected
            @param test is the test to apply to the result from Otter
            @return a {!MyOUnit.TestCase} that runs Otter
*)
let test_otter_core = test_otter ~driver:Driver.run_core

