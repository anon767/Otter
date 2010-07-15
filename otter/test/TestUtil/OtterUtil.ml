open MyOUnit
open Otter

(* test helper that runs the symbolic executor on a file given a source code as a string *)
let test_otter
        content
        ?(label=content)
        ?(setup=(fun _ -> ()))
        ?(main_loop=Driver.init)
        ?(command_line=[])
        ?(has_failing_assertions=false)
        test =
    label >:: test_string_as_file "OtterTest." ".c" content begin fun filename ->
        (* reset the error flag and suppress all output from the symbolic executor *)
        Errormsg.hadErrors := false;
        Executeargs.print_args.Executeargs.arg_print_nothing <- true;

        (* additional setup before parsing *)
        setup filename;

        (* parse and ensure no errors *)
        let file = Frontc.parse filename () in
        assert_bool "Cil parse error" (not !Errormsg.hadErrors);

        (* prepare the file and run the symbolic executor *)
        Executemain.prepare_file file;
        let job = Executemain.job_for_file file command_line in
        let results = main_loop job in

        (* perform tests in order of expressiveness of potential errors *)
        (* first, test if assertions passed *)
        let log = Executedebug.get_log () in
        if not has_failing_assertions then
            assert_string log;

        (* then, run the given test *)
        test file results;

        (* finally, test if assertions passed *)
        if has_failing_assertions then
            assert_bool "Expected some failing assertions but got none." (log <> "");
    end
