open MyOUnit
open Executeargs

(* Suppress all output from the symbolic executor *)
let _ = print_args.arg_print_nothing <- true

(* integration test helper: given a source code as a string, generate a C file and run Cil on it *)
let test_file ?label content (*test*) =
		let label = (match label with Some x -> x | _ -> content) in
    label >:: bracket begin fun () ->
        let filename, fileout = Filename.open_temp_file "ounit_cil_integration." ".c" in
        output_string fileout ("extern void __ASSERT();\nint main (){" ^ content ^ "return 0;}\n");
        close_out fileout;
        filename
    end begin fun filename ->
        Errormsg.hadErrors := false;
        let file = Frontc.parse filename () in
        assert_bool "Cil parse error" (not !Errormsg.hadErrors);
				Cilly.makeCFGFeature.Cil.fd_doit file;
				Cilutil.makeCFG := true; (* doExecute checks that this is set *)
(*
        assert_log "@[<v>";
        (* TODO: setup and log some initial state here *)
        (* TODO: run analysis here *)
        (* TODO: log some end state here *)
        assert_log "@]";
*)
        (* finally run the test *)
				Executemain.doExecute file;
				assert_string (Executedebug.get_log ())
(*        test file (* TODO: also pass some result to check *)*)
    end begin fun filename ->
        Unix.unlink filename
    end


let simple_testsuite = "Simple" >::: [
	test_file ~label:"trivial assignment" "
	int x = 10;
	x = x;
	__ASSERT(x==10);
";
	test_file ~label:"failing test" "
	int x = 10;
	x = x;
	__ASSERT(x==1);
"
]

let testsuite = "Integration" >::: [
    simple_testsuite;
]