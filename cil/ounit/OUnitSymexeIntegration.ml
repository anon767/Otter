open MyOUnit
open Executeargs

(* Suppress all output from the symbolic executor *)
let _ = print_args.arg_print_nothing <- true

(* integration test helper: given a file name, run Cil on the file *)
let test_file filename =
    filename >:: bracket begin fun () ->
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
        ()
    end

let filesFromDir dirname =
	let fileList = ref [] in
	Array.iter
		(fun name ->
			let file = Filename.concat dirname name in
			if (Unix.stat file).Unix.st_kind == Unix.S_REG
			then fileList := file :: !fileList)
		(Sys.readdir dirname);
	!fileList

let simple_testsuite = "Simple" >:::
	List.map test_file (filesFromDir "ounitTests")

let testsuite = "Integration" >::: [
    simple_testsuite;
]