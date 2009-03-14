open MyOUnit

(* integration test helper: given a source code as a string, generate a C file and run Cil on it *)
let test_file content ?(label=content) test =
    label >:: bracket begin fun () ->
        let filename, fileout = Filename.open_temp_file "ounit_cil_integration." ".c" in
        output_string fileout content;
        close_out fileout;
        filename
    end begin fun filename ->
        Errormsg.hadErrors := false;
        let file = Frontc.parse filename () in
        assert_bool "Cil parse error" (not !Errormsg.hadErrors);

        assert_logf "@[<v>";
        (* TODO: setup and log some initial state here *)
        (* TODO: run analysis here *)
        (* TODO: log some end state here *)
        assert_logf "@]";

        (* finally run the test *)
        test file (* TODO: also pass some result to check *)
    end begin fun filename ->
        Unix.unlink filename
    end


let simple_testsuite = "Simple" >::: [
    (* TODO: put some tests here *)
]

let testsuite = "Integration" >::: [
    simple_testsuite;
]