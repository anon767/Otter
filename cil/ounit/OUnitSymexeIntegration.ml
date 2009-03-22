open MyOUnit
open Executeargs

(* directory containing tests for OUnitSymexeIntegration *)
let dir_prefix = Filename.concat "ounit" "OUnitSymexeIntegration"

(* test helper that runs the symbolic executor on a file relative to dir_prefix *)
let test_file path =
    path >:: fun () ->
        (* Suppress all output from the symbolic executor *)
        print_args.arg_print_nothing <- true;

        (* reset error flag *)
        Errormsg.hadErrors := false;
        let file = Frontc.parse (Filename.concat dir_prefix path) () in
        assert_bool "Cil parse error" (not !Errormsg.hadErrors);

        (* finally run the symbolic executor *)
        Executemain.doExecute file;

        (* test that no assertions failed *)
        assert_string (Executedebug.get_log ())


(*
 * OUnit test suite
 *)

let testsuite = "OUnitSymexeIntegration" >:
    let rec test_dir dirname =
        let dir = Sys.readdir (Filename.concat dir_prefix dirname) in
        (* first, sort up *)
        Array.sort String.compare dir;
        (* then, iterate right-to-left, so output list will stay sorted up *)
        let tests = Array.fold_right begin fun filename tests ->
            if filename.[0] = '.' then (* skip hidden files *)
                tests
            else begin
                let path = Filename.concat dirname filename in
                if Sys.is_directory (Filename.concat dir_prefix path) then
                    (test_dir path)::tests (* recurse into directories *)
                else
                    (test_file path)::tests (* add files *)
            end
        end dir [] in
        TestList tests
    in test_dir ""

