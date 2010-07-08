open TestUtil.MyOUnit
open Otter
open Executeargs

(* directory containing tests for IntegrationTests *)
let dir_prefix = Filename.concat "test" "TestOtterIntegration"

(* test helper that runs the symbolic executor on a file relative to dir_prefix *)
let test_file main_loop path =
    path >:: fun () ->
        (* Suppress all output from the symbolic executor *)
        print_args.arg_print_nothing <- true;

        (* reset error flag *)
        Errormsg.hadErrors := false;
        let file = Frontc.parse (Filename.concat dir_prefix path) () in
        assert_bool "Cil parse error" (not !Errormsg.hadErrors);

        (* run the symbolic executor *)
        Executemain.prepare_file file;
        let job = Executemain.job_for_file file ["Integration"] in
        let results = main_loop job in

        (* count jobs that were abandoned *)
        let abandoned = List.fold_left begin fun abandoned result -> match result with
			| Types.Abandoned (s, loc, _) -> (loc.Cil.file, loc.Cil.line, s)::abandoned
			| _ -> abandoned
		end [] results in
		if abandoned <> [] then begin
			let printer ff abandoned = ignore begin List.fold_left begin fun b (f, l, s) ->
				Format.fprintf ff "%(%)@[%s:%d: %s@]" b f l s; "@\n"
			end "" abandoned end in
			assert_failure "@[<hv2>Abandoned paths:@\n%a@]" printer abandoned
		end;

        (* test that no assertions failed *)
        assert_string (Executedebug.get_log ())

(* test helper that runs the symbolic executor on all files recursively in a directory relative to dir_prefix *)
let rec test_dir main_loop dirname =
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
				(test_dir main_loop path)::tests (* recurse into directories *)
			else
				(test_file main_loop path)::tests (* add files *)
		end
	end dir [] in
	TestList tests


(*
 * OUnit test suite
 *)

let testsuite = "Integration" >::: [
	"Otter Core" >: test_dir Driver.main_loop "OtterCore"
]

