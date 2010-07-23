open TestUtil.MyOUnit
open Otter
open Executeargs

(* directory containing tests for IntegrationTests *)
let dir_prefix = Filename.concat "test" "TestOtterIntegration"

(* test helper that runs the symbolic executor on a file relative to dir_prefix *)
let test_file main_loop get_file path =
    path >:: fun () ->
        (* Suppress all output from the symbolic executor *)
        print_args.arg_print_nothing <- true;

        (* reset error flag *)
        Errormsg.hadErrors := false;
        let file = get_file (Filename.concat dir_prefix path) in
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
let rec test_dir main_loop get_file dirname =
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
				(test_dir main_loop get_file path)::tests (* recurse into directories *)
			else
				(test_file main_loop get_file path)::tests (* add files *)
		end
	end dir [] in
	TestList tests


let get_file = (fun path -> Frontc.parse path ())
let get_file_with_preprocess = 
	(fun path -> 
		let path2 = path^".pp.c" in
		if (Sys.command ("./otter.pl -nostdlib -I./libc/ -include./libc/__otter/all.h -E -o"^path2^" "^path^" 2>/dev/null")) == 0 then
			let file = Frontc.parse path2 () in
			let _ = Sys.command ("rm "^path2) in
			file
		else
			let _ = Sys.command ("rm "^path2) in
			assert_failure "Preprocessor parse error."
		
	)

(*
 * OUnit test suite
 *)

let testsuite = "Integration" >::: [
	test_dir Driver.init get_file "OtterCore";
	test_dir Multiprocess.init get_file "MultiprocessOtter";
	test_dir Driver.init_with_libc get_file_with_preprocess "PreprocessedOtter";
]

