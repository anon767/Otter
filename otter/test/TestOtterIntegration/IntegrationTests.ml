open TestUtil.MyOUnit
open Otter
open Executeargs

(* directory containing tests for IntegrationTests *)
let dir_prefix = Filename.concat "test" "TestOtterIntegration"

(* test helper that runs the symbolic executor on all files relative to dir_prefix/dir *)
let test_integration main_loop get_file dir =
    dir >:
        let fulldir = Filename.concat dir_prefix dir in
        test_dir fulldir begin fun path ->
            path >:: fun () ->
                (* Suppress all output from the symbolic executor *)
                print_args.arg_print_nothing <- true;

                (* reset error flag *)
                Errormsg.hadErrors := false;
                let file = get_file (Filename.concat fulldir path) in
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
        end

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
	test_integration Driver.init get_file "OtterCore";
	test_integration Multiprocess.init get_file "MultiprocessOtter";
	test_integration Driver.init_with_libc get_file_with_preprocess "PreprocessedOtter";
]

