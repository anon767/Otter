open TestUtil.MyOUnit
open TestUtil.OtterPragmaTests
open OtterCore


(* directory containing tests for SystemTests *)
let dir_prefix = Filename.concat "test" "TestOtterSystem"


(* test helper that runs the symbolic executor on all files relative to dir_prefix/dir *)
let test_integration main_loop dir =
    let fulldir = Filename.concat dir_prefix dir in

    dir >: test_dir fulldir begin fun relpath ->
        (* load the file at fullpath, but label with relpath *)
        let fullpath = Filename.concat fulldir relpath in
        relpath >:: test_with_temp_file "OtterTest." ".c" begin fun (temppath, tempout) ->
            close_out tempout;

            (* TODO: add standard search paths to otter.pl *)
            if (Sys.command ("./otter.pl -nostdinc -isystem./libc/ -include./libc/__otter/all.h -E -o"^temppath^" "^fullpath^" 2>/dev/null")) <> 0 then
                assert_failure "Preprocessor parse error.";

            test_otter_with_pragma ~main_loop temppath ()
        end
    end


(*
 * OUnit test suite
 *)

let testsuite = "System" >::: [
	test_integration Driver.run_with_libc "OtterCore";
]

