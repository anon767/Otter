open TestUtil.MyOUnit
open TestUtil.OtterPragmaTests
open OtterDriver
open Otter
open MultiOtter
open BackOtter

module CorePragmaTest = TestUtil.OtterPragmaTests.Make (OtterCore.Errors) 
module BackOtterPragmaTest = TestUtil.OtterPragmaTests.Make (BackOtter.BackOtterErrors)


(* directory containing tests for SystemTests *)
let dir_prefix = Filename.concat "test" "TestOtterSystem"


(* test helper that runs the symbolic executor on all files relative to dir_prefix/dir *)
let test_system test_otter_with_pragma driver dir =
    let fulldir = Filename.concat dir_prefix dir in

    dir >: test_dir fulldir begin fun relpath ->
        (* load the file at fullpath, but label with relpath *)
        let fullpath = Filename.concat fulldir relpath in
        relpath >:: test_with_temp_file "OtterTest." ".c" begin fun (temppath, tempout) ->
            close_out tempout;

            (* TODO: add standard search paths to otter.pl *)
            (* TODO: fix string escaping for file names with special characters *)
            if (Sys.command ("./otter.pl -nostdinc -isystem./libc/ -include./libc/__otter/all.h -E -o\""^temppath^"\" \""^fullpath^"\" 2>/dev/null")) <> 0 then
                assert_failure "Preprocessor parse error.";

            test_otter_with_pragma driver temppath ()
        end
    end


(*
 * OUnit test suite
 *)

let testsuite = "System" >::: [
	test_system CorePragmaTest.test_otter_with_pragma Driver.run_with_libc "OtterCore";
	test_system CorePragmaTest.test_otter_with_pragma MultiDriver.run "MultiProcessOtter";
	test_system BackOtterPragmaTest.test_otter_with_pragma BackOtterDriver.callchain_backward_se "BackOtter";
]

