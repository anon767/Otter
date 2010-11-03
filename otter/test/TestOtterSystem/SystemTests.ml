open TestUtil.MyOUnit
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
        relpath >:: TestUtil.OtterUtil.test_with_preprocessed_file fullpath (test_otter_with_pragma driver)
    end


(*
 * OUnit test suite
 *)

let testsuite = "System" >::: [
	test_system CorePragmaTest.test_otter_with_pragma Driver.run_with_libc "OtterCore";
	test_system CorePragmaTest.test_otter_with_pragma MultiDriver.run "MultiProcessOtter";
	test_system BackOtterPragmaTest.test_otter_with_pragma BackOtterDriver.callchain_backward_se "BackOtter";
]

