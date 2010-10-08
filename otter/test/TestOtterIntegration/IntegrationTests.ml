open TestUtil.MyOUnit
open TestUtil.OtterPragmaTests
open OtterDriver
open Otter
open MultiOtter


(* directory containing tests for IntegrationTests *)
let dir_prefix = Filename.concat "test" "TestOtterIntegration"


(* test helper that runs the symbolic executor on all files relative to dir_prefix/dir *)
let test_integration main_loop dir =
    let fulldir = Filename.concat dir_prefix dir in

    dir >: test_dir fulldir begin fun relpath ->
        (* load the file at fullpath, but label with relpath *)
        let fullpath = Filename.concat fulldir relpath in
        relpath >:: test_otter_with_pragma ~main_loop fullpath
    end


(*
 * OUnit test suite
 *)

let testsuite = "Integration" >::: [
	test_integration Driver.run_basic "OtterCore";
	test_integration Multiprocess.run "MultiprocessOtter";
]

