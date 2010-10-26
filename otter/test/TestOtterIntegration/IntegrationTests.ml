open TestUtil.MyOUnit
open OtterDriver
open Otter
open MultiOtter
open BackOtter

module CorePragmaTest = TestUtil.OtterPragmaTests.Make (OtterCore.Errors) 


(* directory containing tests for IntegrationTests *)
let dir_prefix = Filename.concat "test" "TestOtterIntegration"


(* test helper that runs the symbolic executor on all files relative to dir_prefix/dir *)
let test_integration test_otter_with_pragma driver dir =
    let fulldir = Filename.concat dir_prefix dir in

    dir >: test_dir fulldir begin fun relpath ->
        (* load the file at fullpath, but label with relpath *)
        let fullpath = Filename.concat fulldir relpath in
        relpath >:: test_otter_with_pragma driver fullpath
    end


(*
 * OUnit test suite
 *)

let testsuite = "Integration" >::: [
    test_integration CorePragmaTest.test_otter_with_pragma Driver.run_basic "OtterCore";
    test_integration CorePragmaTest.test_otter_with_pragma MultiDriver.run "MultiprocessOtter";
]

