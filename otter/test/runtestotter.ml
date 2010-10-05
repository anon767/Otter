open TestUtil.MyOUnit

(* There's something wrong with ocamlbuild's dynamic dependency that causes it to miss some transitive dependencies
    from one mlpack to another mlpack. A workaround is to make sure all mlpacks are listed somewhere in this file:
    either in working code, or in the dummy module below. *)
module OcamlbuildDependencies = struct
    open DataStructures
    open OcamlUtilities
    open OtterBytes
    open OtterCore
    open OtterJob
    open OtterQueue
    open OtterReporter
    open OtterDriver
    open OtterGraph
    open Otter
end

let _ =
    run_test_tt_main begin TestList [
        TestOtterCore.FunctionCalls.testsuite;
        TestOtterCore.SymbolicPointers.testsuite;
        TestOtter.FileJobs.testsuite;
        TestOtter.Coverage.testsuite;
        TestOtter.BoundsChecking.testsuite;
        TestOtterPragmaTests.OtterPragmaTestsTests.testsuite;
        TestOtterIntegration.IntegrationTests.testsuite;
        TestOtterSystem.SystemTests.testsuite;
    ] end

