open TestUtil.MyOUnit

(* There's something wrong with ocamlbuild's dynamic dependency that causes it to miss some transitive dependencies
    from one mlpack to another mlpack. A workaround is to make sure all mlpacks are listed somewhere in this file:
    either in working code, or in the dummy module below. *)
module OcamlbuildDependencies = struct
    open DataStructures
    open OcamlUtilities
    open CilUtilities
    open OtterBytes
    open OtterCFG
    open OtterCore
    open OtterExtensions
    open OtterJob
    open OtterQueue
    open OtterReporter
    open OtterDriver
    open Otter
    open MultiOtter
    open BackOtter
end

let _ =
    run_test_tt_main begin TestList [
        TestOtterCore.FunctionCalls.testsuite;
        TestOtterCore.BoundsChecking.testsuite;
        TestOtterCore.Coverage.testsuite;
        TestOtterCore.SymbolicPointers.testsuite;
        TestOtterJob.FileJobs.testsuite;
        TestOtterCFG.DistanceToTargets.testsuite;
        TestOtterPragmaTests.OtterPragmaTestsTests.testsuite;
        TestOtterIntegration.IntegrationTests.testsuite;
        TestOtterSystem.SystemTests.testsuite;
    ] end

