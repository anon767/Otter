open TestUtil.MyOUnit

(* ocamlbuild's dynamic dependency is broken: turn the below into static dependencies *)
module OcamlbuildDependencies = struct
    open Otter
end

let _ =
    run_test_tt_main begin TestList [
        TestOtter.FileJobs.testsuite;
        TestOtter.FunctionCalls.testsuite;
        TestOtter.Coverage.testsuite;
        TestOtter.Merging.testsuite;
        TestOtter.BoundsChecking.testsuite;
        TestOtterIntegration.IntegrationTests.testsuite;
    ] end

