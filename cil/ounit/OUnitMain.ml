open MyOUnit

let _ =
    run_test_tt_main begin TestList [
        OUnitSymexeFileJobs.testsuite;
        OUnitSymexeCoverage.testsuite;
        OUnitSymexeIntegration.testsuite;
    ] end

