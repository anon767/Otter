open MyOUnit

let _ =
    run_test_tt_main begin TestList [
        OUnitSymexeIntegration.testsuite;
    ] end

