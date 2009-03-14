open OUnit

let _ =
    run_test_tt_main begin TestList [
        OUnitSymexeIntegration.testsuite;
    ] end

