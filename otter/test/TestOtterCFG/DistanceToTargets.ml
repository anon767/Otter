open TestUtil.MyOUnit
open TestUtil.OtterUtil
open OtterBytes
open OtterCore
open Job

let expected_return_value expected results =
    let returned_bytes = match results with
        | [ Return(Some(bytes), _) ] -> bytes
        | _ -> assert_failure "This test expects one return value"
    in
    let returned_int = 
        try
            Bytes.bytes_to_int_auto returned_bytes
        with e ->
            assert_failure "This test expects one concrete return value"
    in
    assert_equal
        ~printer:(fun ff r -> Format.fprintf ff "return (%d)" r)
        expected
        returned_int

let testsuite = "Distance-to-targets" >::: [
    test_otter
        ~label:"Simple"
        "int main(void) {
            __otter_instr_mark(1);
            return __otter_distance_from_instr_mark(1);
        }"
        (expected_return_value 1);

    test_otter
        ~label:"One-level call context"
        "void f(void) {
            __otter_instr_mark(1);
            return;
        }
        int main(void) {
            f();
            return __otter_distance_from_instr_mark(1);
        }"
        (expected_return_value 1);

]

