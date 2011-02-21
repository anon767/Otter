open TestUtil.MyOUnit
open TestUtil.OtterUtil
open OtterBytes
open OtterCore
open OtterDriver
open BuiltinFunctions
open Interceptor
open Job
open Cil

(** Convenience function to receive a constant int value from an argument
		@param exp the argument 
        @raises Failure if the argument is not a constant int
		@return the constant int
*)
let get_constant_int = function
    | Const(CInt64(i64, _, _)) -> Int64.to_int i64
    | _ -> failwith "Argument is not constant integer"

(* A global map that maps integer indices to (instruction, context) *)
module IndexMap = State.IndexMap
let instruction_context_map = ref IndexMap.empty 

let otter_instr_mark job retopt exps errors =
    let exp = get_lone_arg exps in
    let index = get_constant_int exp in
    let instruction = Job.get_instruction job in
    let context = Job.get_instruction_context job in
    instruction_context_map := IndexMap.add index (instruction, context) (!instruction_context_map);
    let job = end_function_call job in
    (Job.Active job, errors)

let otter_distance_from_instr_mark job retopt exps errors =
    let exp = get_lone_arg exps in
    let index = get_constant_int exp in
    let distance = 
        try
            let instruction_src, context = IndexMap.find index (!instruction_context_map) in
            let instruction_des = Job.get_instruction job in
            OtterCFG.DistanceToTargets.find_in_context instruction_src context [instruction_des] 
        with Not_found -> max_int (* denotes infinite distance, same as DistanceToReturn *)
    in
    let ret = Bytes.int_to_bytes distance in
    let job, errors = set_return_value job retopt ret errors in
    let job = end_function_call job in
    (Job.Active job, errors)

let builtin_interceptor job job_queue interceptor = 
	try
		(
		(intercept_function_by_name_internal "__otter_instr_mark"                otter_instr_mark) @@
		(intercept_function_by_name_internal "__otter_distance_from_instr_mark"  otter_distance_from_instr_mark) @@
		(* pass on the job when none of those match *)
		interceptor
		) job job_queue
	with Failure msg ->
		if !Executeargs.arg_failfast then failwith msg;
		(Job.Complete (Job.Abandoned (`Failure msg, job)), job_queue) 

let expected_return_value expected results =
    let returned_bytes = match results with
        | [ Job.Return(Some(bytes), _) ] -> bytes
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

let run_with_interceptor reporter job =
    let interceptor =
        builtin_interceptor
        >>> BuiltinFunctions.interceptor
    in
    Driver.run ~interceptor reporter job

let testsuite = "Distance-to-targets" >::: [
    test_otter
        ~label:"Simple"
        ~driver:run_with_interceptor
        "int main(void) {
            __otter_instr_mark(1);
            return __otter_distance_from_instr_mark(1);
        }"
        (expected_return_value 1);

    test_otter
        ~label:"One-level call context"
        ~driver:run_with_interceptor
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

