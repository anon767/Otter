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

let builtin_interceptor job interceptor =
	try
		(
		(intercept_function_by_name_internal "__otter_instr_mark"                otter_instr_mark) @@
		(intercept_function_by_name_internal "__otter_distance_from_instr_mark"  otter_distance_from_instr_mark) @@
		(* pass on the job when none of those match *)
		interceptor
		) job
	with Failure msg ->
		if !Executeargs.arg_failfast then failwith msg;
		Job.Complete (Job.Abandoned (`Failure msg, job))

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
    let step job = interceptor job Statement.step in
    Driver.run ~step reporter job

let test_ottercfg code ?label expected = test_otter code ?label ~driver:run_with_interceptor (expected_return_value expected)

let testsuite_fast = "Fast" >::: [

    test_ottercfg
        ~label:"Basic"
        "int main(void) {
            __otter_instr_mark(1);
            return __otter_distance_from_instr_mark(1);
        }"
        1;

     test_ottercfg
        ~label:"Simple instructions between source and target"
        "int main(void) {
            __otter_instr_mark(1);
            int x = 1;
            int y = 1;
            int z = 1;
            return __otter_distance_from_instr_mark(1);
        }"
        4;

    test_ottercfg
        ~label:"Take the shortest path in a function call"
        "void g(int i) {
            if (i) {
                int x = 1;
                int y = 1;
                int z = 1;
            }
            return;
        }
        int main(void) {
            __otter_instr_mark(1);
            g(1);
            return __otter_distance_from_instr_mark(1);
        }"
        3;

    test_ottercfg
        ~label:"Function call"
        "void g(void) {
            return;
        }
        int main(void) {
            __otter_instr_mark(1);
            g();
            return __otter_distance_from_instr_mark(1);
        }"
        2;

    test_ottercfg
        ~label:"Recursion"
        "void g(int i) {
            if (i<=0) 
                return;
            g(i-1);
        }
        int main(void) {
            __otter_instr_mark(1);
            g(10);
            return __otter_distance_from_instr_mark(1);
        }"
        3;

    test_ottercfg
        ~label:"One-level calling context"
        "void f(void) {
            __otter_instr_mark(1);
            return;
        }
        int main(void) {
            f();
            return __otter_distance_from_instr_mark(1);
        }"
        2;

    test_ottercfg
        ~label:"Two-level calling context"
        "void f(void) {
            __otter_instr_mark(1);
            return;
        }
        void g(void) {
            f();
            return;
        }
        int main(void) {
            g();
            return __otter_distance_from_instr_mark(1);
        }"
        3;

]

let testsuite_bugs = "Bugs" >::: [

    test_ottercfg
        ~label:"Nested function call"
        "void f(void) {
            return;
        }
        void g(void) {
            f();
            return;
        }
        int main(void) {
            __otter_instr_mark(1);
            g();
            return __otter_distance_from_instr_mark(1);
        }"
        3;

]

let testsuite = "Distance-to-targets" >::: [
    testsuite_fast;
    testsuite_bugs;
]
