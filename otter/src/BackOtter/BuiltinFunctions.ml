(**

This module contains a library of built-in functions for BackOtter.

*)

open OcamlUtilities
open OtterBytes
open OtterCore
open Interceptor

module B = OtterCore.BuiltinFunctions

let backotter_is_origin_function job retopt exps =
    let truthvalue = if List.length job#state.State.callstack = 1 then Bytes.bytes__one else Bytes.bytes__zero in
    let job = B.set_return_value job retopt truthvalue in
    B.end_function_call job

let backotter_enable_record_decisions job retopt exps =
    let job = job#with_enable_record_decisions true in
    B.end_function_call job

let backotter_disable_record_decisions job retopt exps =
    let job = job#with_enable_record_decisions false in
    B.end_function_call job

let interceptor job interceptor = Profiler.global#call "BackOtter.BuiltinFunctions.interceptor" begin fun () ->
    (* Whenever a new builtin function is added, put it in is_builtin also. *)
    try
        (
        (intercept_function_by_name_internal "__backotter_is_origin_function"         backotter_is_origin_function) @@
        (intercept_function_by_name_internal "__backotter_enable_record_decisions"     backotter_enable_record_decisions) @@
        (intercept_function_by_name_internal "__backotter_disable_record_decisions"    backotter_disable_record_decisions) @@

        (* pass on the job when none of those match *)
        interceptor

        ) job
    with Failure msg ->
        if !Executeargs.arg_failfast then failwith msg;
        (job : _ #Info.t)#finish (Job.Abandoned (`Failure msg))
end


let is_builtin =
    let builtins = [
        "__backotter_is_origin_function";
        "__backotter_enable_record_decisions";
        "__backotter_disable_record_decisions";
    ]
    in function name -> List.mem name builtins

