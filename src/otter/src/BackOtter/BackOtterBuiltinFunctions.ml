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

let backotter_origin_from_mainfn job retopt exps = 
    let origin_fundec = BackOtterUtilities.get_origin_function job in
    let mainfn = ProgramPoints.get_main_fundec job#file in
    let truthvalue = if CilUtilities.CilData.CilFundec.equal origin_fundec mainfn then Bytes.bytes__one else Bytes.bytes__zero in
    let job = B.set_return_value job retopt truthvalue in
    B.end_function_call job

let backotter_enable_record_decisions job retopt exps =
    let job = job#with_enable_record_decisions true in
    B.end_function_call job

let backotter_disable_record_decisions job retopt exps =
    let job = job#with_enable_record_decisions false in
    B.end_function_call job

let backotter_is_bounded job retopt exps =
    let is_bounded = match job#bounding_paths with None -> false | Some _ -> true in
    let truthvalue = if is_bounded then Bytes.bytes__one else Bytes.bytes__zero in
    let job = B.set_return_value job retopt truthvalue in
    B.end_function_call job

let interceptor job interceptor = Profiler.global#call "BackOtter.BuiltinFunctions.interceptor" begin fun () ->
    (* Whenever a new builtin function is added, put it in is_builtin also. *)
    try
        (
        (intercept_function_by_name_internal "__backotter_is_origin_function"         backotter_is_origin_function) @@
        (intercept_function_by_name_internal "__backotter_origin_from_mainfn"         backotter_origin_from_mainfn) @@
        (intercept_function_by_name_internal "__backotter_enable_record_decisions"    backotter_enable_record_decisions) @@
        (intercept_function_by_name_internal "__backotter_disable_record_decisions"   backotter_disable_record_decisions) @@
        (intercept_function_by_name_internal "__backotter_is_bounded"                 backotter_is_bounded) @@

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
        "__backotter_origin_from_mainfn";
        "__backotter_enable_record_decisions";
        "__backotter_disable_record_decisions";
        "__backotter_is_bounded";
    ]
    in function name -> List.mem name builtins

