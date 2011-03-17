open OcamlUtilities
open OtterCore
open State
open Job
open Cil

let max_function_name_length = ref 0

(* Setup max_function_name_length, to be used in set_output_formatter_interceptor *)
let set_max_function_name_length fns =
    max_function_name_length := List.fold_left (fun len fundec -> max len (String.length fundec.svar.vname)) 0 fns

let set_output_formatter_interceptor job job_queue interceptor =
    let origin_function_name = (List.hd (List.rev job#state.callstack)).svar.vname in
    let depth = PathCondition.length job#state.path_condition in
    let loc = Job.get_loc job in
    let label =
        if loc = Cil.locUnknown then
            Format.sprintf "%*s [%d,%d] : " (!max_function_name_length) origin_function_name job#path_id depth
        else
            Format.sprintf "%*s [%d,%d] %s:%d : " (!max_function_name_length) origin_function_name job#path_id depth (Filename.basename loc.Cil.file) loc.Cil.line
    in
    Output.set_formatter (new Output.labeled label);
    interceptor job job_queue

