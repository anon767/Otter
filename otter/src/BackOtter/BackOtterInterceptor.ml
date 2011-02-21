open OcamlUtilities
open OtterCore
open State
open Job
open Cil

let arg_line_targets = ref []
let max_function_name_length = ref 0

let set_output_formatter_interceptor job job_queue interceptor =
    let origin_function_name = (List.hd (List.rev job#state.callstack)).svar.vname in
    let depth = List.length job#state.path_condition in
    let loc = Job.get_loc job in
    let label =
        if loc = Cil.locUnknown then
            Format.sprintf "%*s [%d,%d] : " (!max_function_name_length) origin_function_name job#path_id depth
        else
            Format.sprintf "%*s [%d,%d] %s:%d : " (!max_function_name_length) origin_function_name job#path_id depth (Filename.basename loc.Cil.file) loc.Cil.line
    in
    Output.set_formatter (new Output.labeled label);
    interceptor job job_queue


(** An interceptor that emits FailureReached when some (file, line) in arg_line_targets is encountered. *)
let line_target_interceptor job job_queue interceptor =
    let loc = Job.get_loc job in
    if List.mem (loc.Cil.file, loc.Cil.line) (!arg_line_targets) then
        Complete (Abandoned (`FailureReached, job)), job_queue
    else
        interceptor job job_queue

(** {1 Command-line options} *)

let options = [
    ("--line-targets",
        Arg.String begin fun str ->
            let args = Str.split (Str.regexp ",") str in
            let re = Str.regexp "\\(.*\\):\\(.*\\)" in
            List.iter (fun arg ->
                if Str.string_match re arg 0 then
                    let file = Str.matched_group 1 arg in
                    let line = int_of_string (Str.matched_group 2 arg) in
                    arg_line_targets := (file, line)::(!arg_line_targets)
                else
                    failwith "Error in parsing --line-targets"
            ) args
        end,
        "<line[,lines]> Lines in the form file:linenum[,file:linenum...]. Default is empty list.\n");
]
