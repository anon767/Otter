open OcamlUtilities
open OtterCore
open State
open Job
open Cil


let max_origin_function_name_length = ref 0
let max_current_function_name_length = ref 0
let set_output_formatter_interceptor job interceptor =
        let origin_function_name = (List.hd (List.rev job#state.callstack)).svar.vname in
        let current_function_name = (List.hd job#state.callstack).svar.vname in
        let loc = Job.get_loc job in
        max_origin_function_name_length := max (!max_origin_function_name_length) (String.length origin_function_name);
        max_current_function_name_length := max (!max_current_function_name_length) (String.length current_function_name);
        let label =
            let node_id = job#node_id in
            let is_bounded = match job#bounding_paths with Some _ -> true | None -> false in
            let parent_node_id = try string_of_int job#parent_node_id with Failure "hd" -> "hd" in
            let depth = PathCondition.length job#state.path_condition in
            let label = Format.sprintf "%*s %*s [%s%d,%s,%d]" (!max_origin_function_name_length) origin_function_name (!max_current_function_name_length) current_function_name (if is_bounded then "#" else "") node_id parent_node_id depth in
            let label = 
                if loc = Cil.locUnknown then label
                else Format.sprintf "%s %s:%d" label (Filename.basename loc.Cil.file) loc.Cil.line
            in
            label ^ " : "
        in
        Output.set_formatter (new Output.labeled label);
        interceptor job

let jobinit_interceptor job interceptor =
    if job#is_initialized then interceptor job
    else job#initialize
