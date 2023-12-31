open OcamlUtilities
open State

(* TODO: this module can be a place for better output formatting *)

let old_job_id = ref 0
let set_output_formatter job =
    if !old_job_id <> job#node_id then (
        Output.set_mode Output.MSG_REG;
        Output.printf "***** Changing running job *****@.";
        old_job_id := job#node_id
    );
    let depth = PathCondition.length job#state.path_condition in
    let loc = Job.get_loc job in
    let label =
        if loc = Cil.locUnknown then
            Format.sprintf "[%d,%d] : " job#node_id depth
        else
            Format.sprintf "[%d,%d] %s:%d : " job#node_id depth (Filename.basename loc.Cil.file) loc.Cil.line
    in
    Output.set_formatter (new Output.labeled label)
