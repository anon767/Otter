(** Basic Otter reporter that prints out {!Job.Abandoned} results, and collects all results into a list. *)

open OcamlUtilities
open OtterCore


let default_max_nodes = ref 0
let default_max_paths = ref 0
let default_max_abandoned = ref 0


class ['reason] t
        ?(max_nodes=max 0 !default_max_nodes)
        ?(max_paths=max 0 !default_max_paths)
        ?(max_abandoned=max 0 !default_max_abandoned)
        ()
        = object
    val nodes = 0
    val paths = 0
    val abandoned = 0
    val completed = []

    method report = function
        | Job.Complete completion ->
            begin match completion with
                | Job.Abandoned ((reason : 'reason), loc, { Job.result_state=state; Job.result_history=hist }) ->
                    Output.set_mode Output.MSG_MUSTPRINT;
                    Output.printf "Error \"%a\"@ occurs at %a.@\n"
                        Report.abandoned_reason reason Printcil.loc loc;
                    if !Executeargs.arg_print_callstack then
                        Output.printf "Call stack:@\n  @[%a@]@\n" (Printer.callingContext_list "@\n") state.Types.callContexts;
                    Output.printf "Abandoning path.@\n"
                | _ ->
                    ()
            end;
            let nodes = nodes + 1 in
            let paths = paths + 1 in
            let abandoned = abandoned + (match completion with Job.Abandoned _ -> 1 | _ -> 0) in
            let reporter = {< nodes = nodes; paths = paths; abandoned = abandoned; completed = completion::completed >} in
            if (max_nodes = 0 || nodes < max_nodes)
                    &&(max_paths = 0 || paths < max_paths)
                    && (max_abandoned = 0 || abandoned < max_abandoned) then
                (reporter, true)
            else
                (reporter, false)

        | Job.Active _
        | Job.Fork _ ->
            let nodes = nodes + 1 in
            let reporter = {< nodes = nodes >} in
            if max_nodes = 0 || nodes < max_nodes then
                (reporter, true)
            else
                (reporter, false)

        | Job.Paused _ ->
            invalid_arg "unexpected Job.Paused"

    method completed = completed
end


(** {1 Command-line options} *)

let options = [
    "--max-nodes",
        Arg.Set_int default_max_nodes,
        "<bound> Bound the number of nodes in the execution tree to explore (default: unbounded)";
    "--max-paths",
        Arg.Set_int default_max_paths,
        "<bound> Bound the number of paths to execute to completion (default: unbounded)";
    "--max-abandoned",
        Arg.Set_int default_max_abandoned,
        "<bound> Bound the number of abandoned paths to return (default: unbounded)";
]

