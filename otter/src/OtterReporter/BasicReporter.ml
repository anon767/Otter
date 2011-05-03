open OtterCore


let default_max_steps = ref 0
let default_max_paths = ref 0
let default_max_abandoned = ref 0
let arg_convert_non_failure_abandoned_to_truncated = ref false


class ['self] t
            ?(max_steps=max 0 !default_max_steps)
            ?(max_paths=max 0 !default_max_paths)
            ?(max_abandoned=max 0 !default_max_abandoned)
        ()
        = object (_ : 'self)
    val steps = 0
    val paths = 0
    val abandoned = 0
    val completed = []

    method report results =
        let steps = steps + 1 in
        let paths, abandoned, completed = List.fold_left begin fun (paths, abandoned, completed) (completion, job) ->
            let paths = paths + (match completion with Job.Truncated _ -> 0 | _ -> 1) in
            let abandoned = abandoned + (match completion with Job.Abandoned _ -> 1 | _ -> 0) in
            let completed = (completion, job)::completed in
            (paths, abandoned, completed)
        end (paths, abandoned, completed) results in
        {< steps = steps; paths = paths; abandoned = abandoned; completed = completed >}

    method should_continue =
        (max_steps = 0 || steps < max_steps)
            && (max_paths = 0 || paths < max_paths)
            && (max_abandoned = 0 || abandoned < max_abandoned)

    method completed = completed

    method get_stats = (steps, paths, abandoned)

end

(* TODO: remove this function and roll its functionality elsewhere; should consider a more structured notion of targets or error filtering *)
let convert_non_failure_abandoned_to_truncated results =
    if !arg_convert_non_failure_abandoned_to_truncated then
        List.map begin function
            | Job.Abandoned (`TargetReached _), job as result -> result
            | Job.Abandoned reason, job ->
                (Job.Truncated reason, job)
            | result ->
                result
        end results
    else
        results


(** {1 Command-line options} *)

let options = [
    "--max-steps",
        Arg.Set_int default_max_steps,
        "<bound> Bound the number of instruction steps to execute (default: unbounded)";
    "--max-paths",
        Arg.Set_int default_max_paths,
        "<bound> Bound the number of paths to execute to completion (default: unbounded)";
    "--max-abandoned",
        Arg.Set_int default_max_abandoned,
        "<bound> Bound the number of abandoned paths to return (default: unbounded)";
    "--convert-non-failure-abandoned-to-truncated",
        Arg.Set arg_convert_non_failure_abandoned_to_truncated,
        " Convert non __FAILURE generated Abandoned's to Truncated's (default: no)";
]

