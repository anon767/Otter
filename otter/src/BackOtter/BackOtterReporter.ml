open OtterCore
open OtterReporter

let arg_no_exceptions_as_failures = ref false

class ['reason] t ?max_nodes ?max_paths ?max_abandoned
        ?(no_exceptions_as_failures= !arg_no_exceptions_as_failures) targets_ref = object
    inherit ['reason] BasicReporter.t ?max_nodes ?max_paths ?max_abandoned () as super

    method super_report = super#report

    method report result = match result with
        | Job.Complete (Job.Abandoned (`FailureReached, _, _)) ->
            super#report result

        | Job.Complete (Job.Abandoned (_, _, _)) when no_exceptions_as_failures ->
            (* munge the statistics then call super indirectly *)
            (* TODO: break BasicReporter.report into smaller pieces and get rid of the ugly indirection *)
            {< abandoned = abandoned - 1 >}#super_report result

        | result ->
            super#report result
end


(** {1 Command-line options} *)

let options = [
    "--no-exceptions-as-failures",
        Arg.Set arg_no_exceptions_as_failures,
        " Do not treat general exceptions (e.g., dereferencing a non-pointer) as assertion failures (i.e., contribute failure paths)";
]

