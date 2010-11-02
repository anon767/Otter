open OtterCore
open OtterReporter

module Reporter = ErrorReporter.Make (BackOtterErrors)

let arg_exceptions_as_failures = ref false

class ['self] t ?max_nodes ?max_paths ?max_abandoned
        ?(exceptions_as_failures= !arg_exceptions_as_failures) () = object
    inherit ['self] Reporter.t ?max_nodes ?max_paths ?max_abandoned () as super

    method super_report = super#report

    method report result = match result with
        | Job.Complete (Job.Abandoned (`FailureReached, _, _)) ->
            super#report result

        | Job.Complete (Job.Abandoned (_, _, _)) when not exceptions_as_failures ->
            (* munge the statistics then call super indirectly *)
            (* TODO: break BasicReporter.report into smaller pieces and get rid of the ugly indirection *)
            {< abandoned = abandoned - 1 >}#super_report result

        | result ->
            super#report result
end


(** {1 Command-line options} *)

let options = [
    "--exceptions-as-failures",
        Arg.Set arg_exceptions_as_failures,
        " Treat general exceptions (e.g., dereferencing a non-pointer) as failures (i.e., contribute failing paths)";
]

