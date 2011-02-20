open OtterCore
open BackOtter

class ['self] t ?max_nodes ?max_paths ?max_abandoned () = object
    inherit ['self] BenchmarkingReporter.t ?max_nodes ?max_paths ?max_abandoned () as super

    method super_report = super#report

    method report result = match result with
        | Job.Complete (Job.Abandoned (`FailureReached, _)) ->
            super#report result

        | Job.Complete (Job.Abandoned (_, _)) when not (!BackOtterReporter.arg_exceptions_as_failures)->
            (* munge the statistics then call super indirectly *)
            {< abandoned = abandoned - 1 >}#super_report result

        | result ->
            super#report result
end
