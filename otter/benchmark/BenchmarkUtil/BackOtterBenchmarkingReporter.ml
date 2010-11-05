open OtterCore
open BackOtter

class ['self] t ?max_nodes ?max_paths ?max_abandoned () = object
    inherit ['self] BenchmarkingReporter.t ?max_nodes ?max_paths ?max_abandoned () as super

    method super_report = super#report

    method report result = match result with
        | Job.Complete (Job.Abandoned (`FailureReached, _, _)) ->
            super#report result

        | Job.Complete (Job.Abandoned (_, _, _)) when not (!BackOtterReporter.arg_exceptions_as_failures)->
            Format.printf "OMG You are here!";
            (* munge the statistics then call super indirectly *)
            {< abandoned = abandoned - 1 >}#super_report result

        | result ->
            super#report result
end
