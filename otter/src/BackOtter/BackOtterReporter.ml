open OcamlUtilities
open OtterCore
open OtterReporter

module Reporter = ErrorReporter.Make (BackOtterErrors)

class ['self] t ?max_steps ?max_paths ?max_abandoned () = object
    inherit ['self] Reporter.t ?max_steps ?max_paths ?max_abandoned () as super

    method report result =
        begin match result with
            | Job.Complete (Job.Truncated (`SummaryAbandoned (`FailingPath _) as reason, job)) ->
                Output.set_mode Output.MSG_ERROR;
                Output.printf "@[Error \"@[%a@]\"@ occurs at @[%a@].@\n"
                    BackOtterErrors.printer reason Printcil.loc (Job.get_loc job)
            | _ ->
                ()
        end;
        super#report result
end

