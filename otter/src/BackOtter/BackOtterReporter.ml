open OcamlUtilities
open OtterCore
open OtterReporter

module Reporter = ErrorReporter.Make (BackOtterErrors)

class ['self] t ?max_steps ?max_paths ?max_abandoned () = object
    inherit ['self] Reporter.t ?max_steps ?max_paths ?max_abandoned () as super

    method report results =
        List.iter begin function
            | Job.Truncated (`SummaryAbandoned reason), job ->
                Output.set_mode Output.MSG_ERROR;
                Output.printf "@[Truncated \"@[%a@]\"@ occurs at @[%a@].@\n"
                    BackOtterErrors.printer reason Printcil.loc (Job.get_loc job)
            | _ ->
                ()
        end results;
        super#report results
end

