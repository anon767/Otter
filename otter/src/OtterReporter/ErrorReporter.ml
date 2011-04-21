(** Basic Otter reporter that prints out {!Job.Abandoned} results, and collects all results into a list. *)

open OcamlUtilities
open OtterCore

module type Errors = sig
    type t
    val printer : Format.formatter -> t -> unit
end


module Make (Errors : Errors) = struct
    class ['self] t ?max_steps ?max_paths ?max_abandoned () = object (_ : 'self)
        inherit ['self] BasicReporter.t ?max_steps ?max_paths ?max_abandoned () as super

        method report results =
            List.iter begin function
                | Job.Abandoned reason, job ->
                        Output.set_mode Output.MSG_ERROR;
                        Output.printf "@[Error \"@[%a@]\"@ occurs at @[%a@].@\n"
                            Errors.printer reason Printcil.loc (Job.get_loc job);
                        if !Executeargs.arg_print_callstack then
                            Output.printf "Call stack:@\n  @[%a@]@\n" (Printer.callingContext_list "@\n") job#state.State.callContexts;
                        Output.printf "Abandoning path.@]@."
                | _ ->
                        ()
            end results;
            super#report results
    end
end

