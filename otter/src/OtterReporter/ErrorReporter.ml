(** Basic Otter reporter that prints out {!Job.Abandoned} results, and collects all results into a list. *)

open OcamlUtilities
open OtterCore

module type Errors = sig
    type t
    val printer : Format.formatter -> t -> unit
end


module Make (Errors : Errors) = struct
    class ['self] t ?max_nodes ?max_paths ?max_abandoned () = object (_ : 'self)
        inherit ['self] BasicReporter.t ?max_nodes ?max_paths ?max_abandoned () as super

        method report job_result =
            begin match job_result with
                | Job.Complete (Job.Abandoned (reason, loc, result)) ->
                        Output.set_mode Output.MSG_MUSTPRINT;
                        Output.printf "Error \"%a\"@ occurs at %a.@\n"
                            Errors.printer reason Printcil.loc loc;
                        if !Executeargs.arg_print_callstack then
                            Output.printf "Call stack:@\n  @[%a@]@\n" (Printer.callingContext_list "@\n") result#state.State.callContexts;
                        Output.printf "Abandoning path.@\n"
                | _ ->
                    ()
            end;
            super#report job_result
    end
end

