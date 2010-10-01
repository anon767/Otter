(** Basic Otter reporter that prints out {!Job.Abandoned} results, and collects all results into a list. *)

open OcamlUtilities
open OtterCore

class ['reason] t = object
    val completed = []

    method report = function
        (* log some interesting errors *)
        | Job.Abandoned ((reason : 'reason), loc, { Job.result_state=state; Job.result_history=hist }) as c ->
            Output.set_mode Output.MSG_MUSTPRINT;
            Output.printf "Error \"%a\"@ occurs at %a.@\n"
                Report.abandoned_reason reason Printcil.loc loc;
            if !Executeargs.arg_print_callstack then
                Output.printf "Call stack:@\n  @[%a@]@\n" (Printer.callingContext_list "@\n") state.Types.callContexts;
            Output.printf "Abandoning path.@\n";
            {< completed = c::completed >}

        | c ->
            {< completed = c::completed >}

    method completed = completed

end

let make () = new t
