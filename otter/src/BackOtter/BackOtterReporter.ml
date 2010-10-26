open OcamlUtilities
open OtterCore
open OtterReporter
open Types
open Job
open Cil

let arg_no_exceptions_as_failures = ref false

class ['reason] t ?max_nodes ?max_paths ?max_abandoned
        ?(no_exceptions_as_failures= !arg_no_exceptions_as_failures) targets_ref = object
    inherit ['reason] BasicReporter.t ?max_nodes ?max_paths ?max_abandoned () as super

    method super_report = super#report

    method report result = match result with
        | Job.Complete (Job.Abandoned (`FailureReached, _, job_result)) ->
            let self = super#report result in
            (* Add a failing path to the target function *)
            (* TODO: do the same thing for `Failure when --exceptions-as-failures is enabled *)
            let fundec = List.hd (List.rev job_result.result_state.callstack) in
            let failing_path = job_result.result_decision_path in
            Output.dprintf "@\n=> Create Target for function %s@\n@\n" fundec.svar.vname;
            targets_ref := BackOtterTargets.add fundec failing_path (!targets_ref);
            self

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

