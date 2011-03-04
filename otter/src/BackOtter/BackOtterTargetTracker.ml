open OcamlUtilities
open OtterCore
open Job
open Cil

class ['self] t delegate entry_fn =
object (_ : 'self)
    val delegate = delegate
    method report job_state =
        let original_job_state = job_state in

        (* convert executions that report repeated abandoned paths to Truncated *)
        let job_state = match job_state with
            | Job.Complete (Job.Abandoned (`TargetReached target, job_result)) ->
                let fundec = BackOtterUtilities.get_origin_function_from_job_result job_result in
                let instruction = Job.get_instruction job_result in
                (* Failing path has least recent decision first. See the comment in BidirectionalQueue. *)
                let failing_path = List.rev job_result#decision_path in
                begin try
                    BackOtterTargets.add_path fundec failing_path (Some instruction);
                    job_state
                with Invalid_argument _ ->
                    Job.Complete (Job.Truncated (`SummaryAbandoned (`TargetReached target, Job.get_loc job_result), job_result))
                end
            | Job.Complete (Job.Abandoned (`Failure msg, job_result)) when !BackOtterReporter.arg_exceptions_as_failures ->
                let fundec = BackOtterUtilities.get_origin_function_from_job_result job_result in
                let instruction = Job.get_instruction job_result in
                (* Failing path has least recent decision first. See the comment in BidirectionalQueue. *)
                let failing_path = List.rev job_result#decision_path in
                begin try
                    BackOtterTargets.add_path fundec failing_path (Some instruction);
                    job_state
                with Invalid_argument _ ->
                    Job.Complete (Job.Truncated (`SummaryAbandoned (`Failure msg, Job.get_loc job_result), job_result))
                end
            | _ ->
                job_state
        in
        (* convert executions from non-entry functions to Truncated *)
        let job_state = match job_state with
            | Job.Complete (Job.Return (return_code, job_result))
                    when BackOtterUtilities.get_origin_function_from_job_result job_result != entry_fn ->
                Job.Complete (Job.Truncated (`SummaryReturn return_code, job_result))
            | Job.Complete (Job.Exit (return_code, job_result))
                    when BackOtterUtilities.get_origin_function_from_job_result job_result != entry_fn ->
                Job.Complete (Job.Truncated (`SummaryExit return_code, job_result))
            | Job.Complete (Job.Abandoned (reason, job_result))
                    when BackOtterUtilities.get_origin_function_from_job_result job_result != entry_fn ->
                Job.Complete (Job.Truncated (`SummaryAbandoned (reason, Job.get_loc job_result), job_result))
            | _ ->
                job_state
        in
        let delegate = delegate#report job_state in

        (* Print failing path. This is run after delegate#report so the failing path is printed after the failure message. *)
        let print_failing_path job_result =
            let fundec = BackOtterUtilities.get_origin_function_from_job_result job_result in
            let failing_path = List.rev job_result#decision_path in
            Output.debug_printf "@\n=> Extract the following failing path for function %s:@." fundec.svar.vname;
            Output.debug_printf "@[%a@]@\n@." Decision.print_decisions failing_path;
        in
        begin match original_job_state with
            | Job.Complete (Job.Abandoned (`TargetReached target, job_result)) ->
                Output.printf "target_tracker: TargetReached @[%a@]@." Target.printer target;
                print_failing_path job_result
            | Job.Complete (Job.Abandoned (`Failure msg, job_result)) when !BackOtterReporter.arg_exceptions_as_failures ->
                Output.printf "target_tracker: Failure (%s)@." msg;
                print_failing_path job_result
            | _ -> ()
        end;
        {< delegate = delegate >}

    method should_continue = delegate#should_continue

    method completed = delegate#completed

    method delegate = delegate
end

