open OcamlUtilities
open CilUtilities
open OtterCFG
open OtterCore
open Job
open Cil

(* TODO: replace LineTargets with the below *)
module TargetSet = Set.Make (struct type t = string * int let compare = Pervasives.compare end)
let targets = ref TargetSet.empty
let target_matchers = ref (fun (reason : BackOtterErrors.t) (job : Job.t) k -> k reason job)


let get_line_targets file =
    TargetSet.fold begin fun (filename, linenumber as line) targets ->
        (* Use of_stmt_last to ensure that instructions before the last one are covered *)
        try
            List.map (fun (fundec, stmt) -> Instruction.of_stmt_last file fundec stmt) (FindCil.stmts_by_line file line) @ targets
        with Not_found ->
            let output_mode = Output.get_mode () in
            Output.set_mode Output.MSG_REG;
            Output.printf "Warning: line target %s:%d not found.@." filename linenumber;
            Output.set_mode output_mode;
            targets
    end !targets []


let add_target string =
    let fields = Str.bounded_split (Str.regexp_string ",") string 4 in

    let file, line, error_matcher =
        try
            match fields with
                | [ file; line; name; arg ] -> (file, int_of_string line, BackOtterErrors.matcher name [ Cil.AStr arg ])
                | [ file; line; name ] -> (file, int_of_string line, BackOtterErrors.matcher name [])
                | [ file; line ] -> (file, int_of_string line, fun _ -> true)
                | _ -> FormatPlus.invalid_arg "Invalid number of fields in BackOtter target specification %s" string
        with
            | Failure "int_of_string" ->
                FormatPlus.invalid_arg "Invalid line number in BackOtter target specification %s" string
            | Failure msg -> (* TODO: Error should raise Invalid_arg, not Failure *)
                FormatPlus.invalid_arg "Invalid BackOtter target specification %s: %s" string msg
    in

    let target_matcher reason job_result k =
        let loc = Job.get_loc job_result in
        if loc.Cil.file = file && loc.Cil.line = line && error_matcher reason then begin
            Output.must_printf "BackOtter target reached %s@\n" string;
            let fundec = BackOtterUtilities.get_origin_function job_result in
            let entryfn = ProgramPoints.get_entry_fundec job_result#file in
            let failing_path = List.rev job_result#decision_path in
            let instruction = Job.get_instruction job_result in
            BackOtterTargets.add_path fundec failing_path (Some instruction);
            if CilData.CilFundec.equal fundec entryfn then begin
                (* Remove instruction from line_targets *)
                Output.must_printf "Remove target %s:%d@\n" file line;
                targets := TargetSet.remove (file, line) !targets;
                (* Remove instruction-related failing paths and function targets from BackOtterTargets *)
                BackOtterTargets.remove_target_instruction instruction
            end
        end else
            k reason job_result
    in

    targets := TargetSet.add (file, line) !targets;
    let target_matchers' = !target_matchers in
    target_matchers := (fun reason job k -> target_matchers' reason job (fun reason job -> target_matcher reason job k))



class ['self] t delegate entry_fn =
object (_ : 'self)
    val delegate = delegate
    method report job_state =
        let original_job_state = job_state in

        (* detect requested targets *)
        begin match job_state with
            | Job.Complete (Job.Abandoned (reason, job_result)) ->
                !target_matchers reason (job_result :> Job.t) (fun _ _ -> ())
            | _ ->
                ()
        end;

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


let options = [
    "--backotter-target",
        Arg.String add_target,
        "<file,line[,reason[,reason-argument]> Add a target for BackOtter"
]

