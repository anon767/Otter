open OcamlUtilities
open CilUtilities
open OtterCFG
open OtterCore
open Job
open Cil

(* TODO: replace LineTargets with the below *)
module TargetSet = Set.Make (struct type t = string * int let compare = Pervasives.compare end)
let targets = ref TargetSet.empty
let target_matchers = ref (fun (reason : BackOtterErrors.t) (job : Job.t) k -> (k reason job : BackOtterErrors.t option))


let get_line_targets file =
    TargetSet.fold begin fun (filename, linenumber as line) line_targets ->
        (* Use of_stmt_last to ensure that instructions before the last one are covered *)
        try
            List.map (fun (fundec, stmt) -> Instruction.of_stmt_last file fundec stmt) (FindCil.stmts_by_line file line) @ line_targets
        with Not_found ->
            let output_mode = Output.get_mode () in
            Output.set_mode Output.MSG_REG;
            Output.printf "Warning: line target %s:%d not found.@." filename linenumber;
            targets := TargetSet.remove line !targets;
            Output.set_mode output_mode;
            line_targets
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

    let target_matcher reason job k =
        let loc = Job.get_loc job in
        if loc.Cil.file = file && loc.Cil.line = line && error_matcher reason then begin
            Output.must_printf "BackOtter target reached %s@\n" string;
            let fundec = BackOtterUtilities.get_origin_function job in
            let entryfn = ProgramPoints.get_entry_fundec job#file in
            let failing_path = DecisionPath.rev job#decision_path in
            let instruction = Job.get_instruction job in
            let is_new_path = BackOtterTargets.add_path fundec failing_path (Some instruction) in
            if CilData.CilFundec.equal fundec entryfn then begin
                (* Remove instruction from line_targets *)
                Output.must_printf "Remove target %s:%d@\n" file line;
                targets := TargetSet.remove (file, line) !targets;
                (* Remove instruction-related failing paths and function targets from BackOtterTargets *)
                BackOtterTargets.remove_target_instruction instruction
            end;
            if is_new_path then Some (`FailingPath (reason, fundec, failing_path))
            else None
        end else
            k reason job
    in

    targets := TargetSet.add (file, line) !targets;
    let target_matchers' = !target_matchers in
    target_matchers := (fun reason job k -> target_matchers' reason job (fun reason job -> target_matcher reason job k))



class ['self] t delegate entry_fn =
object (_ : 'self)
    val delegate = delegate
    method report job_state =
        (* convert executions that report repeated abandoned paths to Truncated *)
        let job_state = match job_state with
            | Job.Complete (Job.Abandoned (`TargetReached target, job)) ->
                let fundec = BackOtterUtilities.get_origin_function job in
                let instruction = Job.get_instruction job in
                (* Failing path has least recent decision first. See the comment in BidirectionalQueue. *)
                let failing_path = DecisionPath.rev job#decision_path in
                let is_new_path = BackOtterTargets.add_path fundec failing_path (Some instruction) in
                if is_new_path then Job.Complete (Job.Abandoned (`FailingPath (`TargetReached target, fundec, failing_path), job))
                else Job.Complete (Job.Truncated (`SummaryAbandoned (`TargetReached target), job))
            | Job.Complete (Job.Abandoned (reason, job)) ->
                (* TODO: merge the above cases with target_matchers *)
                begin match !target_matchers reason job (fun _ _ -> None) with
                    | Some abandoned -> Job.Complete (Job.Abandoned (reason, job))
                    | None -> Job.Complete (Job.Truncated (`SummaryAbandoned reason, job))
                end
            | _ ->
                job_state
        in
        (* convert executions from non-entry functions to Truncated *)
        let job_state = match job_state with
            | Job.Complete (Job.Return (return_code, job))
                    when BackOtterUtilities.get_origin_function job != entry_fn ->
                Job.Complete (Job.Truncated (`SummaryReturn return_code, job))
            | Job.Complete (Job.Exit (return_code, job))
                    when BackOtterUtilities.get_origin_function job != entry_fn ->
                Job.Complete (Job.Truncated (`SummaryExit return_code, job))
            | Job.Complete (Job.Abandoned (reason, job))
                    when BackOtterUtilities.get_origin_function job != entry_fn ->
                Job.Complete (Job.Truncated (`SummaryAbandoned reason, job))
            | _ ->
                job_state
        in
        {< delegate = delegate#report job_state >}

    method should_continue = delegate#should_continue

    method completed = delegate#completed

    method delegate = delegate
end


let options = [
    "--backotter-target",
        Arg.String add_target,
        "<file,line[,reason[,reason-argument]> Add a target for BackOtter"
]

