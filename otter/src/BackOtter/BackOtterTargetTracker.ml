open OcamlUtilities
open CilUtilities
open OtterCFG
open OtterCore
open Job
open Cil

(* TODO: replace LineTargets with the below *)
module TargetSet = Set.Make (struct type t = string * int let compare = Pervasives.compare end)
let targets = ref TargetSet.empty
let target_matchers =
    ref (fun (reason : BackOtterErrors.t) (job : (BackOtterErrors.t, BackOtterErrors.t) Job.t) k -> k reason job)


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



let process_completed entry_fn (reason, job) =
    (* convert executions that report repeated abandoned paths to Truncated *)
    let reason = match reason with
        | Job.Abandoned (`TargetReached target) ->
            let fundec = BackOtterUtilities.get_origin_function job in
            let instruction = Job.get_instruction job in
            (* Failing path has least recent decision first. See the comment in BidirectionalQueue. *)
            let failing_path = DecisionPath.rev job#decision_path in
            let is_new_path = BackOtterTargets.add_path fundec failing_path (Some instruction) in
            if is_new_path then Job.Abandoned (`FailingPath (`TargetReached target, fundec, failing_path))
            else Job.Truncated (`SummaryAbandoned (`TargetReached target))
        | Job.Abandoned abandoned ->
            (* TODO: merge the above cases with target_matchers *)
            begin match !target_matchers abandoned job (fun _ _ -> None) with
                | Some abandoned -> Job.Abandoned abandoned
                | None -> Job.Truncated (`SummaryAbandoned abandoned)
            end
        | _ ->
            reason
    in

    (* convert executions from non-entry functions to Truncated *)
    let reason = match reason with
        | Job.Return return_code
                when BackOtterUtilities.get_origin_function job != entry_fn ->
            Job.Truncated (`SummaryReturn return_code)
        | Job.Exit return_code
                when BackOtterUtilities.get_origin_function job != entry_fn ->
            Job.Truncated (`SummaryExit return_code)
        | Job.Abandoned reason
                when BackOtterUtilities.get_origin_function job != entry_fn ->
            Job.Truncated (`SummaryAbandoned reason)
        | _ ->
            reason
    in

    (job : _ #Info.t)#finish reason



let options = [
    "--backotter-target",
        Arg.String add_target,
        "<file,line[,reason[,reason-argument]> Add a target for BackOtter"
]

