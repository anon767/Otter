open OcamlUtilities
open CilUtilities
open OtterCFG
open OtterCore
open Job
open Cil

(* TODO: replace LineTargets with the below *)
module TargetSet = Set.Make (struct type t = string * int let compare = Pervasives.compare end)
let targets = ref TargetSet.empty


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

