(** Closest-to-targets first strategy: jobs are weighted by 1/n where n is the shortest distance to a call to __FAILURE() or a target function. *)

open OcamlUtilities
open CilUtilities
open DataStructures
open OtterCFG
open OtterCore


let get_distance_to_targets =
    let max_distance = max_int in
    let module Memo = Memo.Make (struct
        module F = ListPlus.MakeHashedList (CilData.CilFundec)
        module I = ListPlus.MakeHashedList (Instruction)
        type t = Job.t * CilData.CilFundec.t list * Instruction.t list
        let equal (j1, f1, i1) (j2, f2, i2) = j1 == j2 && F.equal f1 f2 && I.equal i1 i2
        let hash (j, f, i) = Hashtbl.hash (j#jid_unique, F.hash f, I.hash i)
    end) in
    Memo.memo "ClosestToTargetsStrategy.get_distance_to_targets" (fun (job, target_fundecs, line_targets) ->
        Profiler.global#call "ClosestToTargetsStrategy.get_distance_to_targets" begin fun () ->
            let source = Job.get_instruction job in
            let current_fundec = List.hd job#state.State.callstack in
            let target_instrs = List.fold_left (fun lst f -> (Instruction.call_sites_in_caller job#file current_fundec f) @ lst) [] target_fundecs in
            let all_targets = target_instrs @ line_targets in
            if all_targets = [] then
                max_distance
            else
                let context = Job.get_instruction_context job in
                Distance.find_in_context (source, context, all_targets)
        end)

class ['self] t = object (self : 'self)

    method add job = self

    method remove job = self

    method weight job =
        let target_fundecs = BackOtterTargets.get_target_fundecs () in
        let line_targets = LineTargets.get_line_targets job#file in
        let distance = get_distance_to_targets (job, target_fundecs, line_targets) in
        Output.debug_printf "Job %d has distance to target = %d@\n" job#node_id distance;
        1. /. float_of_int (distance)
end



