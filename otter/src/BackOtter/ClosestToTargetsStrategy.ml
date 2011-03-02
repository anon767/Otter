(** Closest-to-targets first strategy: jobs are weighted by 1/n where n is the shortest distance to a call to __FAILURE() or a target function. *)

open OcamlUtilities
open CilUtilities
open DataStructures
open OtterCFG
open OtterCore


let get_distance_to_targets =
    let max_distance = max_int in
    let module Memo = Memo.Make (struct
        type t = (CilData.CilFundec.t list) * (Instruction.t list) * Job.t
        (* TODO: factor out the list comparison and hasing *)
        let equal (f1, l1, j1) (f2, l2, j2) = 
            j1 == j2 &&
            List.length f1 = List.length f2 && 
            List.fold_left2 (fun b i1 i2 -> CilData.CilFundec.equal i1 i2 && b) true f1 f2 &&
            List.length l1 = List.length l2 && 
            List.fold_left2 (fun b i1 i2 -> Instruction.equal i1 i2 && b) true l1 l2 
        let hash (f, l, j) = List.fold_left (fun h i -> h + CilData.CilFundec.hash i) 0 f + List.fold_left (fun h i -> h + Instruction.hash i) 0 l + j#jid_unique
    end) in
    Memo.memo "ClosestToTargetsStrategy.get_distance_to_targets" (fun (target_fundecs, line_targets, job) ->
        Profiler.global#call "ClosestToTargetsStrategy.get_distance_to_targets" begin fun () ->
            let source = Job.get_instruction job in
            let current_fundec = List.hd job#state.State.callstack in
            let target_instrs = List.fold_left (fun lst f -> (Instruction.call_sites_in_caller (job#file, current_fundec, f)) @ lst) [] target_fundecs in
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
        let distance = get_distance_to_targets (target_fundecs, line_targets, job) in
        Output.debug_printf "Job %d has distance to target = %d@\n" job#node_id distance;
        1. /. float_of_int (distance)
end



