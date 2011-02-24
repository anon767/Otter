(** Closest-to-targets first strategy: jobs are weighted by 1/n where n is the shortest distance to a call to __FAILURE() or a target function. *)

open CilUtilities
open DataStructures
open OtterCFG
open OtterCore


class ['self] t = object (self : 'self)

    method add job = self

    method remove job = self

    method weight job =
        let max_distance = max_int in
        let get_distance_to_targets target_fundecs line_targets job =
            OcamlUtilities.Profiler.global#call "ClosestToTargetsStrategy.weight/get_distance_to_targets" begin fun () ->
                let source = Job.get_instruction job in
                let target_instrs = List.map (fun f -> Instruction.of_fundec job#file f) target_fundecs in
                let all_targets = target_instrs @ line_targets in
                if all_targets = [] then
                    max_distance
                else
                    let context = Job.get_instruction_context job in
                    DistanceToTargets.find_in_context source context all_targets
            end
        in
        let target_fundecs = BackOtterTargets.get_target_fundecs () in
        let line_targets = LineTargets.get_line_targets job#file in
        let distance = get_distance_to_targets target_fundecs line_targets job in
        1. /. float_of_int (distance)
end



