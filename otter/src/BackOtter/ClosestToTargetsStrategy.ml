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
        let get_distance_to_targets target_fundecs job =
            OcamlUtilities.Profiler.global#call "ClosestToTargetsStrategy.weight/get_distance_to_targets" begin fun () ->
                if target_fundecs = [] then
                    max_distance (* = max_int in DistanceToTargets *)
                else
                    let source = Job.get_instruction job in
                    let target_instrs = List.map (fun f -> Instruction.of_fundec job#file f) target_fundecs in
                    let context = Job.get_instruction_context job in
                    DistanceToTargets.find_in_context source context target_instrs
            end
        in
        let target_fundecs = BackOtterTargets.get_target_fundecs () in
        (* target_fundecs includes failure_fn *)
        let distance = get_distance_to_targets target_fundecs job in
        1. /. float_of_int (distance)
end



