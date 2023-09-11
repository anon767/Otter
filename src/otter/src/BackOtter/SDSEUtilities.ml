open OcamlUtilities
open CilUtilities
open OtterCFG
open OtterCore


(* Note: get_distance_internal is not polymorphic due to "value restriction" *)
let get_distance_internal = ClosestToTargetsStrategy.get_distances

let get_distance ?interprocedural line_targets job =
    Profiler.global#call "BackOtter.ClosestToTargetsStrategy.get_distance" begin fun () ->
        let source = Job.get_instruction job in
        let context = Job.get_instruction_context job in
        let distance = get_distance_internal ?interprocedural source context (BackOtterTargets.get_target_fundecs ()) line_targets in
        Output.debug_printf "Job %d has distance to target = %d@\n" job#node_id distance;
        distance
    end

let interSDSE_score line_targets job = OtterQueue.ClosestToTargetsStrategy.inversely_proportional (get_distance line_targets job)

