(** Closest-to-targets first strategy: jobs are weighted by 1/n where n is the shortest distance to a call to __FAILURE() or a target function. *)

open OcamlUtilities
open CilUtilities
open OtterCFG
open OtterCore


let get_distances = ClosestToTargetsStrategy.get_distances

(** Approximation of round-robin *)
let weight ?interprocedural weight_fn count job =
    Profiler.global#call "BackOtter.ClosestToTargetsStrategy.weight" begin fun () ->
        let source = Job.get_instruction job in
        let file = source.Instruction.file in
        let context = Job.get_instruction_context job in
        let line_targets = (BackOtterTargetTracker.get_line_targets file) in
        let the_line_target = if line_targets = [] then [] else [List.nth line_targets (count mod (List.length line_targets))] in
        let distance = get_distances ?interprocedural source context (BackOtterTargets.get_target_fundecs ()) the_line_target in
        Output.debug_printf "Job %d has distance to target = %d@\n" job#node_id distance;
        weight_fn distance
    end

class ['self] t ?interprocedural weight_fn = object (self : 'self)
    val mutable count = 0

    method add job = self

    method remove job = self

    method weights jobs = 
        let r = List.map (weight ?interprocedural weight_fn count) jobs in
        count <- count + 1; r
end

