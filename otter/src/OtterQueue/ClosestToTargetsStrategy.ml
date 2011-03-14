(** Closest-to-targets first strategy: jobs are weighted by 1/n where n is the shortest distance to a call to __FAILURE(). *)

open CilUtilities
open DataStructures
open OtterCFG
open OtterCore

(* Abstract distance *)
let weight_of_distance distance =
    if distance < 5 then 3.0
    else if distance < max_int then 2.0
    else 1.0


class ['self] t = object (self : 'self)

    method add job = self

    method remove job = self

    method weight job =
        let get_distance_to_targets target_fundecs job =
            if target_fundecs = [] then
                max_int (* = max_int in DistanceToTargets *)
            else
                let source = Job.get_instruction job in
                let target_instrs = List.map (fun f -> Instruction.of_fundec job#file f) target_fundecs in
                let context = Job.get_instruction_context job in
                Distance.find_in_context (source, context, target_instrs)
        in
        let file = job#file in
        let failure_fn = ProgramPoints.get_failure_fundec file in
        let distance = get_distance_to_targets [failure_fn] job in
        weight_of_distance distance
end


