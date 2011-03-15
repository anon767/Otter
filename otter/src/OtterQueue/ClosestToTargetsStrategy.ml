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

    method weights jobs =
        List.map begin fun job ->
            let target_instr = Instruction.of_fundec job#file (ProgramPoints.get_failure_fundec job#file) in
            let call_sites = Instruction.call_sites target_instr in
            let source = Job.get_instruction job in
            let context = Job.get_instruction_context job in
            let distance = Distance.find_in_context (source, context, call_sites) in (* = max_int if call_sites = [] *)
            weight_of_distance distance
        end jobs
end


