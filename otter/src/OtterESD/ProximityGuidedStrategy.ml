(** Proximity-guided strategy as in ESD *)

open CilUtilities
open DataStructures
open OtterCFG
open OtterCore


class ['self] t weight_fn = object (self : 'self)

    method add job = self

    method remove job = self

    method weights jobs =
        List.map begin fun job ->
            let target_instr = Instruction.of_fundec job#file (ProgramPoints.get_failure_fundec job#file) in
            let call_sites = Instruction.call_sites target_instr in
            let intermediate_goals = List.fold_left (
                fun lst call_site -> match IntermediateGoals.find call_site with Some goal -> goal :: lst | None -> lst
            ) [] call_sites in
            let source = Job.get_instruction job in
            let context = Job.get_instruction_context job in
            let distance = Distance.find_in_context (source, context, intermediate_goals @ call_sites)  in
            weight_fn distance
        end jobs
end


