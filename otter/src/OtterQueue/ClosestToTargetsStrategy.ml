(** Closest-to-targets first strategy: jobs are weighted by 1/n where n is the shortest distance to a call to __FAILURE(). *)

open CilUtilities
open DataStructures
open OtterCFG
open OtterCore


class ['self] t = object (self : 'self)

    method add job = self

    method remove job = self

    method weight job =
        let get_distance_to_targets target_fundecs job =
            if target_fundecs = [] then
                max_int (* = max_int in DistanceToTargets *)
            else
                let source = Job.get_instruction job in
                let target_instrs = List.map (fun f -> Instruction.of_fundec job.Job.file f) target_fundecs in
                let context = Job.get_instruction_context job in
                DistanceToTargets.find_in_context source context target_instrs
        in
        let file = job.Job.file in
        let failure_fn =
            let fname = !Executeargs.arg_failurefn in
            try FindCil.fundec_by_name file fname
            with Not_found -> OcamlUtilities.FormatPlus.failwith "Failure function %s not found" fname
        in
        let distance = get_distance_to_targets [failure_fn] job in
        1. /. float_of_int (distance)
end


