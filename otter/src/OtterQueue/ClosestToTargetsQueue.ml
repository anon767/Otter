(** Closest-to-targets queue for (forward) Otter.
 *  It simply returns the job that has the closest __FAILURE().
 *)

open DataStructures
open CilUtilities
open OtterCFG
open OtterCore

module JobKey = struct
    type t = Job.job
    let compare = Pervasives.compare
end

module JobPriority = PrioritySearchQueue.Make
    (JobKey)
    (struct type t = int let compare = Pervasives.compare end)

(* TODO: factor this and the one in BackOtterUtilities out to somewhere *)
let get_distance_to_targets target_fundecs job =
    let get_distance_to_targets () =
        if target_fundecs = [] then
            max_int (* = max_int in DistanceToTargets *)
        else
            let source = Job.get_instruction job in
            let target_instrs = List.map (fun f -> Instruction.of_fundec job.Job.file f) target_fundecs in
            let context = Job.get_instruction_context job in
            DistanceToTargets.find_in_context source context target_instrs
    in
    Stats.time "BackOtterUtilities.get_distance_to_targets" get_distance_to_targets ()

class ['self] t = object (self : 'self)

    val queue = JobPriority.empty


    method put job =
        (* TODO: can this be precalculated somewhere? *)
        let file = job.Job.file in
        let failure_fn =
            let fname = !Executeargs.arg_failurefn in
            try FindCil.fundec_by_name file fname
            with Not_found -> OcamlUtilities.FormatPlus.failwith "Failure function %s not found" fname
        in
        let distance = get_distance_to_targets [failure_fn] job in
        let queue = JobPriority.insert job distance job queue in
        {< queue = queue >}

    method get =
        try
            let job, _, _ = JobPriority.find_min queue in
            let queue = JobPriority.delete_min queue in
            Some ({< queue = queue >}, job)
        with JobPriority.Empty ->
            None
end

