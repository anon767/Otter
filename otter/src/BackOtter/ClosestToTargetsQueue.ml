open BackOtterUtilities
open DataStructures
open OtterCore
open Job

(* TODO: improve the framework of BackOtterTargets.
 * 1. Maintain a set of new targets/failing paths since last iteration.
 * 2. Update only jobs that are affected by the new changes.
 * 3. This queue can then be implemented as a priority queue with update key.
 *)
class ['job] t targets_ref = object
    val queue = []

    method put (job : 'job) =
        {< queue = job :: queue >}

    method get =
        let target_fundecs = BackOtterTargets.get_fundecs (!targets_ref) in
        let score job =
            let distance_to_target = get_distance_to_targets target_fundecs job in
            -distance_to_target
        in
        if queue = [] then None else
        (* get_job_with_highest_score will break tie by random. *)
        let queue, job = get_job_with_highest_score score queue in
        Some ({< queue = queue; >}, job)

end

