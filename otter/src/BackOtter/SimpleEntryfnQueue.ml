open BackOtterUtilities
open DataStructures
open OtterCore
open Job

class ['job] t target_fundecs = object
    val queue = []

    method put (job : 'job) =
        {< queue = job :: queue >}

    method get =
        let score job =
            (* When some targets are found in the job's current function (i.e., close enough), we bias towards them. *)
            (* TODO: sometimes the above strategy may not work well.... *)
            let distance_to_target = get_distance_to_targets_within_function target_fundecs job in
            let path_length = (List.length job.decisionPath) in (* Approximated execution path length *)
            [-distance_to_target; -path_length]
        in
        let queue, job = get_job_with_highest_score score queue in
        Some ({< queue = queue; >}, job)

end

