open BackOtterUtilities
open DataStructures
open OtterCore
open Job

class ['job] t entry_fn failure_fn targets_ref = object
    val queue = []

    method put (job : 'job) =
        {< queue = job :: queue >}

    method get =
        if queue = [] then None else
        let targets = !targets_ref in
        let target_fundecs = failure_fn :: (BackOtterTargets.get_fundecs targets) in
        let score job =
            let num_of_failing_paths = List.length (BackOtterTargets.get (get_origin_function job) targets) in
            let execution_path_length = List.length job.exHist.executionPath in
            let distance_from_entryfn = get_distance_from job.Job.file entry_fn (get_origin_function job) in
            let distance_to_target = get_distance_to_targets target_fundecs job in
            (* First pick the one closest to entry_fn, then the one closest to a target, then the most explored one *)
            [-num_of_failing_paths; -distance_from_entryfn; -distance_to_target; execution_path_length]
        in
        let queue, job = get_job_with_highest_score score queue in
        Some ({< queue = queue; >}, job)

end

