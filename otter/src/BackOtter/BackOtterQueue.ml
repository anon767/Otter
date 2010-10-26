(* A queue that ranks based on
 * 1. Whether a job is on bounding paths
 * 2. Distance to targets
 * 3. Whether the function is main(), i.e., forward search.
 *    Do breath-first in this case.
 * 4. (Optional) How long a function has run (to avoid get stuck)
 *
 * TODO: optimize findmin
 *)
open OtterCore
open Types
open Job

class ['job] t targets_ref entry_fn = object
    val jobs = []
    val forward_search = false
    val high_score = infinity
    val base_score = 100.0
    val low_score = neg_infinity

    method put (job : 'job) =
        {< jobs = job::jobs >}

    method get =
        let distance_to_target job targets =
            (* TODO: bring in the distance calculation.
             * Also bring in the min-cost estimate for each function call *)
            0
        in
        let score_function job =
            let origin_function = List.hd (List.rev job.state.callstack) in
            if forward_search then
                if origin_function == entry_fn then
                    -. (float_of_int (List.length job.decisionPath)) (* Almost BFS *)
                else
                    low_score
            else
                if origin_function == entry_fn then
                    low_score
                else
                    let distance = float_of_int (distance_to_target job (!targets_ref)) in
                    match job.boundingPaths with
                    | Some(_::_) -> base_score *. 2.0 -. distance (* bounding paths have higher importance *)
                    | Some([]) -> low_score
                    | _ -> base_score -. distance
        in
        let rest_jobs, returned_job_opt, _ = List.fold_left (
            fun (rest_jobs, returned_job_opt, score) job ->
                let new_score = score_function job in
                match returned_job_opt with
                | None -> rest_jobs, Some job, new_score
                | Some (returned_job) ->
                    if new_score > score then
                        returned_job :: rest_jobs, Some job, new_score
                    else
                        job :: rest_jobs, Some returned_job, score
        ) ([], None, 0.0) jobs
        in
        match returned_job_opt with
        | None -> None
        | Some (returned_job) ->
            Some ({< jobs = rest_jobs ; forward_search = not forward_search >}, returned_job)
end
