(** Rank by strategies Otter job queue.

    Jobs are ranked using a list of strategies to weigh jobs in the following manner: jobs are weighed by the first
    strategy and the highest weighted jobs filtered. If only one job remains, then it is returned; otherwise, if there
    are more than one job tied with the same highest weight, they are then weighed by the second strategy and the
    highest weighted jobs filtered again, and so on, until the list of strategies is exhausted, in which case a job is
    choosen randomly from the remaining jobs and returned.
*)

open DataStructures

module JobSet = struct
    module M = Map.Make (struct type t = int let compare = Pervasives.compare end)
    let empty = M.empty
    let is_empty = M.is_empty
    let remove job jobs = M.remove job#path_id jobs
    let add job jobs = M.add job#path_id job jobs
    let elements jobs = M.fold (fun _ job jobs -> job::jobs) jobs []
end


class ['self] t strategies = object (self : 'self)
    val jobs = JobSet.empty
    val strategies = strategies

    method put job =
        let jobs = JobSet.add job jobs in
        let strategies = List.map (fun strategy -> strategy#add job) strategies in
        {< jobs = jobs; strategies = strategies >}

    method remove job =
        let jobs = JobSet.remove job jobs in
        let strategies = List.map (fun strategy -> strategy#remove job) strategies in
        {< jobs = jobs; strategies = strategies >}

    method get = OcamlUtilities.Timer.time "RankedQueue.t#get" begin fun () ->
        if JobSet.is_empty jobs then
            None
        else
            (* helper to select the highest weighted jobs using a strategy *)
            let find_max_jobs strategy jobs =
                fst begin List.fold_left begin fun (max_jobs, max_weight) job ->
                    let weight = strategy#weight job in
                    if weight > max_weight then
                        ([ job ], weight)
                    else if weight = max_weight then
                        (job::max_jobs, max_weight)
                    else
                        (max_jobs, max_weight)
                end ([], neg_infinity) jobs end
            in
            (* helper to find the highest weighted job using a list of strategies in order *)
            let rec find_max jobs strategies = match jobs, strategies with
                | [ job ], _ ->
                    job
                | jobs, strategy::strategies ->
                    find_max (find_max_jobs strategy jobs) strategies
                | jobs, [] ->
                    (* more than one job remains after applying all strategies: choose one at random *)
                    List.nth jobs (Random.int (List.length jobs))
            in
            let job = find_max (JobSet.elements jobs) strategies in
            Some (self#remove job, job)
    end ()
end

