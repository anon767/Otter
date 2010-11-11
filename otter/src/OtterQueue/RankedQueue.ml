(** Rank by strategies Otter job queue.

    Jobs are ranked using a list of strategies to weigh jobs in the following manner: jobs are weighed by the first
    strategy and the highest weighted jobs filtered. If only one job remains, then it is returned; otherwise, if there
    are more than one job tied with the same highest weight, they are then weighed by the second strategy and the
    highest weighted jobs filtered again, and so on, until the list of strategies is exhausted, in which case a job is
    choosen randomly from the remaining jobs and returned.
*)

open DataStructures
open OtterCore

module JobSet = Set.Make (struct type t = Job.job let compare x y = Pervasives.compare x.Job.jid y.Job.jid end)


class ['self] t strategies = object (_ : 'self)
    val jobs = JobSet.empty
    val strategies = strategies

    method put job =
        let jobs = JobSet.add job jobs in
        let strategies = List.map (fun strategy -> strategy#add job) strategies in
        {< jobs = jobs; strategies = strategies >}

    method get =
        if JobSet.is_empty jobs then
            None
        else
            (* helper to select the highest weighted jobs using a strategy *)
            let find_max_jobs strategy jobs =
                let weights = strategy#weights jobs in
                fst begin List.fold_left2 begin fun (max_jobs, max_weight) job weight ->
                    if weight > max_weight then
                        ([ job ], weight)
                    else if weight = max_weight then
                        (job::max_jobs, max_weight)
                    else
                        (max_jobs, max_weight)
                end ([], neg_infinity) jobs weights end
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
            let jobs = JobSet.remove job jobs in
            let strategies = List.map (fun strategy -> strategy#remove job) strategies in
            Some ({< jobs = jobs; strategies = strategies >}, job)
end

