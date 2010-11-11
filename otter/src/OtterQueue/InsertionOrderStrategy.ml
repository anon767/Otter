(** Insertion-order Otter strategy: jobs are weighted by 1/n where n is the ranked insertion order of the job. *)

open DataStructures
open OtterCore


module JobOrder = PrioritySearchQueue.Make
    (struct type t = Job.job let compare x y = Pervasives.compare x.Job.jid y.Job.jid end)
    (struct type t = int let compare = Pervasives.compare end)


class ['self] t = object (_ : 'self)
    val queue = JobOrder.empty

    method add job =
        (* insert the job with rank = the rank of the most recent job - 1 *)
        let rank = try let _, rank, () = JobOrder.find_min queue in rank with JobOrder.Empty -> 0 in
        let queue = JobOrder.insert job (rank - 1) () queue in
        {< queue = queue >}

    method remove job =
        try
            (* remove the job and shift the rank of all jobs more recent than that job *)
            let rank = fst (JobOrder.lookup job queue) in
            let queue = JobOrder.delete job queue in
            let pre = JobOrder.at_most rank queue in
            let queue = List.fold_left (fun queue (job, _, ()) -> JobOrder.adjust (fun rank -> rank + 1) job queue) queue pre in
            {< queue = queue >}
        with JobOrder.Key ->
            raise Not_found

    method weights jobs =
        try
            (* job ranks are negative, so negate it for positive weights *)
            List.map (fun job -> -1. /. float_of_int (fst (JobOrder.lookup job queue))) jobs
        with JobOrder.Key ->
            raise Not_found
end

