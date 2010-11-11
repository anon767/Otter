(** Generational Otter job queue, based on "Automated whitebox fuzz testing".

    This scheme works by first executing a job along a path, and assigning all forks of that job to the first
    generation. When the first job has completed, jobs are then executed from the first generation, assigning all
    forks from those jobs to the second generation, and so forth. Thus, this scheme is biased towards jobs that are
    close to jobs that were completed earlier.

    P. Godefroid, M. Y. Levin, and D. Molnar. Automated whitebox fuzz testing. In Proceedings of NDSS '08 (Network and
    Distributed Systems Security), pages 151--166, 2008.

    @see <https://www.isoc.org/isoc/conferences/ndss/08/papers/10_automated_whitebox_fuzz.pdf>
    https://www.isoc.org/isoc/conferences/ndss/08/papers/10_automated_whitebox_fuzz.pdf
 *)

open DataStructures
open OtterCore

module JobGeneration = PrioritySearchQueue.Make
    (struct type t = Job.job let compare x y = Pervasives.compare x.Job.jid y.Job.jid end)
    (struct type t = int let compare = Pervasives.compare end)


class ['self] t = object (self : 'self)
    val queue = JobGeneration.empty
    val next = 1

    method add job =
        (* jobs are added to the 0th generation *)
        let queue = JobGeneration.insert job 0 () queue in
        {< queue = queue >}

    method remove job =
        try
            let generation, () = JobGeneration.lookup job queue in
            let queue = JobGeneration.delete job queue in
            if generation = 0 then
                (* move the rest of the 0th generation to the next generation *)
                let gen0 = JobGeneration.at_most 0 queue in
                let queue = List.fold_left (fun queue (job, _, ()) -> JobGeneration.adjust (fun _ -> next) job queue) queue gen0 in
                {< queue = queue >}
            else
                (* no 0th generation jobs remaining (i.e. a job has run to completion), update the next generation *)
                let next = generation + 1 in
                {< queue = queue; next = next >}
        with JobGeneration.Key ->
            raise Not_found

    method weights jobs =
        try
            let _, generation, () = JobGeneration.find_min queue in
            if generation = 0 then
                (* if there are 0th generation jobs, pick one and force it to completion *)
                let gen0 = JobGeneration.at_most 0 queue in
                let job0, _, () = List.nth gen0 (Random.int (List.length gen0)) in
                List.map (fun job -> if job.Job.jid = job0.Job.jid then 1. else 0.) jobs
            else
                (* pick any one of the current generation *)
                List.map (fun job -> if fst (JobGeneration.lookup job queue) = generation then 1. else 0.) jobs
        with JobGeneration.Key | JobGeneration.Empty ->
            raise Not_found
end

