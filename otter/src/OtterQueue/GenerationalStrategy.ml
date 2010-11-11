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
            (* dequeue the job *)
            let generation, () = JobGeneration.lookup job queue in
            let queue = JobGeneration.delete job queue in

            (* move the (rest of the) 0th generation to the next generation *)
            let gen0 = JobGeneration.at_most 0 queue in
            let queue = List.fold_left (fun queue (job, _, ()) -> JobGeneration.adjust (fun _ -> next) job queue) queue gen0 in

            (* if no 0th generation jobs remained (i.e. a job has run to completion), update the next generation *)
            let next = if generation <> 0 then generation + 1 else next in
            {< queue = queue; next = next >}
        with JobGeneration.Key ->
            raise Not_found

    method weights jobs =
        try
            (* pick any one of the most recent generation *)
            let _, generation, () = JobGeneration.find_min queue in
            List.map (fun job -> if fst (JobGeneration.lookup job queue) = generation then 1. else 0.) jobs
        with JobGeneration.Key | JobGeneration.Empty ->
            raise Not_found
end

