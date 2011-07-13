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

module JobSet = Set.Make (struct type t = int let compare = Pervasives.compare end)
module JobGeneration = PrioritySearchQueue.Make
    (struct type t = int let compare = Pervasives.compare end)
    (struct type t = int let compare = Pervasives.compare end)


class ['self] t = object (self : 'self)
    (* it's much cheaper to keep work separate from queue since work is often queried and queue can be much larger *)
    val work = JobSet.empty
    val queue = JobGeneration.empty
    val next = 1

    method add job =
        let work = JobSet.add job#node_id work in
        {< work = work >}

    method remove job =
        (* dequeue the job *)
        let next', work, queue =
            if JobSet.mem job#node_id work then
                let work = JobSet.remove job#node_id work in
                (next, work, queue)
            else
                try
                    let generation, () = JobGeneration.lookup job#node_id queue in
                    let queue = JobGeneration.delete job#node_id queue in
                    (generation + 1, work, queue)
                with JobGeneration.Key ->
                    raise Not_found
        in

        (* move the rest of work to the next generation *)
        let queue = JobSet.fold (fun jid queue -> JobGeneration.insert jid next () queue) work queue in
        let work = JobSet.empty in

        (* update the next generation *)
        let next = next' in

        {< work = work; queue = queue; next = next >}

    method weights jobs =
        (* pick any one of work or the most recent generation *)
        if JobSet.is_empty work then
            try
                let _, generation, () = JobGeneration.find_min queue in
                List.map (fun job -> if fst (JobGeneration.lookup job#node_id queue) = generation then 1. else 0.) jobs
            with JobGeneration.Key | JobGeneration.Empty ->
                raise Not_found
        else
            List.map (fun job -> if JobSet.mem job#node_id work then 1. else 0.) jobs
end

