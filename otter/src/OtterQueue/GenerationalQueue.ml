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

class ['job] t empty_generation = object
    val work = RandomBag.empty
    val current = empty_generation
    val next = empty_generation

    method put (job : 'job) =
        {< work = RandomBag.put job work >}

    method get =
        match RandomBag.get work with
            | None ->
                begin match current#get with
                    | Some (current, job) ->
                        (* pick from the current generation *)
                        Some ({< current = current >}, job)
                    | None ->
                        begin match next#get with
                            | Some (next, job) ->
                                (* move next generation to current generation, and pick from it *)
                                Some ({< current = next; next = empty_generation >}, job)
                            | None ->
                                None
                        end
                end
            | Some (work, job) ->
                (* pick randomly from the working set, and move the rest into the next generation *)
                let next = RandomBag.fold (fun x next -> next#put x) work next in
                Some ({< work = RandomBag.empty; next = next >}, job)
end

let make empty_generation = new t empty_generation
