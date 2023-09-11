(** Convert a prioritizer into a queue *)

open DataStructures

class ['self] t prioritizer = object (self : 'self)
    val prioritizer = prioritizer

    method put job = {< prioritizer = prioritizer#add job >}

    method remove job = {< prioritizer = prioritizer#remove job >}

    method get = OcamlUtilities.Profiler.global#call "PrioritizerRandomQueue.t#get" begin fun () ->
        match prioritizer#prioritize None with
        | [] -> None
        | jobs ->
            let maxw = List.fold_left (fun maxw (job, w) -> max maxw w) Pervasives.neg_infinity jobs in
            let jobs = List.filter (fun (job, w) -> w = maxw) jobs in
            let jobs = List.map (fun (job, w) -> job) jobs in
            let job = List.nth jobs (Random.int (List.length jobs)) in
            Some (self#remove job, job)
    end
end


