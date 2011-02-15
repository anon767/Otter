(** Otter job queue that runs a job up to a certain number of steps or to a fork before choosing another job from an
    underlying queue. *)

open DataStructures


class ['self] t ?(limit=20) queue = object (self : 'self)
    val steps = 0
    val work = `None
    val queue = queue

    method put job =
        let work = match work with
            | `None -> `One job
            | `One _ -> `Many
            | `Many -> `Many
        in
        let queue = queue#put job in
        {< work = work; queue = queue >}

    method remove job =
        let work = match work with
            | `One job' when job == job' -> `None
            | _ -> work
        in
        let queue = queue#remove job in
        {< work = work; queue = queue >}

    method get = OcamlUtilities.Timer.time "BatchQueue.t#get" begin fun () ->
        match work with
            | `One job when steps < limit ->
                let queue = queue#remove job in
                Some ({< work = `None; queue = queue; steps = steps + 1 >}, job)
            | _ ->
                begin match queue#get with
                    | Some (queue, job) ->
                        Some ({< work = `None; queue = queue; steps = 0 >}, job)
                    | None ->
                        None
                end
    end ()
end

