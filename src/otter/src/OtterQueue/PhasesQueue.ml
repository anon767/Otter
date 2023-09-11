(** [queues] is a [(queue, live)] list. The first [queue] is used until [live ()] return false, in which case it switches to the second [queue], and so on. *)

class ['self] t queues = object (self : 'self)
    initializer if List.length queues < 2 then invalid_arg "PhasesQueue.t: less than 2 queues"

    val queues = queues

    method put job =
        match self#chop queues with
        | [] -> invalid_arg "PhasesQueue.t#put: no more queues"
        | (queue, live) :: queues -> {< queues = (queue#put job, live) :: queues >}

    method remove job =
        match self#chop queues with
        | [] -> invalid_arg "PhasesQueue.t#remove: no more queues"
        | (queue, live) :: queues -> {< queues = (queue#remove job, live) :: queues >}

    method get = OcamlUtilities.Profiler.global#call "PhasesQueue.t#get" begin fun () ->
        match self#chop queues with
        | [] -> invalid_arg "PhasesQueue.t#get: no more queues"
        | (queue, live) :: queues ->
            match queue#get with
                | Some (queue, job) ->
                    Some ({< queues = (queue, live) :: queues >}, job)
                | None ->
                    None
    end

    method private chop queues =
        let rec chop = function
            | [] -> invalid_arg "PhasesQueue.t#chop: no more queues (1)"
            | ((queue, live) :: queues') as queues ->
                if live () then queues else 
                    let _ = OcamlUtilities.Output.must_printf "PhasesQueue: switch queue@." in
                    chop queues'
        in
        let rec transfer q_src q_des =
            match q_src#get with
            | None -> q_des
            | Some(q_src, job) -> transfer q_src (q_des#put job)
        in
        match queues with
        | [] -> invalid_arg "PhasesQueue.t#chop: no more queues (2)"
        | (queue', _) :: queues' ->
            match chop queues with
            | [] -> invalid_arg "PhasesQueue.t#chop: no more queues (3)"
            | (queue'', live) :: queues'' ->
                let queue = if queue' == queue'' then queue'' else transfer queue' queue'' in
                (queue, live) :: queues''

end

let immortal () = true
