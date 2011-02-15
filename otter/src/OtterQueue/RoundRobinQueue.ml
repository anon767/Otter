(** Round-robin Otter job queue.

    Job are chosen from a list of queues in rotation.
*)

open DataStructures

module Queue = struct
    type 'a t = { front: 'a list; back: 'a list }
    let empty = { front = []; back = [] }
    let from_list xs = { front = xs; back = [] }
    let enqueue x q = { q with back = x::q.back }
    let dequeue q =
        let q = if q.front = [] then { front = List.rev q.back; back = [] } else q in
        match q.front with
            | x::xs -> ({ q with front = xs }, x)
            | [] -> raise Not_found
    let map f q =
        { front = List.rev_append (List.rev_map f q.front) (List.rev_map f q.back); back = [] }
end

class ['self] t queues = object (self : 'self)
    initializer if List.length queues < 2 then invalid_arg "RoundRobinQueue.t: less than 2 queues"

    val queues = Queue.from_list queues

    method put job =
        {< queues = Queue.map (fun queue -> queue#put job) queues >}

    method remove job =
        {< queues = Queue.map (fun queue -> queue#remove job) queues >}

    method get = OcamlUtilities.Profiler.global#call "RoundRobinQueue.t#get" begin fun () ->
        let queues, queue = Queue.dequeue queues in
        match queue#get with
            | Some (queue, job) ->
                let queues = Queue.map (fun queue -> queue#remove job) queues in
                let queues = Queue.enqueue queue queues in
                Some ({< queues = queues >}, job)
            | None ->
                None
    end
end

