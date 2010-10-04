(** Best-first Otter job queue. *)
open DataStructures

class ['job] t prioritizer max_priority = object
    (* best-first is a (min) priority queue 
     * Any job with priority higher than max_priority will be dropped 
     *)
    val heap = Heap.empty

    method put (job : 'job) =
        let priority : float = prioritizer job in
        if priority > max_priority then
            {<>}
        else
            {< heap = Heap.insert (job, priority) heap >}

    method get = if Heap.is_empty heap then None else
        let (job, _) = Heap.find_min heap in
        Some ({< heap = Heap.delete_min heap >}, job)
end

let default_prioritizer job = 0.0
let default_max_priority = 1.0

let make prioritizer max_priority = new t prioritizer max_priority
let make_default () = make default_prioritizer default_max_priority
