(** Best-first Otter job queue. *)
open DataStructures

type rank =
    | Rank of float
    | Drop of string 

class ['self] t prioritizer = object (_ : 'self)
    (* best-first is a (min) priority queue *)
    val heap = Heap.empty

    method put job =
        match prioritizer job with
        | Rank (rank) ->
            {< heap = Heap.insert (job, rank) heap >}
        | Drop (msg) ->
            Format.printf "%s@\n" msg;
            {<>}

    method get = if Heap.is_empty heap then None else
        let (job, _) = Heap.find_min heap in
        Some ({< heap = Heap.delete_min heap >}, job)
end
