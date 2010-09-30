(** Breadth-first Otter job queue. *)

class ['job] t = object
    (* breadth-first is a queue *)
    val current = []
    val next = []

    method put (job : 'job) =
        {< next = job::next >}

    method get = match current, next with
        | job::rest, _ -> Some ({< current = rest >}, job)
        | [], job::rest -> Some ({< current = rest; next = [] >}, job)
        | [], [] -> None
end
