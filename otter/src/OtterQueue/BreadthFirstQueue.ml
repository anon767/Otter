(** Breadth-first Otter job queue. *)

class ['self] t = object (_ : 'self)
    (* breadth-first is a queue *)
    val current = []
    val next = []

    method put job =
        {< next = job::next >}

    method get = match current with
        | job::rest -> Some ({< current = rest >}, job)
        | [] ->
            begin match List.rev next with
                | job::rest -> Some ({< current = rest; next = [] >}, job)
                | [] -> None
            end
end
