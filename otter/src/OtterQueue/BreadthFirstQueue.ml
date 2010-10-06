(** Breadth-first Otter job queue. *)

class ['job] t = object
    (* breadth-first is a queue *)
    val current = []
    val next = []

    method put (job : 'job) =
        {< next = job::next >}

    method get = match current with
        | job::rest -> Some ({< current = rest >}, job)
        | [] ->
            begin match List.rev next with
                | job::rest -> Some ({< current = rest; next = [] >}, job)
                | [] -> None
            end
end

let make () = new t