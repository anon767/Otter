(** Depth-first Otter job queue. *)

class ['job] t = object
    (* depth-first is a stack *)
    val stack = []

    method put (job : 'job) =
        {< stack = job::stack >}

    method get = match stack with
        | job::rest -> Some ({< stack = rest >}, job)
        | [] -> None
end

let make () = new t
