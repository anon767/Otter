(** Depth-first Otter job queue. *)

class ['self] t = object (_ : 'self)
    (* depth-first is a stack *)
    val stack = []

    method put job =
        {< stack = job::stack >}

    method get = match stack with
        | job::rest -> Some ({< stack = rest >}, job)
        | [] -> None
end
