(**/**)
let is_in_list = List.memq

let remove_from_list item lst = List.filter (fun ele -> ele != item) lst

let add_into_list item lst =
    let lst = remove_from_list item lst in
    item :: lst
(**/**)

(* Given a queue that supports put and get, add "remove" on top of it which takes the
 * job directly out from the queue.
 * Precondition: a job is entered into the queue only once. This is true for our use. *)
class ['job, 'queue] t queue = object
    val queue : 'queue = queue
    val contents = []

    method put (job : 'job) =
        {< queue = queue#put job; contents = job :: contents; >}

    method get =
        let rec get queue contents = match queue#get with
            | Some (queue, job) ->
                if not (is_in_list job contents) then
                    get queue contents
                else
                    Some ({< queue = queue;
                             contents = remove_from_list job contents; >}, job)
            | None -> assert(contents = []); None
        in
        get queue contents

    method remove (job : 'job) =
        {< contents = remove_from_list job contents; >}

    method get_contents = contents

end

