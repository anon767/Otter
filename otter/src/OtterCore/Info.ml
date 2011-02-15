(** Information about paths and nodes in an execution tree. *)

open DataStructures

class t :
    object ('self)
        method path_id : int
        method node_id : int
        method parent_path_id : int
        method parent_node_id : int
        method parent_list : (int * int) list
        method fork : 'a 'b . ('self -> 'a -> 'b -> 'b) -> 'a list -> 'b -> 'b
        method become : 'self -> unit
    end
=
    let path_counter = Counter.make () in
    let node_counter = Counter.make () in
    object (self : 'self)
        val mutable path_id = Counter.next path_counter
        val mutable node_id = Counter.next node_counter
        val mutable parent_list = []

        method path_id = path_id
        method node_id = node_id
        method parent_path_id = fst (List.hd parent_list)
        method parent_node_id = snd (List.hd parent_list)
        method parent_list = parent_list

        (** [x#fork fn list acc] forks [x] into as many objects as the number of elements in [list] with different
            ids, and calls [fn] with each pair of forked object and an element from [list] with an accumulator [acc]
            (i.e., folds over the list of forked object). The first forked object will inherit the [path_id] of [x]
            with a new [node_id], whereas the rest will be given fresh [path_id]s and [node_id]s, thereby enforcing
            the execution tree id scheme.

            Note that [fork] is an explicitly-annotated polymorphic method. In Ocaml, methods are monomorphic
            unless explicitly annotated, at both declaration and call sites. Call [fork] with, e.g.,
            [(job : #Info.t)#fork fn list acc].
        *)
        method fork : 'a 'b . ('self -> 'a -> 'b -> 'b) -> 'a list -> 'b -> 'b = fun f xs acc ->
            match xs with
                | x::xs ->
                    let acc = f self x acc in
                    List.fold_left begin fun acc x ->
                        f {<
                            path_id = Counter.next path_counter;
                            node_id = Counter.next node_counter;
                            parent_list = (path_id, node_id)::parent_list;
                        >} x acc
                    end acc xs
                | [] ->
                    (* TODO: would it be more convenient to allow this? *)
                    invalid_arg "empty list"

        method become (other : 'self) =
            path_id <- other#path_id;
            node_id <- other#node_id;
            parent_list <- other#parent_list
    end