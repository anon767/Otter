(** Information about paths and nodes in an execution tree. *)

open DataStructures
open OcamlUtilities

class t :
    object ('self)
        method path_id : int
        method node_id : int
        method parent_path_id : int
        method parent_node_id : int
        method parent_list : (int * int) list

        method path_profiler : Profiler.t
        method node_profiler : Profiler.t
        method _with_node_profiler : Profiler.t -> 'self

        method fork : 'a 'b . ('self -> 'a -> 'b -> 'b) -> 'a list -> 'b -> 'b
        method fork2: 'a 'b . ('self -> 'a) -> ('self -> 'b) -> 'a * 'b
        method finish : 'self
        method profile_call : 'a . string -> ('self -> 'self * 'a) -> ('self * 'a)

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

        val mutable path_profiler = new Profiler.t
        method path_profiler = path_profiler

        val mutable node_profiler = new Profiler.t
        method node_profiler = node_profiler
        method _with_node_profiler node_profiler = {< node_profiler = node_profiler >}

        (** [x#fork fn list acc] forks [x] into as many objects as the number of elements in [list] with different
            ids, and calls [fn] with each pair of forked object and an element from [list] with an accumulator [acc]
            (i.e., folds over the list of forked object). The first forked object will inherit the [path_id] of [x]
            with a new [node_id], whereas the rest will be given fresh [path_id]s and [node_id]s, thereby enforcing
            the execution tree id scheme. The updated accumulator will be returned.

            In addition, profiling data for the node represented by [x] will be printed if enabled, and the data will
            be added to the path represented by [x] before forking.

            Note that [fork] is an explicitly-annotated polymorphic method. In Ocaml, methods are monomorphic
            unless explicitly annotated, at both declaration and call sites. Call [fork] with, e.g.,
            [(x : #Info.t)#fork fn list acc].
        *)
        method fork : 'a 'b . ('self -> 'a -> 'b -> 'b) -> 'a list -> 'b -> 'b = fun f xs acc ->
            match xs with
                | x::xs ->
                    (* TODO: what to do if fork occurs inside a profiled method? *)
                    Profiler.global#add node_profiler;

                    (* TODO: fix Output's mode handling *)
                    let old_mode = Output.get_mode () in
                    Output.set_mode Output.MSG_PROFILING;
                    Output.printf "== Profile for node %d ==@\n@[%t@]@." node_id node_profiler#printer;
                    Output.set_mode old_mode;

                    let parent_list = (path_id, node_id)::parent_list in
                    let path_profiler = path_profiler#add node_profiler in
                    let node_profiler = node_profiler#reset in

                    let acc = f {<
                        (* path_id is inherited by the first one, while node_id is updated at every #fork,
                         * even if it's the only one *)
                        node_id = Counter.next node_counter;
                        parent_list = parent_list;
                        path_profiler = path_profiler;
                        node_profiler = node_profiler;
                    >} x acc in

                    List.fold_left begin fun acc x ->
                        f {<
                            (* others get new path_ids and node_ids *)
                            path_id = Counter.next path_counter;
                            node_id = Counter.next node_counter;
                            parent_list = parent_list;
                            path_profiler = path_profiler;
                            node_profiler = node_profiler;
                        >} x acc
                    end acc xs
                | [] ->
                    (* TODO: Should we throw an exception? *)
                    acc


        (** [x#fork2 left right] is a convenience function that calls [fork] and applies two functions to the forked
            objects, returning the results as a tuple [(left_result, right_result)].
        *)
        method fork2 : 'a 'b . ('self -> 'a) -> ('self -> 'b) -> 'a * 'b = fun left right ->
            (** reuse #fork to centralize the forking policy *)
            match self#fork (fun x dir xs -> match dir with `L -> (`L (left x))::xs | `R -> (`R (right x))::xs) [`L; `R] [] with
                | [ `R right; `L left ] -> (left, right)
                | _ -> assert false


        (** [x#finish] should be called when the path represented by [x] is considered finished, and will print the
            profiling data for the node and path represented by [x].
        *)
        method finish =
            (* TODO: figure out how to enforce finish; probably by pulling job_completion into this module *)
            Profiler.global#add node_profiler;
            let path_profiler = path_profiler#add node_profiler in

            (* TODO: fix Output's mode handling *)
            let old_mode = Output.get_mode () in
            Output.set_mode Output.MSG_PROFILING;
            Output.printf "== Profile for node %d ==@\n@[%t@]@." node_id node_profiler#printer;
            Output.printf "== Profile for path %d ==@\n@[%t@]@." path_id path_profiler#printer;
            Output.set_mode old_mode;

            {< path_profiler = path_profiler >}


        (** [x#profile_call label fn] profiles the call to [fn] and records it under [label]. The function [fn] is
            given a [x] updated with a new profiling context, and must return a tuple [(x', data)] where [x'] is [x]
            itself or an updated version of [x].

            Note that [profile_call] is an explicitly-annotated polymorphic method. In Ocaml, methods are monomorphic
            unless explicitly annotated, at both declaration and call sites. Call [profile_call] with, e.g.,
            [(x : #Info.t)#profile_call label fn].
        *)
        method profile_call : 'a . string -> ('self -> 'self * 'a) -> ('self * 'a) = fun label f ->
            let node_profiler, (self, x) = node_profiler#call label begin fun node_profiler ->
                let self, x = f {< node_profiler = node_profiler >} in
                (self#node_profiler, (self, x))
            end ~catch:begin fun exn node_profiler ->
            (* TODO: turn self into an abandoned job and store node_profiler in it *)
                raise exn
            end in
            (self#_with_node_profiler node_profiler, x)

        method become (other : 'self) =
            path_id <- other#path_id;
            node_id <- other#node_id;
            parent_list <- other#parent_list;
            path_profiler <- other#path_profiler;
            node_profiler <- other#node_profiler
    end

