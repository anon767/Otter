(** Information about paths and nodes in an execution tree. *)

open DataStructures
open OcamlUtilities

let path_counter = Counter.make ()
let node_counter = Counter.make ()

class ['complete] t :
    object ('self)
        method path_id : int
        method node_id : int
        method path_length : int
        method parent_path_id : int
        method parent_node_id : int
        method parent_list : (int * int) list

        method path_profiler : Profiler.t
        method node_profiler : Profiler.t
        method _with_node_profiler : Profiler.t -> 'self

        method run : ('self -> 'self) -> 'self list * ('complete * 'self) list
        method fork : 'a . 'a list -> 'self * 'a
        method fork_keep_parent : 'a . 'a list -> 'self * 'a
        method finish : 'a . 'complete -> 'a
        method fork_finish : 'complete -> 'self
        method try_run : ('self -> 'self) -> catch_finish:('complete * 'self -> 'self) -> 'self

        method clone : 'self

        method profile_call : 'a . string -> ('self -> 'self * 'a) -> ('self * 'a)

        method become : 'self -> unit
    end
=
    object (self : 'self)
        (* It is less expensive to use two prompts: one for forking, one for finishing. In particular for [try_run],
         * using a combined prompt for  forking/finishing means that the continuation following [try_run] has to be
         * copied so that it can to be applied to the list of results (from possible forking); whereas using separate
         * prompts only requires that the finishing prompt be pushed onto the stack. *)
        val fork_prompt = Delimcc.new_prompt ()
        val finish_prompt = Delimcc.new_prompt ()

        val mutable path_id = Counter.next path_counter
        val mutable node_id = Counter.next node_counter
        val mutable path_length = 0
        val mutable parent_list = []

        method path_id = path_id
        method node_id = node_id
        method path_length = path_length
        method parent_path_id = fst (List.hd parent_list)
        method parent_node_id = snd (List.hd parent_list)
        method parent_list = parent_list

        val mutable path_profiler = new Profiler.t
        method path_profiler = path_profiler

        val mutable node_profiler = new Profiler.t
        method node_profiler = node_profiler
        method _with_node_profiler node_profiler = {< node_profiler = node_profiler >}


        (* Forking is based on the non-deterministic computation pattern (related to Haskell's List monad; similar to green threads):
                let p = new_prompt ()
                let run f = push_prompt p (fun () -> [f ()])
                let fork xs =  take_subcont p (fun sk () -> List.concat (List.map (fun x -> push_delim_subcont sk (fun () -> x)) xs))

            Finishing is based on the exception pattern (related to Haskell's Exception monad):
                let q = new_prompt ()
                let try_run f ~catch = match push_prompt q (fun () -> `Active (f ())) with `Active x -> x | `Complete x -> catch x
                let finish x = abort q (`Complete x)

            See http://ambassadortothecomputers.blogspot.com/2010/08/mixing-monadic-and-direct-style-code.html
            for an excellent explanation of delimited continuations.
        *)


        (** [x#run f] calls [f x] and must be called if [f] calls [x#fork], [x#finish], [x#fork_finish] or [x#try_run]:
            it delimits the context in which forking or finishing occurs.

            If [x#fork] or [x#fork_finish] is called within [f], then the continuation up until [f] returns is executed
            for each fork. Likewise, if [x#finish] or [x#fork_finish] is called, then the finished result is returned
            from [f] (or caughted by [x#try_run]).

            The result is returned in as a pair of list of results that were returned normally or were returned via
            calls to [x#finish] or [x#fork_finish].
        *)
        method run (f : 'self -> 'self) =
            let profiler_context = Profiler.global#save in
            if Delimcc.is_prompt_set fork_prompt then invalid_arg "Info.t#run: already in call";
            let results =
                Delimcc.push_prompt fork_prompt begin fun () ->
                    match Delimcc.push_prompt finish_prompt (fun () -> `Active (f self)) with
                        | `Active x -> ([ x ], [])
                        | `Complete x -> ([], [ x ])
                end
            in
            Profiler.global#restore profiler_context;
            results


        (* helper function for [x#fork] and [x#fork_finish] *)
        method private fork_internal : 'a . keep_parent:bool -> 'a list -> 'self * 'a = fun ~keep_parent xs ->
            match xs with
                | x::xs ->
                    (* TODO: suspend and resume local profilers for each forked object *)
                    Profiler.global#add node_profiler;

                    (* TODO: fix Output's mode handling *)
                    let old_mode = Output.get_mode () in
                    Output.set_mode Output.MSG_PROFILING;
                    Output.printf "== Profile for node %d ==@\n@[%t@]@." node_id node_profiler#printer;
                    Output.set_mode old_mode;

                    let parent_list, path_length =
                        if keep_parent then
                            (parent_list, path_length)
                        else
                            ((path_id, node_id)::parent_list, path_length + 1)
                    in
                    let path_profiler = path_profiler#add node_profiler in
                    let node_profiler = node_profiler#reset in

                    Delimcc.take_subcont fork_prompt begin fun sk () ->
                        let profiler_context = Profiler.global#save in

                        let active, complete = Delimcc.push_delim_subcont sk begin fun () ->
                            Profiler.global#restore profiler_context;
                            let self =
                                if keep_parent then
                                    self
                                else
                                    {<
                                        (* path_id is inherited by the first one, while node_id is updated at every #fork,
                                         * even if it's the only one *)
                                        node_id = Counter.next node_counter;
                                        parent_list = parent_list;
                                        path_length = path_length;
                                        path_profiler = path_profiler;
                                        node_profiler = node_profiler;
                                    >}
                            in
                            (self, x)
                        end in

                        let active, complete = List.fold_left begin fun (active, complete) x ->
                            let active', complete' = Delimcc.push_delim_subcont sk begin fun () ->
                                Profiler.global#restore profiler_context;
                                let self = {<
                                    (* others get new path_ids and node_ids *)
                                    path_id = Counter.next path_counter;
                                    node_id = Counter.next node_counter;
                                    parent_list = parent_list;
                                    path_length = path_length;
                                    path_profiler = path_profiler;
                                    node_profiler = node_profiler;
                                >} in
                                (self, x)
                            end in
                            (List.rev_append active' active, List.rev_append complete' complete)
                        end (List.rev active, List.rev complete) xs in

                        (List.rev active, List.rev complete)
                    end

                | [] ->
                    (* don't allow empty lists at all: it'll be too difficult to track down mistakes due to empty lists *)
                    invalid_arg "Info.t#fork: empty list"


        (** [x#fork list] must be called within [x#run]: it conceptually forks the current Ocaml execution
            (like {!Unix.fork}), into as many as execution the number of elements in [list], and returns in each
            execution [x] with different ids and an element from [list] as a pair. The list of results returned by
            [x#run] will include results from the forked executions.

            The first forked object will inherit the [path_id] of [x] with a new [node_id];while the remaining objects
            will be given fresh [path_id]s and [node_id]s, thereby enforcing the execution tree id scheme.

            In addition, profiling data for the node represented by [x] will be printed if enabled, and the data will
            be added to the path represented by [x] before forking.

            Note that [fork] is an explicitly-annotated polymorphic method. In Ocaml, methods are monomorphic
            unless explicitly annotated, at both declaration and call sites. Call [fork] with, e.g.,
            [(x : _ #Info.t)#fork list]. {b The error "The universal variable 'a would escape its scope" usually
            indicates that a call site lacks such an annotation, and can be fixed by adding the annotation.}
        *)
        method fork : 'a . 'a list -> 'self * 'a = self#fork_internal ~keep_parent:false
        method fork_keep_parent : 'a . 'a list -> 'self * 'a = self#fork_internal ~keep_parent:true


        (** [x#finish complete] must be called within [x#run]: it terminates further execution and returns the result
            [complete] to [x#run].

            In addition, it will print the profiling data for the node and path represented by [x].

            Note that [finish] is an explicitly-annotated polymorphic method. In Ocaml, methods are monomorphic
            unless explicitly annotated, at both declaration and call sites. Call [fork] with, e.g.,
            [(x : _ #Info.t)#finish complete]. {b The error "The universal variable 'a would escape its scope" usually
            indicates that a call site lacks such an annotation, and can be fixed by adding the annotation.}
        *)
        method finish : 'a . 'complete -> 'a = fun complete ->
            Profiler.global#add node_profiler;
            let path_profiler = path_profiler#add node_profiler in

            (* TODO: fix Output's mode handling *)
            let old_mode = Output.get_mode () in
            Output.set_mode Output.MSG_PROFILING;
            Output.printf "== Profile for node %d ==@\n@[%t@]@." node_id node_profiler#printer;
            Output.printf "== Profile for path %d ==@\n@[%t@]@." path_id path_profiler#printer;
            Output.set_mode old_mode;

            Delimcc.abort finish_prompt (`Complete (complete, {< path_profiler = path_profiler >}))


        (** [x#fork_finish complete] combines [fork] and [finish]: it returns the original [x] while simulatenously
            terminating execution on a fork of [x] (but with the same [parent_id]).
        *)
        method fork_finish complete =
            match self#fork_internal ~keep_parent:true [ `Active; `Complete ] with
                | x, `Active -> x
                | x, `Complete -> x#finish complete


        (** [x#try_run f ~catch_finish:handler] calls [f x], and if [f] calls [x'#finish complete] where [x'] is
            derived from [x], it calls [handler (complete, x')]. The methods [x#try_run ... ~catch_finish] and
            [x#finish] can be seen as analogous to [try ... with ...] and [raise].
        *)
        method try_run f ~catch_finish =
            let profiler_context = Profiler.global#save in
            match Delimcc.push_prompt finish_prompt (fun () -> `Active (f self)) with
                | `Active x ->
                    x
                | `Complete x ->
                    Profiler.global#restore profiler_context;
                    catch_finish x


        (** [x#clone] is a convenience function that returns a copy of [x] but with new [path_id] and [node_id], but
            the same [parent_id]. If [x#clone] is called within a call to [x#run], an [Invalid_argument] exception
            will be raised; [x#fork] should be used instead.
         *)
        method clone =
            if Delimcc.is_prompt_set fork_prompt then invalid_arg "Info.t#clone: cannot be called within Info.t#run";
            {< path_id = Counter.next path_counter; node_id = Counter.next node_counter >}


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
                (* TODO: somehow update node_profiler *)
                raise exn
            end in
            (self#_with_node_profiler node_profiler, x)


        method become (other : 'self) =
            path_id <- other#path_id;
            node_id <- other#node_id;
            parent_list <- other#parent_list;
            path_length <- other#path_length;
            path_profiler <- other#path_profiler;
            node_profiler <- other#node_profiler
    end

