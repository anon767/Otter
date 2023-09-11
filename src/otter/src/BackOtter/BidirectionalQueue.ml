(** A queue that implements mixed Call-chain-backward Symbolic Execution (CCBSE).

    Note that:
    - Jobs coming from the entry function will always go into the forward search, even for pure CCBSE.
    - [remove] is not well-defined for this queue, therefore it's not supported.
 *)
open OcamlUtilities
open Cil
open OtterCore
open BackOtterUtilities

(**/**)
let arg_ratio = ref 0.5
let real_timer = ref (new BackOtterTimer.t ~timing_method:`TimeReal ())
(**/**)

class ['job] t ?(ratio=(!arg_ratio))
               file
               ?(forward_queue=BackOtterQueue.get_default_fqueue file)
               ?(backward_queue=BackOtterQueue.get_default_bqueue file)
               ()
    = object (self : 'self)
        val forward_queue = new ContentQueue.t forward_queue
        val backward_queue = new ContentQueue.t backward_queue
        val bounded_queue = new OtterQueue.RankedQueue.t [ new OtterQueue.DepthFirstStrategy.t ]
        val timer = new BackOtterTimer.t ()

        (** Put a job into this queue. The job will go to [forward_queue], [backward_queue] and/or [bounded_queue], or be discarded, based on the following:
            + If the job is bounded and still in-bound, put it in the bounded_queue.
            + Else if the job is bounded but out-bound, discard it.
            + Else, put the job into one of [forward_queue] and [backward_queue].
              If the job's next instruction is a function call, and if the function has failing paths, put it in bounded_queue.
         *)
        method put (job : 'job) =
            Profiler.global#call "BidirectionalQueue.t#put" begin fun () ->
                match job#bounding_paths with
                | Some [] ->
                     Output.debug_printf "Discard out-of-bound job %d@." job#node_id;
                     self

                | Some bounding_paths ->
                    {< bounded_queue = bounded_queue#put job >}

                | None ->
                    (* Regular job. Check if the job is a function call to a target function *)
                    Profiler.global#call "BidirectionalQueue.t#put/regular" begin fun () ->
                        let job, bounded_queue =
                            (* Retreive the last function call along the path, and reset it. *)
                            let fundec_opt = match job#last_function_call with
                                | Some varinfo -> begin try Some (CilUtilities.FindCil.fundec_by_varinfo file varinfo) with Not_found -> None end
                                | None -> None
                            in
                            let job = job#clear_last_function_call in
                            let bounded_queue = match fundec_opt with
                                | Some (fundec) ->
                                    let failing_paths = BackOtterTargets.get_paths fundec in
                                    let failing_paths_length = List.length failing_paths in
                                    if failing_paths_length = 0 then
                                        (* Not a target function *)
                                        bounded_queue
                                    else begin
                                        Output.debug_printf "Call target function %s@." fundec.svar.vname;

                                        (* Fork a bounded job out of the regular job *)
                                        let bounded_job = job#clone in
                                        let bounded_job = bounded_job#with_bounding_paths (Some failing_paths) in
                                        Output.debug_printf "Add job %d into the bounded_queue@." bounded_job#node_id;
                                        let bounded_queue = bounded_queue#put bounded_job in
                                        bounded_queue
                                    end
                                | None -> bounded_queue
                            in (job, bounded_queue)
                        in
                        let forward_queue, backward_queue =
                            let entry_fn = ProgramPoints.get_entry_fundec file in
                            if get_origin_function job == entry_fn then
                                forward_queue#put job, backward_queue
                            else
                                forward_queue, backward_queue#put job
                        in
                        {< forward_queue = forward_queue;
                           backward_queue = backward_queue;
                           bounded_queue = bounded_queue; >}
                    end
            end

        (** Get a job out of this queue.
            + If the bounded_queue is nonempty, return bounded_queue#get.
            + Else if there exists a new failing path, update all job's bounding paths
            + Else, return queue#get for queue in either [forward_queue] or [backward_queue].
         *)
        method get =
            Profiler.global#call "BidirectionalQueue.t#get" begin fun () ->
                (* Clear the label, as anything printed here has no specific job context *)
                Output.set_formatter (new Output.plain);

                match bounded_queue#get with
                | Some (bounded_queue, job) ->
                    Output.debug_printf "Take job_unique %d from bounded_queue@." job#node_id;
                    Some ({< bounded_queue = bounded_queue >}, job)

                | None ->
                    Profiler.global#call "BidirectionalQueue.t#get/regular" begin fun () ->
                        (* TODO: check paths overlapping here *)

                        (* If there're no more entry jobs, the forward search will end. So we terminate. *)
                        if forward_queue#length = 0 then
                            None
                        else
                            (* Determine whether to run the forward search of the backward search *)
                            let want_forward_search =
                                if backward_queue#length = 0 then
                                    true
                                else
                                    let entry_fn = ProgramPoints.get_entry_fundec file in
                                    if ratio <= 0.0 && FunctionManager.is_ready_to_run file entry_fn then
                                        true
                                    else
                                        let forward_time, backward_time = timer#time_elapsed in
                                        let total_time = forward_time +. backward_time in
                                        if total_time <= 0.0001 (* epsilon *) then
                                            ratio > 0.0
                                        else
                                            forward_time /. total_time <= ratio
                            in
                            if want_forward_search then
                                match forward_queue#get with
                                | Some (forward_queue, job) ->
                                    real_timer := (!real_timer)#time_forward;
                                    Some ({< forward_queue = forward_queue; timer = timer#time_forward; >}, job)
                                | None -> failwith "This is unreachable"

                            else if backward_queue#length > 0 then
                                match backward_queue#get with
                                | Some (backward_queue, job) ->
                                    real_timer := (!real_timer)#time_backward;
                                    Some ({< backward_queue = backward_queue; timer = timer#time_backward; >}, job)
                                | None -> failwith "This is unreachable"

                            else
                                None
                    end
            end

        (** [remove] is not supported.
            @raise Failure when being called.
         *)
        method remove (job : 'job) : 'self =
            failwith "BidirectionalQueue does not support method remove"

    end

(** {1 Command-line options} *)

let options = [
    "--bidirectional-search-ratio",
        Arg.Set_float arg_ratio,
        Printf.sprintf "<ratio> The fraction of computation dedicated to forward search (default: %.2f)" !arg_ratio;
]
