open BackOtterUtilities
open DataStructures
open OcamlUtilities
open OtterCore
open State
open Job
open Decision
open Cil

let default_bidirectional_search_ratio = ref 0.5
let arg_backotter_overlap_path_matching = ref false

module JobMap = struct
    module M = Map.Make (struct type t = int let compare = Pervasives.compare end)
    let empty = M.empty
    let remove job map = M.remove job#node_id map
    let add job property map = M.add job#node_id property map
    let find job map = M.find job#node_id map
    let mem job map = M.mem job#node_id map
end

class ['job] t ?(ratio=(!default_bidirectional_search_ratio))
               ?(f_queue=BackOtterQueue.get_default_fqueue ())
               ?(b_queue=BackOtterQueue.get_default_bqueue ())
               file =
    object (self : 'self)
        val entryfn_jobqueue = new ContentQueue.t f_queue
        val otherfn_jobqueue = new ContentQueue.t b_queue
        val timer = new BackOtterTimer.t
        (* fundecs whose initialized jobs have been created *)

        (* A worklist for bounded jobs. TODO: maybe Random-path is better? *)
        val bounded_jobqueue = new OtterQueue.RankedQueue.t [ new OtterQueue.DepthFirstStrategy.t ]

        (* TODO: the line below will be gone when a better way of distributing jobs to queues is implemented *)
        val entry_fn = ProgramPoints.get_entry_fundec file

        method put (job : 'job) = Profiler.global#call "BidirectionalQueue.t#put" begin fun () ->
            (* If job is bounded and still in bound, put it in the bounded_jobqueue.
             * Else if the job is bounded but out bound, discard it.
             * Else,
             *   1. Put the job into one of {entryfn_jobqueue, otherfn_jobqueue};
             *   2. If the job's next instr is a function call, and if the function has failing paths,
             *      bound the job and put it in bounded_jobqueue.
             *)
            match job#bounding_paths with
            | Some [] -> (* Discard *)
                 Output.set_mode Output.MSG_REPORT;
                 Output.printf "Discard out-of-bound job %d@\n" job#node_id;
                 self 
            | Some bounding_paths -> {< bounded_jobqueue = bounded_jobqueue#put job >}
            | None -> Profiler.global#call "BidirectionalQueue.t#put/regular_parent" begin fun () ->
                (* Regular. Check if the job is a function call to a target function *)
                let job, bounded_jobqueue =
                    let fundec_opt = match job#latest_function_call with
                        | Some varinfo -> begin try Some (CilUtilities.FindCil.fundec_by_varinfo file varinfo) with Not_found -> None end
                        | None -> None
                    in
                    let job = job#clear_latest_function_call in
                    let bounded_jobqueue = match fundec_opt with
                        | Some (fundec) ->
                            let failing_paths = BackOtterTargets.get_paths fundec in
                            let failing_paths_length = List.length failing_paths in
                            if failing_paths_length = 0 then 
                                bounded_jobqueue (* Not a target function *)
                            else begin
                                Output.set_mode Output.MSG_REPORT;
                                Output.printf "Call target function %s@." fundec.svar.vname;

                                (* just note that the original job was forked into a bounded job *)
                                let bounded_job = job#clone in
                                let bounded_job = bounded_job#with_bounding_paths (Some failing_paths) in
                                Output.debug_printf "Add job %d into the bounded_jobqueue@." bounded_job#node_id;
                                let bounded_jobqueue = bounded_jobqueue#put bounded_job in
                                bounded_jobqueue
                            end
                        | None -> bounded_jobqueue
                    in (job, bounded_jobqueue)
                in
                let entryfn_jobqueue, otherfn_jobqueue =
                    Profiler.global#call "BidirectionalQueue.t#put/regular_parent/put" begin fun () ->
                    if get_origin_function job == entry_fn then
                        entryfn_jobqueue#put job, otherfn_jobqueue
                    else
                        entryfn_jobqueue, otherfn_jobqueue#put job
                    end
                in
                {< entryfn_jobqueue = entryfn_jobqueue;
                   otherfn_jobqueue = otherfn_jobqueue;
                   bounded_jobqueue = bounded_jobqueue; >}
            end
        end

        method get =
            Profiler.global#call "BidirectionalQueue.t#get" begin fun () ->
                (* Clear the label, as anything printed here has no specific job context *)
                Output.set_formatter (new Output.plain);

                (* 
                 * If the bounded_jobqueue is nonempty, return bounded_jobqueue#get.
                 * Else if there exists a new failing path, update all job's bounding paths
                 * (this procedure, call it "overlap", is a TODO.)
                 * Else, return jobqueue#get for jobqueue in {entryfn_jobqueue, otherfn_jobqueue}.
                 *
                 * Idea: in #get, jobs are bounded if the new failing path has effect on them.
                 *)
                match bounded_jobqueue#get with
                | Some (bounded_jobqueue, job) ->
                    (* Has bounded job to run *)
                    let _ = Output.debug_printf "Take job_unique %d from bounded_jobqueue@." job#node_id in
                    Some ({< bounded_jobqueue = bounded_jobqueue >}, job)
                | None -> Profiler.global#call "BidirectionalQueue.t#get/regular_get" begin fun () ->
                    (* TODO: implement "overlap" here *)

                    (* If there's no more entry jobs, the forward search has ended. So we terminate. *)
                    if entryfn_jobqueue#length = 0 then None else 

                    (* Determine whether to run entry function jobs or other function jobs *)
                    let want_process_entryfn =
                        (* if ratio <= 0.0 (i.e., pure Backward) AND entry_fn is a caller to some
                         * target, ALWAYS want to process entry_fn. *)
                        if ratio <= 0.0 && FunctionManager.is_ready_to_run file entry_fn then true else
                        let entryfn_time_elapsed, otherfn_time_elapsed = timer#time_elapsed in
                        let total_elapsed = entryfn_time_elapsed +. otherfn_time_elapsed in
                        if total_elapsed <= 0.0001 (* epsilon *) then ratio > 0.0
                        else entryfn_time_elapsed /. total_elapsed <= ratio
                    in
                    if (not (entryfn_jobqueue#length = 0)) && (otherfn_jobqueue#length = 0 || want_process_entryfn)  then
                        (* Do forward search *)
                        Profiler.global#call "BidirectionalQueue.t#get/regular_get/forward_search" begin fun () ->
                        match entryfn_jobqueue#get with
                        | Some (entryfn_jobqueue, job) ->
                            Some ({<
                                entryfn_jobqueue = entryfn_jobqueue;
                                otherfn_jobqueue = otherfn_jobqueue;  (* might have been added with new jobs from above *)
                                timer = timer#time_entryfn;
                            >}, job)
                        | None -> failwith "This is unreachable"
                        end
                    else if otherfn_jobqueue#length > 0 then
                        (* Do "backward" search *)
                        Profiler.global#call "BidirectionalQueue.t#get/regular_get/backward_search" begin fun () ->
                        match otherfn_jobqueue#get with
                        | Some (otherfn_jobqueue, job) ->
                            Some ({<
                                entryfn_jobqueue = entryfn_jobqueue;
                                otherfn_jobqueue = otherfn_jobqueue;
                                timer = timer#time_otherfn;
                            >}, job)
                        | None -> failwith "This is unreachable"
                        end
                    else
                        None
                end
            end 

    method remove (job : 'job) : 'self = failwith "BidirectionalQueue does not support method remove"

    end

(** {1 Command-line options} *)

let options = [
    "--bidirectional-search-ratio",
        Arg.Set_float default_bidirectional_search_ratio,
        "<ratio> The fraction of computation dedicated to forward search (default: 0.5)";
    "--backotter-no-overlap-path-matching",
        Arg.Clear arg_backotter_overlap_path_matching,
        " Disable paths match-up by overlapping";
]

