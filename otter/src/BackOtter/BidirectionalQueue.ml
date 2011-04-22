open BackOtterUtilities
open DataStructures
open OcamlUtilities
open OtterCore
open State
open Job
open Decision
open Cil

(* TODO: Refactor this *)

let default_bidirectional_search_ratio = ref 0.5
let arg_backotter_overlap_path_matching = ref true


(*
 * Improve efficiency of bounding paths update/checking.
 *
 * Let DP: decision path, BP: bounding path, FP: failing path.
 * Invariants:
 * 1. DP is most recent decision first. FP and BP are least recent decision first.
 * 2. DP and BP do not overlap---(hd BP) is the decision that a job with DP has to follow.
 * 3. job_to_bounding_paths always has jobs that are mapped to non-empty bounding_paths.
 *
 * To check if a FP can bound a DP:
 * For each prefix(DP, k) where k < len(FP) and DP[k] is a call to origin(FP),
 *     If rev_equal(prefix(DP, k), prefix(FP, k)), "YES", and let BP = suffix(FP, k+1)
 *     Else "NO"
 * Lots of memoization
 *
 * Given job = (DP, BP), to check if the child job' = DP' is bounded by BP,
 * If DP == DP', "YES" (there's no new decision)
 * Else assert(tl(DP') == DP), "YES" if hd(DP') == hd(BP)
 *
 *)
module JobMap = struct
    module M = Map.Make (struct type t = int let compare = Pervasives.compare end)
    let empty = M.empty
    let remove job map = M.remove job#node_id map
    let add job property map = M.add job#node_id property map
    let find job map = M.find job#node_id map
    let mem job map = M.mem job#node_id map
end

(* This function is to make sure that a decision path with a function call in
 * latest decision won't create bounded job more than once *)
let function_call_of_latest_decision =
    let visited_set = Hashtbl.create 0 in
    fun decision_path ->
        Profiler.global#call "BidirectionalQueue.function_call_of_latest_decision" begin fun () ->
            if decision_path = DecisionPath.empty then None 
            else match DecisionPath.hd decision_path with
                | DecisionFuncall (_, varinfo) ->
                    let path_id = DecisionPath.id decision_path in
                    if Hashtbl.mem visited_set path_id then None
                    else (Hashtbl.add visited_set path_id (); Some varinfo)
                | _ -> None
        end

(* TODO: package the long list of arguments into BackOtterProfile.t *)
class ['job] t ?(ratio=(!default_bidirectional_search_ratio))
               file
               entry_job
               f_queue
               b_queue
               starter_fundecs =
    object (self)
        val entryfn_jobqueue = f_queue
        val otherfn_jobqueue = b_queue
        (* fundecs whose initialized jobs have been created *)
        val origin_fundecs = starter_fundecs

        (* A worklist for bounded jobs. TODO: maybe Random-path is better? *)
        val bounded_jobqueue = new OtterQueue.RankedQueue.t [ new OtterQueue.DepthFirstStrategy.t ]

        (* the bounded_job previously returned by #get*)
        val previous_bounded_job = None

        (* TODO: make bounding_paths an instance field of BackOtterJob.t 
         *       Then the next two fields can be thrown away. *)
        (* A mapping from jid to bounding paths *)
        val job_to_bounding_paths = JobMap.empty
        
        (* TODO: the line below will be gone when a better way of distributing jobs to queues is implemented *)
        val entry_fn = ProgramPoints.get_entry_fundec file

        method put (job : 'job) =
            Profiler.global#call "BidirectionalQueue.t#put" begin fun () ->
            (* If the job is from a bounded job,
             * if it's still in bound, put it into job_to_bounding_paths,
             * otherwise, discard it.
             *
             * Else, put the job into the jobqueue.
             * If the job's next instr is a function call, and if the function has failing paths,
             * bound the job and put it in job_to_bounding_paths.
             *
             * Idea: in #put, jobs are bounded if they are from bounded jobs or they are calls to
             *       targets.
             *
             * Given job = (DP, BP), to check if the child job' = DP' is bounded by BP,
             * If DP == DP', "YES" (there's no new decision)
             * Else assert(tl(DP') == DP), "YES" if hd(DP') == hd(BP)
             *)
            match previous_bounded_job with
                | Some parent ->
                    Profiler.global#call "BidirectionalQueue.t#put/bounded_parent" begin fun () ->
                        (* If job comes from a bounded job *)
                        Output.debug_printf "Job %d has parent %d@." job#node_id (try snd (List.hd job#parent_list) with Failure "hd" -> -1);
                        Output.debug_printf "Previous bounded job is %d@." parent#node_id;
                        let previous_bounded_job =
                            if job#node_id = parent#node_id then
                                None (* did not fork, make sure #get doesn't clobber this job's bounding paths *)
                            else
                                previous_bounded_job
                        in
                        let bounding_paths = JobMap.find parent job_to_bounding_paths in
                        if DecisionPath.equal parent#decision_path job#decision_path then (* no new decision *)
                            Profiler.global#call "BidirectionalQueue.t#put/bounded_parent/no_new_decision" begin fun () ->
                            let _ = Output.debug_printf "No new decision@." in
                            let _ = Output.debug_printf "Add job_unique %d into the bounded_jobqueue@." job#node_id in
                            {<
                                job_to_bounding_paths = JobMap.add job bounding_paths job_to_bounding_paths;
                                previous_bounded_job = previous_bounded_job;
                                bounded_jobqueue = bounded_jobqueue#put job;
                            >}
                            end
                        else (* has new decision *)
                            Profiler.global#call "BidirectionalQueue.t#put/bounded_parent/new_decision" begin fun () ->
                            let _ = Output.debug_printf "Has new decision@." in
                            let _ = assert(DecisionPath.equal parent#decision_path (DecisionPath.tl job#decision_path)) in
                            let bounded_decision = DecisionPath.hd job#decision_path in
                            let bounding_paths = List.fold_left (
                                fun acc path -> 
                                    if not (DecisionPath.equal path DecisionPath.empty) && Decision.equal bounded_decision (DecisionPath.hd path) then (DecisionPath.tl path) :: acc 
                                    else acc
                            ) [] bounding_paths in
                            if bounding_paths = [] then (* out bound *)
                                let _ = Output.debug_printf "Out bound@." in
                                self (* Discard the job *)
                            else
                                let _ = Output.debug_printf "Add job_unique %d into the bounded_jobqueue@." job#node_id in
                                {<
                                    job_to_bounding_paths = JobMap.add job bounding_paths job_to_bounding_paths;
                                    previous_bounded_job = previous_bounded_job;
                                    bounded_jobqueue = bounded_jobqueue#put job;
                                >}
                            end
                    end

                | None ->
                    Profiler.global#call "BidirectionalQueue.t#put/regular_parent" begin fun () ->
                        (* If job does not from a bounded job, i.e., regular *)
                        (* Check if the job is a function call to a target function *)
                        let job, job_to_bounding_paths, bounded_jobqueue =
                            let fundec_opt =
                                match 
                                    Profiler.global#call "BidirectionalQueue.t#put/regular_parent/function_call_of_latest_decision"
                                        (fun () -> function_call_of_latest_decision job#decision_path)
                                with
                                | Some (varinfo) -> (try Some(CilUtilities.FindCil.fundec_by_varinfo file varinfo) with Not_found -> None)
                                | None -> None
                            in
                            match fundec_opt with
                            | Some (fundec) ->
                                let failing_paths = BackOtterTargets.get_paths fundec in
                                let failing_paths_length = List.length failing_paths in
                                if failing_paths_length = 0 then
                                    (job, job_to_bounding_paths, bounded_jobqueue) (* Not a target function *)
                                else begin
                                    Output.set_mode Output.MSG_REPORT;
                                    Output.printf "Call target function %s@." fundec.svar.vname;

                                    (* just note that the original job was forked into a bounded job *)
                                    let bounded_job = job#clone in
                                    Output.debug_printf "Add job %d into the bounded_jobqueue@." bounded_job#node_id;
                                    let job_to_bounding_paths = JobMap.add bounded_job failing_paths job_to_bounding_paths in
                                    let bounded_jobqueue = bounded_jobqueue#put bounded_job in

                                    (job, job_to_bounding_paths, bounded_jobqueue)
                                end
                            | None ->
                                (job, job_to_bounding_paths, bounded_jobqueue)
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
                           job_to_bounding_paths = job_to_bounding_paths;
                           bounded_jobqueue = bounded_jobqueue; >}
                    end
            end

        method get =
            Profiler.global#call "BidirectionalQueue.t#get" begin fun () ->
                (* Clear the label, as anything printed here has no specific job context *)
                Output.set_formatter (new Output.plain);

                (* First check if there're existing jobs in job_to_bounding_paths.
                 * If so, simply return one of them.
                 *
                 * Else, assert(job_to_bounding_paths is empty)
                 * If there exists a new failing path, update all job's bounding paths.
                 * And add jobs into job_to_bounding_paths if they have any.
                 *
                 * If job_to_bounding_paths is not empty, take a job from it and return.
                 * Else, return jobqueue#get.
                 *
                 * Idea: in #get, jobs are bounded if the new failing path has effect on them.
                 *)

                let job_to_bounding_paths = match previous_bounded_job with
                    | Some job -> JobMap.remove job job_to_bounding_paths
                    | None -> job_to_bounding_paths
                in
                match bounded_jobqueue#get with
                | Some (bounded_jobqueue, job) ->
                    (* Has bounded job to run *)
                    let _ = Output.debug_printf "Take job_unique %d from bounded_jobqueue@." job#node_id in
                    Some ({< previous_bounded_job = Some job; bounded_jobqueue = bounded_jobqueue; >}, job)
                | None ->
                    (* For each existing job, see if it can be bounded. If so, update bounded_jobqueue et al *)
                    let entryfn_jobqueue, otherfn_jobqueue, job_to_bounding_paths, bounded_jobqueue =
                        if (!arg_backotter_overlap_path_matching) then 
                            let new_failing_path = BackOtterTargets.get_last_failing_path () in
                            match new_failing_path with
                            | Some (target_fundec, failing_path) ->
                                Profiler.global#call "BidirectionalQueue.t#get/update_bounding_status" begin fun () ->
                                    (*
                                     * To check if a FP can bound a DP:
                                     * For each prefix(DP, k) where k < len(FP) and DP[k] is a call to origin(FP),
                                     *     If rev_equal(prefix(DP, k), prefix(FP, k)), "YES", and let BP = suffix(FP, k+1)
                                     *     Else "NO"
                                     *)
                                    let failing_path_length = DecisionPath.length failing_path in
                                    let find_bounded_jobs job (jobqueue, job_to_bounding_paths, bound_jobqueue) =
                                        let rec scan decision_path depth =
                                            if depth <= 0 || DecisionPath.length decision_path = 0 then []
                                            else match DecisionPath.hd decision_path with
                                                | DecisionFuncall (_, varinfo)  when varinfo.vid = target_fundec.svar.vid ->
                                                     let bounding_paths = scan (DecisionPath.tl decision_path) (depth - 1) in
                                                     let matches, _, failing_tail =
                                                         Profiler.global#call "BidirectionalQueue.t#get/update_bounding_status/scan/rev_equals" begin fun () ->
                                                             rev_equals Decision.equal decision_path failing_path depth
                                                         end
                                                     in
                                                     if matches then failing_tail :: bounding_paths else bounding_paths
                                                | _ -> scan (DecisionPath.tl decision_path) (depth - 1)
                                        in
                                        let bounding_paths =
                                            Profiler.global#call "BidirectionalQueue.t#get/update_bounding_status/scan" begin fun () ->
                                            (* TODO: job#decision_path is too long. Maybe maintain a decision path for function calls only. *)
                                            scan job#decision_path (min failing_path_length (DecisionPath.length job#decision_path))
                                            end
                                        in
                                        if bounding_paths = [] then
                                            (jobqueue, job_to_bounding_paths, bounded_jobqueue)
                                        else begin

                                            (* just note that the original job was forked into a bounded job *)
                                            let bounded_job = job#clone in
                                            Output.debug_printf "Add job %d into the bounded_jobqueue@." bounded_job#node_id;
                                            let job_to_bounding_paths = JobMap.add bounded_job bounding_paths job_to_bounding_paths in
                                            let bounded_jobqueue = bounded_jobqueue#put bounded_job in

                                            (jobqueue, job_to_bounding_paths, bounded_jobqueue)
                                        end
                                    in
                                    let entryfn_jobqueue, job_to_bounding_paths, bounded_jobqueue =
                                        entryfn_jobqueue#fold find_bounded_jobs (entryfn_jobqueue, job_to_bounding_paths, bounded_jobqueue)
                                    in
                                    let otherfn_jobqueue, job_to_bounding_paths, bounded_jobqueue =
                                        otherfn_jobqueue#fold find_bounded_jobs (otherfn_jobqueue, job_to_bounding_paths, bounded_jobqueue)
                                    in
                                    (entryfn_jobqueue, otherfn_jobqueue, job_to_bounding_paths, bounded_jobqueue)
                                end
                            | None ->
                                (entryfn_jobqueue, otherfn_jobqueue, job_to_bounding_paths, bounded_jobqueue)
                        else
                            entryfn_jobqueue, otherfn_jobqueue, job_to_bounding_paths, bounded_jobqueue
                    in
                    match bounded_jobqueue#get with
                    | Some (bounded_jobqueue, job) ->
                        (* Has bounded job to run *)
                        let _ = Output.debug_printf "Take job from bounded_jobqueue@." in
                        Some ({<
                            entryfn_jobqueue = entryfn_jobqueue;
                            otherfn_jobqueue = otherfn_jobqueue;
                            bounded_jobqueue = bounded_jobqueue;
                            previous_bounded_job = Some job;
                            job_to_bounding_paths = job_to_bounding_paths;
                        >}, job)
                    | None -> (* Regular get *)
                        (* assert: job_to_bounding_paths are unchanged at this point, and bounded_jobqueue is always empty *)
                        Profiler.global#call "BidirectionalQueue.t#get/regular_get" begin fun () ->
                        (* If there's no more entry jobs, the forward search has ended. So we terminate. *)
                        if entryfn_jobqueue#length = 0 then None else

                        (* Set up targets, which maps target functions to their failing paths, and
                         * target_fundecs, which is basically the key set of targets *)
                        let target_fundecs = BackOtterTargets.get_target_fundecs () in

                        (* Create new jobs for callers of new targets *)
                        let origin_fundecs', otherfn_jobqueue =
                            Profiler.global#call "BidirectionalQueue.t#get/regular_get/create_new_jobs" begin fun () ->
                            List.fold_left (
                                fun (origin_fundecs, otherfn_jobqueue) target_fundec ->
                                    let callers = CilUtilities.CilCallgraph.find_callers file target_fundec in
                                    List.fold_left (
                                        fun (origin_fundecs, otherfn_jobqueue) caller ->
                                            Output.debug_printf "Function %s is the caller of target function %s@\n" caller.svar.vname target_fundec.svar.vname;
                                            let caller = BackOtterUtilities.get_transitive_unique_caller file caller in
                                            (* caller is already initialized *)
                                            if List.memq caller origin_fundecs then
                                                origin_fundecs, otherfn_jobqueue
                                            (* caller is entry_fn: mark it as initialized, but don't actually add it to otherfn_jobqueue *)
                                            else if caller == entry_fn then
                                                caller :: origin_fundecs, otherfn_jobqueue
                                            (* caller is not entry_fn *)
                                            else
                                                let job = (
                                                    Output.debug_printf "Create new job for function %s@." caller.svar.vname;
                                                    Profiler.global#call "BidirectionalQueue.t#get/regular_get/create_new_jobs/new_functionjob" begin fun () ->
                                                        (* TODO: let's try a simpler Job initializer *)
                                                        BackOtterJob.get_function_job file caller
                                                    end
                                                ) in
                                                caller :: origin_fundecs, otherfn_jobqueue#put job
                                    ) (origin_fundecs, otherfn_jobqueue) callers
                            ) (origin_fundecs, otherfn_jobqueue) target_fundecs
                            end
                        in

                        (* debug, Debug, DEBUG (warning: these can slow down regular_get) *)
                        Output.debug_printf "Number of entry function jobs: %d@." (entryfn_jobqueue#length);
                        Output.debug_printf "Number of other function jobs: %d@." (otherfn_jobqueue#length);
                        List.iter (fun f -> Output.debug_printf "Target function: %s@." f.svar.vname) (BackOtterTargets.get_target_fundecs ());
                        List.iter (fun f -> Output.debug_printf "Origin function: %s@." f.svar.vname) (origin_fundecs');

                        (* Determine whether to run entry function jobs or other function jobs *)
                        let want_process_entryfn =
                            (* if ratio <= 0.0 (i.e., pure Backward) AND entry_fn is a caller to some
                             * target, ALWAYS want to process entry_fn. *)
                            if ratio <= 0.0 && List.memq entry_fn origin_fundecs' then true else
                            let entryfn_time_elapsed, otherfn_time_elapsed = !BackOtterTimer.timer_ref in
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
                                    origin_fundecs = origin_fundecs';
                                    previous_bounded_job = None;
                                    job_to_bounding_paths = job_to_bounding_paths;
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
                                    origin_fundecs = origin_fundecs';
                                    previous_bounded_job = None;
                                    job_to_bounding_paths = job_to_bounding_paths;
                                >}, job)
                            | None -> failwith "This is unreachable"
                            end
                        else
                            None
                        end
            end
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

