open BackOtterUtilities
open OcamlUtilities
open OtterCore
open Types
open Job
open Decision
open Cil

let default_bidirectional_search_ratio = ref 0.5

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
module JobMap = Map.Make (JobOrderedType)

(* TODO: package the long list of arguments into BackOtterProfile.t *)
class ['job] t ?(ratio=(!default_bidirectional_search_ratio))
               file
               targets_ref
               timer_ref
               entry_fn
               failure_fn
               entry_job
               f_queue
               b_queue =
    (* ratio < 0.0 denotes purely backward (beware of precision!) *)
    let is_bidirectional = ratio >= 0.0 in
    object (self)
        val entryfn_jobqueue = let queue = new ContentQueue.t f_queue in if is_bidirectional then queue#put entry_job else queue
        val otherfn_jobqueue = new ContentQueue.t b_queue
        (* fundecs whose initialized jobs have been created *)
        val origin_fundecs = []
        (* A mapping from job to boundingPaths *)
        val job_to_bounding_paths = JobMap.empty
        (* The most recent job returned by #get
         * It is supposed to be the parent of all jobs being put into this queue *)
        val last_bounded_job_from_get = None

        method put (job : 'job) =
            let put () =
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
            (* This function is to make sure that a decision path with a function call in
             * latest decision won't create bounded job more than once *)
            match last_bounded_job_from_get with
            | Some (parent_job) ->
                (try
                    (* Parent job is bounded *)
                    let bounding_paths = JobMap.find parent_job job_to_bounding_paths in
                    if parent_job.decisionPath == job.decisionPath then (* no new decision *)
                        {< job_to_bounding_paths = JobMap.add job bounding_paths job_to_bounding_paths; >}
                    else (* has new decision *)
                        let _ = assert(parent_job.decisionPath == List.tl job.decisionPath) in
                        let bounded_decision = List.hd job.decisionPath in
                        let bounding_paths = List.fold_left (
                            fun acc path -> if Decision.equals bounded_decision (List.hd path) then (List.tl path) :: acc else acc
                        ) [] bounding_paths in
                        if bounding_paths = [] then (* out bound *)
                            self
                        else
                            {< job_to_bounding_paths = JobMap.add job bounding_paths job_to_bounding_paths; >}
                with Not_found -> (* Parent job is not bounded *) failwith "Parent job must be bounded"
                )
            | None -> (* No parent job. Regular put *)
                let function_call_of_latest_decision =
                    let memotable = Hashtbl.create 0 in
                    function
                    | DecisionFuncall (_, fundec) :: _ as decisions->
                        if Hashtbl.mem memotable decisions then None
                        else (Hashtbl.add memotable decisions (); Some fundec)
                    | _ -> None
                in
                let entryfn_jobqueue, otherfn_jobqueue =
                    if is_bidirectional && get_origin_function job == entry_fn then
                        entryfn_jobqueue#put job, otherfn_jobqueue
                    else
                        entryfn_jobqueue, otherfn_jobqueue#put job
                in
                let job_to_bounding_paths =
                    match function_call_of_latest_decision job.decisionPath with
                    | Some (fundec) ->
                        let failing_paths = BackOtterTargets.get fundec (!targets_ref) in
                        if failing_paths = [] then job_to_bounding_paths
                        else JobMap.add job failing_paths job_to_bounding_paths
                    | None -> job_to_bounding_paths
                in
                {< entryfn_jobqueue = entryfn_jobqueue;
                   otherfn_jobqueue = otherfn_jobqueue;
                   job_to_bounding_paths = job_to_bounding_paths; >}
        in
        BackOtterUtilities.time "BidirectionalQueue.t#put" put ()

        method get =
            let get () =
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
                let job_to_bounding_paths =
                    match last_bounded_job_from_get with
                    | Some job -> JobMap.remove job job_to_bounding_paths
                    | None -> job_to_bounding_paths
                in
                let get_job_from_job_to_bounding_paths job_to_bounding_paths =
                    (* TODO: choose which job to run first? E.g., closest to entry fn *)
                    (* Ocaml 3.12.0 has Map.choose *)
                    let choose job_to_bounding_paths =
                        match JobMap.fold (fun job bounding_paths select -> if select = None then Some (job, bounding_paths) else select) job_to_bounding_paths None with
                        | None -> invalid_arg "job_to_bounding_paths is empty"
                        | Some (job, bounding_paths) -> job, bounding_paths
                    in
                    let job, _ = choose job_to_bounding_paths in
                    Some ({< job_to_bounding_paths = job_to_bounding_paths;
                             last_bounded_job_from_get = Some job; >}, job)
                in
                if not (JobMap.is_empty job_to_bounding_paths) then
                    get_job_from_job_to_bounding_paths job_to_bounding_paths
                else
                    let targets, new_failing_path = BackOtterTargets.get_last_failing_path (!targets_ref) in
                    targets_ref := targets;
                    let job_to_bounding_paths = match new_failing_path with
                        | Some (target_fundec, failing_path) ->
                            let update_job_to_bounding_paths () =
                                (*
                                 * To check if a FP can bound a DP:
                                 * For each prefix(DP, k) where k < len(FP) and DP[k] is a call to origin(FP),
                                 *     If rev_equal(prefix(DP, k), prefix(FP, k)), "YES", and let BP = suffix(FP, k+1)
                                 *     Else "NO"
                                 *)
                                let failing_path_length = length failing_path in
                                let jobs =
                                    (* Usually otherfn_jobqueue is much shorter, unless it's pure-backward *)
                                    if entryfn_jobqueue#length = 0 then otherfn_jobqueue#get_contents
                                    else List.rev_append otherfn_jobqueue#get_contents entryfn_jobqueue#get_contents
                                in
                                List.fold_left (fun job_to_bounding_paths job ->
                                    let rec scan decision_path depth =
                                        if depth <= 0 then [] else
                                        match decision_path with
                                        | DecisionFuncall (_, fundec) :: tail when fundec == target_fundec ->
                                             let bounding_paths = scan tail (depth - 1) in
                                             let matches, _, failing_tail = rev_equals Decision.equals decision_path failing_path depth in
                                             if matches then failing_tail :: bounding_paths else bounding_paths
                                        | _ :: tail -> scan tail (depth - 1)
                                        | [] -> []
                                    in
                                    let bounding_paths = scan job.decisionPath (min failing_path_length (length job.decisionPath)) in
                                    if bounding_paths = [] then job_to_bounding_paths
                                    else JobMap.add job bounding_paths job_to_bounding_paths
                                ) job_to_bounding_paths jobs
                            in
                            BackOtterUtilities.time "BidirectionalQueue.t#get/update_job_to_bounding_paths" update_job_to_bounding_paths ()
                        | None -> job_to_bounding_paths
                    in
                    if not (JobMap.is_empty job_to_bounding_paths) then
                        get_job_from_job_to_bounding_paths job_to_bounding_paths

                    else let regular_get () = (* Regular get *)
                        (* If there's no more entry jobs, the forward search has ended. So we terminate. *)
                        if is_bidirectional && entryfn_jobqueue#length = 0 then None else

                        (* Set up targets, which maps target functions to their failing paths, and
                         * target_fundecs, which is basically the key set of targets *)
                        let targets = !targets_ref in
                        let target_fundecs = BackOtterTargets.get_fundecs targets in

                        (* Create new jobs for callers of new targets/failure_fn *)
                        let origin_fundecs', otherfn_jobqueue =
                            let impl () =
                            List.fold_left (
                                fun (origin_fundecs, otherfn_jobqueue) target_fundec ->
                                    let callers = CilUtilities.CilCallgraph.find_callers file target_fundec in
                                    List.fold_left (
                                        fun (origin_fundecs, otherfn_jobqueue) caller ->
                                            if (is_bidirectional && caller == entry_fn) || List.memq caller origin_fundecs then
                                                origin_fundecs, otherfn_jobqueue
                                            else
                                                let job =
                                                    if caller == entry_fn then entry_job
                                                    else (
                                                        Output.debug_printf "Create new job for function %s@\n" caller.svar.vname;
                                                        OtterJob.FunctionJob.make file caller)
                                                in
                                                caller :: origin_fundecs, otherfn_jobqueue#put job
                                    ) (origin_fundecs, otherfn_jobqueue) callers
                            ) (origin_fundecs, otherfn_jobqueue) (failure_fn :: target_fundecs)
                            in BackOtterUtilities.time "BidirectionalQueue.t#get/create_new_jobs" impl ()
                        in

                        (* debug, Debug, DEBUG (warning: these can slow down regular_get) *)
                        Output.debug_printf "Number of entry function jobs: %d@\n" (entryfn_jobqueue#length);
                        Output.debug_printf "Number of other function jobs: %d@\n" (otherfn_jobqueue#length);
                        List.iter (fun f -> Output.debug_printf "Target function: %s@\n" f.svar.vname) (BackOtterTargets.get_fundecs targets);
                        List.iter (fun f -> Output.debug_printf "Origin function: %s@\n" f.svar.vname) (origin_fundecs');

                        (* Determine whether to run entry function jobs or other function jobs *)
                        let want_process_entryfn =
                            let entryfn_time_elapsed, otherfn_time_elapsed = !timer_ref in
                            let total_elapsed = entryfn_time_elapsed +. otherfn_time_elapsed in
                            if total_elapsed <= 0.0001 (* epsilon *) then ratio > 0.0
                            else entryfn_time_elapsed /. total_elapsed <= ratio
                        in
                        if (not (entryfn_jobqueue#length = 0)) && (otherfn_jobqueue#length = 0 || want_process_entryfn)  then
                            (* Do forward search *)
                            match entryfn_jobqueue#get with
                            | Some (entryfn_jobqueue, job) ->
                                Some ({< entryfn_jobqueue = entryfn_jobqueue;
                                         otherfn_jobqueue = otherfn_jobqueue;  (* might have been added with new jobs from above *)
                                         origin_fundecs = origin_fundecs';
                                         job_to_bounding_paths = job_to_bounding_paths;
                                         last_bounded_job_from_get = None >}, job)
                            | None -> failwith "This is unreachable"
                        else if otherfn_jobqueue#length > 0 then
                            (* Do "backward" search *)
                            match otherfn_jobqueue#get with
                            | Some (otherfn_jobqueue, job) ->
                                Some ({< otherfn_jobqueue = otherfn_jobqueue;
                                         origin_fundecs = origin_fundecs';
                                         job_to_bounding_paths = job_to_bounding_paths;
                                         last_bounded_job_from_get = None >}, job)
                            | None -> failwith "This is unreachable"
                        else
                            None
                    in
                    BackOtterUtilities.time "BidirectionalQueue.t#get/regular_get" regular_get ()

        in
        BackOtterUtilities.time "BidirectionalQueue.t#get" get ()

    end

(** {1 Command-line options} *)

let options = [
    "--bidirectional-search-ratio",
        Arg.Set_float default_bidirectional_search_ratio,
        "<ratio> The fraction of computation dedicated to forward search (default: 0.5)";
]

