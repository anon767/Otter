open BackOtterUtilities
open DataStructures
open OcamlUtilities
open OtterCore
open Types
open Job
open Decision
open Cil

let default_bidirectional_search_ratio = ref 0.5

let points_tos = [
    "regular", CilUtilities.CilPtranal.points_to;
    "naive", CilUtilities.CilPtranal.naive_points_to;
    "unsound", CilUtilities.CilPtranal.unsound_points_to;
]
let default_points_to = ref CilUtilities.CilPtranal.points_to

(*
 * Improve efficiency of bounding paths update/checking.
 *
 * Let DP: decision path, BP: bounding path, FP: failing path.
 * Invariants:
 * 1. DP is most recent decision first. FP and BP are least recent decision first.
 * 2. DP and BP do not overlap---(hd BP) is the decision that a job with DP has to follow.
 * 3. jid_to_bounding_paths always has jobs that are mapped to non-empty bounding_paths.
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
module JidMap = Map.Make (struct
    type t = int
    let compare a b = a - b
end)

(* This function is to make sure that a decision path with a function call in
 * latest decision won't create bounded job more than once *)
let function_call_of_latest_decision =
    let memotable = Hashtbl.create 0 in
    function
    | DecisionFuncall (_, fundec) :: _ as decisions->
        if Hashtbl.mem memotable decisions then None
        else (Hashtbl.add memotable decisions (); Some fundec)
    | _ -> None

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

        (* A worklist for bounded jobs. I guess the strategy does not matter. *)
        val bounded_jobqueue = new BackOtterQueue.RankedQueue.t [ new BackOtterQueue.DepthFirstStrategy.t ]
        (* A mapping from jid to boundingPaths *)
        val jid_to_bounding_paths = JidMap.empty
        (* A mapping from jid to job *)
        val jid_to_job = JidMap.empty

        method put (job : 'job) =
            let put () =
            (* If the job is from a bounded job,
             * if it's still in bound, put it into jid_to_bounding_paths,
             * otherwise, discard it.
             *
             * Else, put the job into the jobqueue.
             * If the job's next instr is a function call, and if the function has failing paths,
             * bound the job and put it in jid_to_bounding_paths.
             *
             * Idea: in #put, jobs are bounded if they are from bounded jobs or they are calls to
             *       targets.
             *
             * Given job = (DP, BP), to check if the child job' = DP' is bounded by BP,
             * If DP == DP', "YES" (there's no new decision)
             * Else assert(tl(DP') == DP), "YES" if hd(DP') == hd(BP)
             *)
            let jid_to_job = JidMap.add job.jid_unique job jid_to_job in
            (* If job comes from a bounded job *)
            let _ = Output.debug_printf "Job %d has parent %d@\n" job.jid_unique job.jid_parent in
            if JidMap.mem job.jid_parent jid_to_bounding_paths then
                let parent_job = JidMap.find job.jid_parent jid_to_job in
                let bounding_paths = JidMap.find parent_job.jid_unique jid_to_bounding_paths in
                if parent_job.decisionPath == job.decisionPath then (* no new decision *)
                    let _ = Output.debug_printf "No new decision@\n" in
                    let _ = Output.debug_printf "Add job_unique %d into the bounded_jobqueue @\n" job.jid_unique in
                    {< jid_to_bounding_paths = JidMap.add job.jid_unique bounding_paths jid_to_bounding_paths;
                       jid_to_job = jid_to_job;
                       bounded_jobqueue = bounded_jobqueue#put job; >}
                else (* has new decision *)
                    let _ = Output.debug_printf "Has new decision@\n" in
                    let _ = assert(parent_job.decisionPath == List.tl job.decisionPath) in
                    let bounded_decision = List.hd job.decisionPath in
                    let bounding_paths = List.fold_left (
                        fun acc path -> if Decision.equals bounded_decision (List.hd path) then (List.tl path) :: acc else acc
                    ) [] bounding_paths in
                    if bounding_paths = [] then (* out bound *)
                        let _ = Output.debug_printf "Out bound@\n" in
                        self (* Discard the job *)
                    else
                        let _ = Output.debug_printf "Add job_unique %d into the bounded_jobqueue @\n" job.jid_unique in
                        {< jid_to_bounding_paths = JidMap.add job.jid_unique bounding_paths jid_to_bounding_paths;
                           jid_to_job = jid_to_job;
                           bounded_jobqueue = bounded_jobqueue#put job; >}
            else
            (* If job does not from a bounded job, i.e., regular *)
                let entryfn_jobqueue, otherfn_jobqueue =
                    if is_bidirectional && get_origin_function job == entry_fn then
                        entryfn_jobqueue#put job, otherfn_jobqueue
                    else
                        entryfn_jobqueue, otherfn_jobqueue#put job
                in
                (* Check if the job is a function call to a target function *)
                let jid_to_job, jid_to_bounding_paths, bounded_jobqueue =
                    match function_call_of_latest_decision job.decisionPath with
                    | Some (fundec) ->
                        let failing_paths = BackOtterTargets.get fundec (!targets_ref) in
                        if List.length failing_paths = 0 then jid_to_job, jid_to_bounding_paths, bounded_jobqueue (* Not a target function *)
                        else
                            let _ = Output.debug_printf "Call target function %s@\n" fundec.svar.vname in
                            let bounded_job = {job with jid_unique = Counter.next Job.job_counter_unique;} in
                            let _ = Output.debug_printf "Add job_unique %d into the bounded_jobqueue @\n" bounded_job.jid_unique in
                            JidMap.add bounded_job.jid_unique bounded_job jid_to_job,
                            JidMap.add bounded_job.jid_unique failing_paths jid_to_bounding_paths,
                            bounded_jobqueue#put bounded_job
                    | None -> jid_to_job, jid_to_bounding_paths, bounded_jobqueue
                in
                {< entryfn_jobqueue = entryfn_jobqueue;
                   otherfn_jobqueue = otherfn_jobqueue;
                   jid_to_bounding_paths = jid_to_bounding_paths;
                   jid_to_job = jid_to_job;
                   bounded_jobqueue = bounded_jobqueue; >}
        in
        Timer.time "BidirectionalQueue.t#put" put ()

        method get =
            let get () =
                (* Clear the label, as anything printed here has no specific job context *)
                Output.set_formatter (new Output.plain);

                (* First check if there're existing jobs in jid_to_bounding_paths.
                 * If so, simply return one of them.
                 *
                 * Else, assert(jid_to_bounding_paths is empty)
                 * If there exists a new failing path, update all job's bounding paths.
                 * And add jobs into jid_to_bounding_paths if they have any.
                 *
                 * If jid_to_bounding_paths is not empty, take a job from it and return.
                 * Else, return jobqueue#get.
                 *
                 * Idea: in #get, jobs are bounded if the new failing path has effect on them.
                 *)
                (* TODO
                let jid_to_bounding_paths =
                    match last_bounded_job_from_get with
                    | Some job -> JidMap.remove job.jid_unique jid_to_bounding_paths
                    | None -> jid_to_bounding_paths
                in
                *)
                match bounded_jobqueue#get with
                | Some (bounded_jobqueue, job) ->
                    (* Has bounded job to run *)
                    let _ = Output.debug_printf "Take job_unique %d from bounded_jobqueue@\n" job.jid_unique in
                    Some ({< bounded_jobqueue = bounded_jobqueue; >}, job)
                | None ->
                    (* For each existing job, see if it can be bounded. If so, update bounded_jobqueue et al *)
                    let targets, new_failing_path = BackOtterTargets.get_last_failing_path (!targets_ref) in
                    targets_ref := targets;
                    let jid_to_job, jid_to_bounding_paths, bounded_jobqueue =
                        match new_failing_path with
                        | Some (target_fundec, failing_path) ->
                            Timer.time "BidirectionalQueue.t#get/update_bounding_status" begin fun () ->
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
                                List.fold_left (fun (jid_to_job, jid_to_bounding_paths, bounded_jobqueue) job ->
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
                                    if bounding_paths = [] then (jid_to_job, jid_to_bounding_paths, bounded_jobqueue)
                                    else
                                        let bounded_job = {job with jid_unique = Counter.next Job.job_counter_unique;} in
                                        let _ = Output.debug_printf "Add job_unique %d into the bounded_jobqueue @\n" bounded_job.jid_unique in
                                        JidMap.add bounded_job.jid_unique bounded_job jid_to_job,
                                        JidMap.add bounded_job.jid_unique bounding_paths jid_to_bounding_paths,
                                        bounded_jobqueue#put bounded_job
                                ) (jid_to_job, jid_to_bounding_paths, bounded_jobqueue) jobs
                            end ()
                        | None -> (jid_to_job, jid_to_bounding_paths, bounded_jobqueue)
                    in
                    match bounded_jobqueue#get with
                    | Some (bounded_jobqueue, job) ->
                        (* Has bounded job to run *)
                        let _ = Output.debug_printf "Take job from bounded_jobqueue@\n" in
                        Some ({< bounded_jobqueue = bounded_jobqueue;
                                 jid_to_bounding_paths = jid_to_bounding_paths;
                                 jid_to_job = jid_to_job; >}, job)
                    | None -> (* Regular get *)
                        (* assert: jid_to_job and jid_to_bounding_paths are unchanged at this point, and bounded_jobqueue is always empty *)
                        Timer.time "BidirectionalQueue.t#get/regular_get" begin fun () ->
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
                                                        OtterJob.FunctionJob.make file ~points_to:(!default_points_to file) caller)
                                                in
                                                caller :: origin_fundecs, otherfn_jobqueue#put job
                                    ) (origin_fundecs, otherfn_jobqueue) callers
                            ) (origin_fundecs, otherfn_jobqueue) (failure_fn :: target_fundecs)
                            in Timer.time "BidirectionalQueue.t#get/create_new_jobs" impl ()
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
                            let forward_search () =
                            match entryfn_jobqueue#get with
                            | Some (entryfn_jobqueue, job) ->
                                Some ({< entryfn_jobqueue = entryfn_jobqueue;
                                         otherfn_jobqueue = otherfn_jobqueue;  (* might have been added with new jobs from above *)
                                         origin_fundecs = origin_fundecs';
                                         jid_to_bounding_paths = jid_to_bounding_paths;
                                         >}, job)
                            | None -> failwith "This is unreachable"
                            in
                            Timer.time "BidirectionalQueue.t#get/forward_search" forward_search ()
                        else if otherfn_jobqueue#length > 0 then
                            (* Do "backward" search *)
                            let backward_search () =
                            match otherfn_jobqueue#get with
                            | Some (otherfn_jobqueue, job) ->
                                Some ({< otherfn_jobqueue = otherfn_jobqueue;
                                         origin_fundecs = origin_fundecs';
                                         jid_to_bounding_paths = jid_to_bounding_paths;
                                         >}, job)
                            | None -> failwith "This is unreachable"
                            in
                            Timer.time "BidirectionalQueue.t#get/backward_search" backward_search ()
                        else
                            None
                        end ()

        in
        Timer.time "BidirectionalQueue.t#get" get ()

    end

(** {1 Command-line options} *)

let options = [
    "--bidirectional-search-ratio",
        Arg.Set_float default_bidirectional_search_ratio,
        "<ratio> The fraction of computation dedicated to forward search (default: 0.5)";
    "--points-to",
        Arg.Symbol (fst (List.split points_tos), fun name -> default_points_to := List.assoc name points_tos),
        "<points_to> Set the default points_to analysis (default: regular)";
]

