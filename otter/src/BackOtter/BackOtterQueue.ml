open BackOtterUtilities
open OcamlUtilities
open OtterCore
open Types
open Job
open Decision
open Cil

(* In general, during forward search, we want to prioritize based on the following, in decreasing order.
 * 1. Jobs running on bounding paths, because the sooner we verify failing paths, the sooner we have new failing paths for ongoing executions.
 * 2. Jobs from entry_fn: breath-first. Other jobs: split among {closer to targets, origin function closer to entry_fn}.
 *    (This results in DFS backward, and gives shortest path from main to failure if the callgraph is like a tree).
 *
 * TODO:
 * 1. Coverage-driven
 * 2. More efficient implementation of update_bounding_paths.
 *    Basically we only need to update jobs that are affected by the last discovery of failing path.
 *)

let default_bidirectional_search_ratio = ref 0.5


(* Given the failing paths for each target, construct bounding paths for the job.
 * Very inefficient, but working
 *
 * Returns (job, job with bounding paths, smallest depth of the first feasible attachment)
 * *)
let update_bounding_paths targets job =
    let update_bounding_paths () =
        let is_suffix eq suf lst =
            let rec is_prefix eq pre lst =
                match pre, lst with
                | d1::pre', d2::lst' -> if eq d1 d2 then is_prefix eq pre' lst' else -1
                | [], [] -> 0
                | [], _ -> 1
                | _, _ -> -1
            in
            is_prefix eq (List.rev suf) (List.rev lst)
        in
        let rec get_bounding_paths decision_path = match decision_path with
            | DecisionFuncall (_, fundec) :: decision_tail ->
                let failing_paths = BackOtterTargets.get fundec targets in
                let stitched_failing_paths = List.map (fun p -> List.append p decision_path) failing_paths in
                (* Take out paths from bounding_paths where job.decisionPath is not a suffix of them *)
                let stitched_failing_paths = List.filter (fun p -> is_suffix Decision.equals job.decisionPath p >= 0) stitched_failing_paths in
                let upper_bounding_paths, depth = get_bounding_paths decision_tail in
                let depth = if upper_bounding_paths = [] then (List.length decision_path) else depth in
                List.fold_left (fun paths path ->
                    if List.exists (fun p -> is_suffix Decision.equals p path = 0) paths then paths else path :: paths
                ) upper_bounding_paths stitched_failing_paths, depth
            | _ :: decision_tail -> get_bounding_paths decision_tail
            | [] -> [], max_int
        in
        let bounding_paths, depth = get_bounding_paths job.decisionPath in
        job, {job with boundingPaths = Some bounding_paths;}, depth (* TODO: make boundingPaths not option *)
    in
    Stats.time "BackOtterQueue.update_bounding_paths" update_bounding_paths ()


let has_bounding_paths (_,job,_) = match job.boundingPaths with
    | Some (_ :: _) -> true
    | _ -> false


type job_type = EntryfnJob of job | OtherfnJob of job

class ['job] t ?(ratio=(!default_bidirectional_search_ratio)) file targets_ref timer_ref entry_fn failure_fn f_queue = object (self)

    val otherfn_jobs = []
    val entryfn_jobqueue = new RemovableQueue.t (f_queue)
    val origin_fundecs = [entry_fn] (* fundecs whose FunctionJob-initialized jobs have been created, or entry_fn *)

    method put (job : 'job) =
        if get_origin_function job == entry_fn then
            {< entryfn_jobqueue = entryfn_jobqueue#put job >}
        else
            {< otherfn_jobs = job :: otherfn_jobs >}

    method get = let get () =
        (* Clear the label, as anything printed here has no specific job context *)
        Output.set_formatter (new Output.plain);

        (* If there's no more entry jobs, the forward search has ended. So we terminate. *)
        if entryfn_jobqueue#get_contents = [] then None else

        (* Set up targets, which maps target functions to their failing paths, and
         * target_fundecs, which is basically the key set of targets *)
        let targets = !targets_ref in
        let target_fundecs = BackOtterTargets.get_fundecs targets in

        (* Create new jobs for callers of new targets/failure_fn *)
        let origin_fundecs', otherfn_jobs =
            List.fold_left (
                fun (origin_fundecs, otherfn_jobs) target_fundec ->
                    let callers = CilUtilities.CilCallgraph.find_callers file target_fundec in
                    List.fold_left (
                        fun (origin_fundecs, otherfn_jobs) caller ->
                            if List.memq caller origin_fundecs then
                                origin_fundecs, otherfn_jobs
                            else
                                let job = OtterJob.FunctionJob.make file caller in
                                caller :: origin_fundecs, job :: otherfn_jobs
                    ) (origin_fundecs, otherfn_jobs) callers
            ) (origin_fundecs, otherfn_jobs) (failure_fn :: target_fundecs) in

        (* debug, Debug, DEBUG *)
        Output.debug_printf "Number of entry function jobs: %d@\n" (List.length (entryfn_jobqueue#get_contents));
        Output.debug_printf "Number of other function jobs: %d@\n" (List.length otherfn_jobs);
        List.iter (fun f -> Output.debug_printf "Target function: %s@\n" f.svar.vname) (BackOtterTargets.get_fundecs targets);
        List.iter (fun f -> Output.debug_printf "Origin function: %s@\n" f.svar.vname) (origin_fundecs');

        (* Return a pair, where the first element is the job in jobs that can have bounding paths,
         * and the second element is the job with bounding paths added.
         * The first element is used as the key to remove it from jobs.
         *)
        let find_job_with_bounding_paths jobs =
            let job_assocs = List.map (update_bounding_paths targets) jobs in
            let job_assocs = List.sort (fun (_,_,d1) (_,_,d2) -> d1 - d2) job_assocs in (* shallower depth first *)
            let (job, job_with_paths, depth) = List.find has_bounding_paths job_assocs in
            Output.debug_printf "Job %d is run under influence of bounding path(s) from function %s, with depth %d:@\n"
                job_with_paths.jid
                (get_origin_function job_with_paths).svar.vname
                depth;
            begin match job_with_paths.boundingPaths with
                | None -> ()
                | Some bounding_paths -> List.iter (fun path -> Output.debug_printf "Path: @[%a@]@\n" Decision.print_decisions path) bounding_paths
            end;
            job, job_with_paths
        in
        (* Determine whether to run entry function jobs or other function jobs *)
        let want_process_entryfn =
            (* Experimental: increase this ratio when there're plenty of failing paths discovered,
             * to avoid the backward search from taking too long... *)
            let num_of_toplevel_failing_paths =
                let callees = CilUtilities.CilCallgraph.find_callees file entry_fn in
                List.fold_left (fun num callee -> List.length (BackOtterTargets.get callee targets) + num) 0 callees
            in
            ignore num_of_toplevel_failing_paths;
            let entryfn_time_elapsed, otherfn_time_elapsed = !timer_ref in
            let total_elapsed = entryfn_time_elapsed +. otherfn_time_elapsed in
            if total_elapsed <= 0.0001 (* epsilon *) then ratio > 0.0
            else entryfn_time_elapsed /. total_elapsed <= ratio
        in
        if entryfn_jobqueue#get_contents <> [] && (otherfn_jobs = [] || want_process_entryfn)  then
            (* Do forward search *)
            try
                let job, job_with_paths = find_job_with_bounding_paths entryfn_jobqueue#get_contents in
                let entryfn_jobqueue = entryfn_jobqueue#remove job in
                Some ({< entryfn_jobqueue = entryfn_jobqueue;
                         otherfn_jobs = otherfn_jobs;  (* Jobs might have been added into ths queue *)
                         origin_fundecs = origin_fundecs'; >}, job_with_paths)
            with Not_found ->
                match entryfn_jobqueue#get with
                | Some (entryfn_jobqueue, job) ->
                    Some ({< entryfn_jobqueue = entryfn_jobqueue;
                             otherfn_jobs = otherfn_jobs;  (* Same as above *)
                             origin_fundecs = origin_fundecs'; >}, job)
                | None -> failwith "This is unreachable"
        else if otherfn_jobs <> [] then
            (* Do "backward" search *)
            let get_distance_from_entryfn = get_distance_from file entry_fn in
            try
                let otherfn_jobs = List.sort (fun j1 j2 ->
                    (get_distance_from_entryfn (get_origin_function j1)) -
                    (get_distance_from_entryfn (get_origin_function j2))) otherfn_jobs
                in
                let job, job_with_paths = find_job_with_bounding_paths otherfn_jobs in
                let otherfn_jobs = List.filter (fun j -> j != job) otherfn_jobs in
                Some ({< otherfn_jobs = otherfn_jobs;
                         origin_fundecs = origin_fundecs'; >}, job_with_paths)
            with Not_found ->
                let score job =
                    let num_of_failing_paths = List.length (BackOtterTargets.get (get_origin_function job) targets) in
                    let execution_path_length = List.length job.exHist.executionPath in
                    let distance_from_entryfn = get_distance_from_entryfn (get_origin_function job) in
                    let distance_to_target = get_distance_to_targets (failure_fn :: target_fundecs) job in
                    (* First pick the one closest to entry_fn, then the one closest to a target, then the most explored one *)
                    [-num_of_failing_paths; -distance_from_entryfn; -distance_to_target; execution_path_length]
                in
                let otherfn_jobs', job = get_job_with_highest_score score otherfn_jobs in
                Some ({< otherfn_jobs = otherfn_jobs';
                         origin_fundecs = origin_fundecs'; >}, job)
            else
                None
    in
    Stats.time "BackOtterQueue.t#get" get ()

end

(** {1 Command-line options} *)

let options = [
    "--bidirectional-search-ratio",
        Arg.Set_float default_bidirectional_search_ratio,
        "<ratio> The fraction of computation dedicated to forward search (default: 0.5)";
]

