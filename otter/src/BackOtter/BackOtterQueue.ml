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
let computation_unit = [
    "time", `Time;
    "step", `Step;
]

let default_bidirectional_search_ratio = ref 0.5
let default_bidirectional_search_method = ref `Time  (* time | step *)


(* Very inefficient, but working *)
let update_bounding_paths targets job =
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
            let bounding_paths = get_bounding_paths decision_tail in
            let failing_paths = BackOtterTargets.get fundec targets in
            let stitched_failing_paths = List.map (fun p -> List.append p decision_path) failing_paths in
            List.fold_left (fun paths path ->
                if List.exists (fun p -> is_suffix Decision.equals p path = 0) paths then paths else path :: paths
            ) bounding_paths stitched_failing_paths
        | _ :: decision_tail -> get_bounding_paths decision_tail
        | [] -> []
    in
    let bounding_paths = get_bounding_paths job.decisionPath in
    (* Take out paths from bounding_paths where job.decisionPath is not a suffix of them *)
    let bounding_paths = List.filter (fun p -> is_suffix Decision.equals job.decisionPath p >= 0) bounding_paths in
    job, {job with boundingPaths = Some bounding_paths;} (* TODO: make boundingPaths not option *)


let has_bounding_paths (_,job) = match job.boundingPaths with
    | Some (_ :: _) -> true
    | _ -> false


type job_type = EntryfnJob of job | OtherfnJob of job

class ['job] t file targets_ref entry_fn failure_fn = object (self)

    (* TODO: refactor this mess *)
    val otherfn_jobs = []
    val entryfn_jobqueue = new RemovableQueue.t (new SimpleEntryfnQueue.t (BackOtterTargets.get_fundecs (!targets_ref)))
    val entryfn_processed = 0
    val otherfn_processed = 0
    val origin_fundecs = [entry_fn] (* fundecs whose FunctionJob-initialized jobs have been created, or entry_fn *)
    val last_time = None
    val last_job_type = None
    val entryfn_time_elapsed = 0.0
    val otherfn_time_elapsed = 0.0

    method put (job : 'job) =
        if get_origin_function job == entry_fn then
            {< entryfn_jobqueue = entryfn_jobqueue#put job >}
        else
            {< otherfn_jobs = job :: otherfn_jobs >}

    method get =
        (* Clear the label, as anything printed here has no specific job context *)
        Output.set_formatter (new Output.plain);

        (* If there's no more entry jobs, the forward search has ended. So we terminate. *)
        if entryfn_jobqueue#get_contents = [] then None else

        (* Update time elapsed *)
        let current_time = Unix.gettimeofday () in
        let last_job_type, entryfn_time_elapsed, otherfn_time_elapsed = match last_time with
        | None -> last_job_type, entryfn_time_elapsed, otherfn_time_elapsed
        | Some t ->
            let time_elapsed = current_time -. t in
            let entryfn_time_elapsed, otherfn_time_elapsed =
                match last_job_type with
                | Some (EntryfnJob _) -> entryfn_time_elapsed +. time_elapsed, otherfn_time_elapsed
                | Some (OtherfnJob _) -> entryfn_time_elapsed, otherfn_time_elapsed +. time_elapsed
                | None -> entryfn_time_elapsed, otherfn_time_elapsed
            in
            last_job_type, entryfn_time_elapsed, otherfn_time_elapsed
        in
        let last_time = Some current_time in

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
            let (job, job_with_paths) = List.find has_bounding_paths job_assocs in
            Output.debug_printf "Job %d is run under influence of bounding path(s) from function %s:@\n"
                job_with_paths.jid
                (get_origin_function job_with_paths).svar.vname;
            begin match job_with_paths.boundingPaths with
                | None -> ()
                | Some bounding_paths -> List.iter (fun path -> Output.debug_printf "Path: @[%a@]@\n" Decision.print_decisions path) bounding_paths
            end;
            job, job_with_paths
        in
        (* Determine whether to run entry function jobs or other function jobs *)
        let want_process_entryfn =
            (* TODO: increase this ratio when there're plenty of failing paths discovered, and the backward search
             * starts taking too long... *)
            let ratio = !default_bidirectional_search_ratio in
            match (!default_bidirectional_search_method) with
            | `Time ->
                let total_elapsed = entryfn_time_elapsed +. otherfn_time_elapsed in
                if total_elapsed <= 0.0001 (* epsilon *) then ratio > 0.0
                else entryfn_time_elapsed /. total_elapsed <= ratio
            | `Step ->
                let total_processed = entryfn_processed + otherfn_processed in
                if total_processed = 0 then ratio > 0.0
                else (float_of_int entryfn_processed) /. (float_of_int total_processed) <= ratio
        in
        if entryfn_jobqueue#get_contents <> [] && (otherfn_jobs = [] || want_process_entryfn)  then
            (* Do forward search *)
            try
                let job, job_with_paths = find_job_with_bounding_paths entryfn_jobqueue#get_contents in
                let entryfn_jobqueue = entryfn_jobqueue#remove job in
                Some ({< entryfn_jobqueue = entryfn_jobqueue;
                         otherfn_jobs = otherfn_jobs;  (* Jobs might have been added into ths queue *)
                         origin_fundecs = origin_fundecs';
                         last_time = last_time;
                         last_job_type = Some (EntryfnJob job_with_paths);
                         entryfn_time_elapsed = entryfn_time_elapsed;
                         otherfn_time_elapsed = otherfn_time_elapsed; >}, job_with_paths)
            with Not_found ->
                match entryfn_jobqueue#get with
                | Some (entryfn_jobqueue, job) ->
                    Some ({< entryfn_jobqueue = entryfn_jobqueue;
                             otherfn_jobs = otherfn_jobs;  (* Same as above *)
                             origin_fundecs = origin_fundecs';
                             entryfn_processed = entryfn_processed + 1;
                             last_time = last_time;
                             last_job_type = Some (EntryfnJob job);
                             entryfn_time_elapsed = entryfn_time_elapsed;
                             otherfn_time_elapsed = otherfn_time_elapsed; >}, job)
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
                         origin_fundecs = origin_fundecs';
                         last_time = last_time;
                         last_job_type = Some (OtherfnJob job_with_paths);
                         entryfn_time_elapsed = entryfn_time_elapsed;
                         otherfn_time_elapsed = otherfn_time_elapsed; >}, job_with_paths)
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
                         origin_fundecs = origin_fundecs';
                         otherfn_processed = otherfn_processed + 1;
                         last_time = last_time;
                         last_job_type = Some (OtherfnJob job);
                         entryfn_time_elapsed = entryfn_time_elapsed;
                         otherfn_time_elapsed = otherfn_time_elapsed; >}, job)
            else
                None

end

(** {1 Command-line options} *)

let options = [
    "--bidirectional-search-ratio",
        Arg.Set_float default_bidirectional_search_ratio,
        "<ratio> The fraction of computation dedicated to forward search (default: 0.5)";
    "--bidirectional-search-method",
        Arg.Symbol (fst (List.split computation_unit), fun s -> default_bidirectional_search_method := List.assoc s computation_unit),
        "<time|step> Unit of computations of forward and backward searches (default: \"time\")";
]

