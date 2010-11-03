open CilUtilities
open OcamlUtilities
open OtterCFG
open OtterGraph
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
 * 2. More efficient implementation of update_bounding_paths_with_targets.
 *    Basically we only need to update jobs that are affected by the last discovery of failing path.
 *)
let computation_unit = [
    "time", `Time;
    "step", `Step;
]

let default_bidirectional_search_ratio = ref 0.5
let default_bidirectional_search_method = ref `Time  (* time | step *)
let max_distance = max_int


let get_origin_function job = List.hd (List.rev job.state.callstack)

let get_job_with_highest_score ?(compare=Pervasives.compare) score_fn jobs =
    let jobs, maxopt = List.fold_left (fun (jobs, maxopt) job' ->
        let score' = score_fn job' in
        match maxopt with
        | Some (job, score) ->
            if compare score score' < 0 then
                job :: jobs, Some (job', score')
            else
                job' :: jobs, Some (job, score)
        | None -> jobs, Some (job', score')
    ) ([], None) jobs in
    match maxopt with
    | Some (job, _) -> jobs, job
    | None -> failwith "get_job_with_highest_score assumes a non empty list"


let get_distance_to_targets target_fundecs job =
    if target_fundecs = [] then
        max_distance (* = max_int in DistanceToTargets *)
    else
        let file = job.Job.file in
        let source = Job.get_instruction job in
        let target_instrs = List.map (fun f -> Instruction.of_fundec file f) target_fundecs in
        let remaining_instrs stmt instr = match stmt.skind with
            | Instr (instrs) -> let rec behead = function [] -> [] | h::t -> if h == instr then h::t else behead t in behead instrs
            | _ -> invalid_arg "stmt must be a list of instrs"
        in
        let context = List.fold_right2 (
            fun call fundec context -> match call with
            | Source (_,stmt,instr,_) -> (Instruction.make file fundec stmt (remaining_instrs stmt instr)) :: context
            | _ -> context
                (* Don't need to capture the calling context beyond a Types.NoReturn,
                 * since Otter will return an error if it returns from a NoReturn function,
                 * i.e., any targets beyond a NoReturn will be unreachable. *)
            )
            (List.rev (List.tl (List.rev job.state.callContexts))) (* Discard the last element Runtime from callContexts *)
            (List.tl job.state.callstack)                          (* Discard the first function from the callstack *)
            []
        in
        DistanceToTargets.find_in_context source target_instrs context

let get_distance_to_targets_within_function target_fundecs job =
    let file = job.Job.file in
    let current_fundec = List.hd job.state.callstack in
    let callees = CilCallgraph.find_callees file current_fundec in
    let target_fundecs = List.filter (fun target_fundec -> List.memq target_fundec callees) target_fundecs in
    get_distance_to_targets target_fundecs job


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
    {job with boundingPaths = Some bounding_paths;} (* TODO: make boundingPaths not option *)


let has_bounding_paths job = match job.boundingPaths with
    | Some (_ :: _) -> true
    | _ -> false

(* TODO: cache the result. Or maybe this is already implemented somewhere. *)
let get_distance_from file f1 f2 =
    let rec bfs = function
        | [] -> max_distance
        | (f, d) :: tail ->
            if f == f2 then d else
            let callees = CilCallgraph.find_callees file f in
            let tail = List.fold_left (fun tail callee ->
                if List.exists (fun (k,_) -> k == callee) tail then tail
                else tail @ [(callee, d+1)]
            ) tail callees in
            bfs tail
    in
    bfs [(f1, 0)]


type job_type = EntryfnJob of job | OtherfnJob of job

class ['job] t file targets_ref entry_fn failure_fn = object (self)
    val entryfn_jobs = []
    val otherfn_jobs = []
    val entryfn_processed = 0
    val otherfn_processed = 0
    val origin_fundecs = [entry_fn] (* fundecs whose FunctionJob-initialized jobs have been created, or entry_fn *)
    (* TODO: refactor this mess *)
    val last_time = None
    val last_job_type = None
    val entryfn_time_elapsed = 0.0
    val otherfn_time_elapsed = 0.0

    method put (job : 'job) =
        if get_origin_function job == entry_fn then
            {< entryfn_jobs = job :: entryfn_jobs >}
        else
            {< otherfn_jobs = job :: otherfn_jobs >}

    method get =
        (* Clear the label, as anything printed here has no specific job context *)
        Output.set_formatter (new Output.plain);

        (* If there's no more entry jobs, the forward search has ended. So we terminate. *)
        if entryfn_jobs = [] then None else

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

        Output.debug_printf "Number of entry function jobs: %d@\n" (List.length entryfn_jobs);
        Output.debug_printf "Number of other function jobs: %d@\n" (List.length otherfn_jobs);
        List.iter (fun f -> Output.debug_printf "Target function: %s@\n" f.svar.vname) (BackOtterTargets.get_fundecs targets);
        List.iter (fun f -> Output.debug_printf "Origin function: %s@\n" f.svar.vname) (origin_fundecs');

        let get_distance_from_entryfn = get_distance_from file entry_fn in
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
        if entryfn_jobs <> [] && (otherfn_jobs = [] || want_process_entryfn)  then
            (* Do forward search *)
            try
                let update_bounding_paths_with_targets = update_bounding_paths targets in
                let entryfn_jobs' = List.map update_bounding_paths_with_targets entryfn_jobs in
                let job = List.find has_bounding_paths entryfn_jobs' in
                Output.debug_printf "Job %d is run under influence of bounding path(s) from function %s:@\n" job.jid (get_origin_function job).svar.vname;
                (match job.boundingPaths with | None -> () | Some bounding_paths ->
                    List.iter (fun path -> Output.debug_printf "Path: @[%a@]@\n" Decision.print_decisions path) bounding_paths);
                let entryfn_jobs' = List.filter (fun j -> j != job) entryfn_jobs' in
                Some ({< entryfn_jobs = entryfn_jobs';
                         origin_fundecs = origin_fundecs';
                         last_time = last_time;
                         last_job_type = Some (EntryfnJob job);
                         entryfn_time_elapsed = entryfn_time_elapsed;
                         otherfn_time_elapsed = otherfn_time_elapsed; >}, job)
            with Not_found ->
                let score job =
                    (* When some targets are found in the job's current function (i.e., close enough), we bias towards them. *)
                    (* TODO: sometimes the above strategy may not work well.... *)
                    let distance_to_target = get_distance_to_targets_within_function (failure_fn :: target_fundecs) job in
                    let path_length = ((List.length job.decisionPath)) in (* Approximated execution path length *)
                    [-distance_to_target; -path_length]
                in
                let entryfn_jobs', job = get_job_with_highest_score score entryfn_jobs in
                Some ({< entryfn_jobs = entryfn_jobs';
                         otherfn_jobs = otherfn_jobs;
                         origin_fundecs = origin_fundecs';
                         entryfn_processed = entryfn_processed + 1;
                         last_time = last_time;
                         last_job_type = Some (EntryfnJob job);
                         entryfn_time_elapsed = entryfn_time_elapsed;
                         otherfn_time_elapsed = otherfn_time_elapsed; >}, job)
        else if otherfn_jobs <> [] then
            (* Do "backward" search *)
            try
                let update_bounding_paths_with_targets = update_bounding_paths targets in
                let otherfn_jobs' = List.map update_bounding_paths_with_targets otherfn_jobs in
                let otherfn_jobs' = List.sort (fun j1 j2 -> (get_distance_from_entryfn (get_origin_function j1)) - (get_distance_from_entryfn (get_origin_function j2))) otherfn_jobs' in
                let job = List.find has_bounding_paths otherfn_jobs' in
                Output.debug_printf "Job %d is run under influence of bounding path(s) from function %s:@\n" job.jid (get_origin_function job).svar.vname;
                (match job.boundingPaths with | None -> () | Some bounding_paths ->
                    List.iter (fun path -> Output.debug_printf "Path: @[%a@]@\n" Decision.print_decisions path) bounding_paths);
                let otherfn_jobs' = List.filter (fun j -> j != job) otherfn_jobs' in
                Some ({< otherfn_jobs = otherfn_jobs';
                         origin_fundecs = origin_fundecs';
                         last_time = last_time;
                         last_job_type = Some (OtherfnJob job);
                         entryfn_time_elapsed = entryfn_time_elapsed;
                         otherfn_time_elapsed = otherfn_time_elapsed; >}, job)
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

