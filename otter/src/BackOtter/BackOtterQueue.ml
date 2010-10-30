open OcamlUtilities
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
 * 1. Target-driven followed by coverage-driven
 * 2. Include distance-from-entry.
 * 3. Better implementation of get_distance_to_targets
 * 4. More efficient implementation of update_bounding_paths_with_targets. Basically we only need to update jobs that are affected by the last discovery of failing path.
 *)
let default_bidirectional_search_ratio = ref 0.5

let get_origin_function job = List.hd (List.rev job.state.callstack)

let get_job_with_highest_score score_fn jobs =
    let jobs, jobopt, score = List.fold_left (fun (jobs, jobopt, score) job' ->
        let score' = score_fn job' in
        match jobopt with
        | Some job ->
            if score' > score then
                job :: jobs, Some job', score'
            else
                job' :: jobs, Some job, score
        | None -> jobs, Some job', score'
    ) ([], None, 0.0) jobs in
    match jobopt with
    | Some job -> jobs, job
    | None -> failwith "get_job_with_highest_score assumes a non empty list"

let get_distance_to_targets target_fundecs job =
    let graph,root = Graph.make_graph (List.hd job.state.callstack) in
    let target_nodes =
        Graph.filter_nodes graph
        begin
            fun node -> match node.Graph.obj with
            | InstrStmt.Instr((Call(_,Lval(Var(varinfo),_),exps,_))::_,_)  ->
                    List.fold_left (fun b t -> if t.svar == varinfo then true else b) false target_fundecs
            | _ -> false
        end
    in
    let get_predicate job node =
        match job.instrList,job.stmt,node.Graph.obj with
        | [],stmt,InstrStmt.Stmt(stmt') when stmt==stmt' -> true
        | [],_,_ -> false
        | instrs,_,InstrStmt.Instr(instrs',_) when instrs==instrs' -> true
        | _ -> false
    in
    let sources = Graph.filter_nodes graph (get_predicate job) in
    assert(List.length sources = 1);
    let source = List.hd sources in
    Graph.set_color graph max_int;
    let backward_distance_from_targets =
        List.fold_left (fun d tar ->
            let d' = Graph.backward_distance graph source tar in
            min d d'
        ) max_int target_nodes
    in
    backward_distance_from_targets


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


class ['job] t file targets_ref entry_fn failure_fn = object (self)
    val entryfn_jobs = []
    val otherfn_jobs = []
    val entryfn_processed = 0
    val otherfn_processed = 0
    val origin_fundecs = [entry_fn] (* fundecs whose FunctionJob-initialized jobs have been created, or entry_fn *)

    method put (job : 'job) =
        if get_origin_function job == entry_fn then
            {< entryfn_jobs = job :: entryfn_jobs >}
        else
            {< otherfn_jobs = job :: otherfn_jobs >}

    method get =
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
        let update_bounding_paths_with_targets = update_bounding_paths targets in
        let entryfn_jobs' = List.map update_bounding_paths_with_targets entryfn_jobs in
        let otherfn_jobs' = List.map update_bounding_paths_with_targets otherfn_jobs in
        try
            (* Check if any job is on bounding paths (i.e., being verified) *)
            let job = List.find has_bounding_paths (List.rev_append entryfn_jobs' otherfn_jobs') in
            Output.debug_printf "Job %d is run under influence of bounding path(s) from function %s:@\n" job.jid (get_origin_function job).svar.vname;
            (match job.boundingPaths with | None -> () | Some bounding_paths ->
                List.iter (fun path -> Output.debug_printf "Path: @[%a@]@\n" Decision.print_decisions path) bounding_paths);
            let entryfn_jobs'' = List.filter (fun j -> j != job) entryfn_jobs' in
            let otherfn_jobs'' = List.filter (fun j -> j != job) otherfn_jobs' in
            Some ({< entryfn_jobs = entryfn_jobs'';
                     otherfn_jobs = otherfn_jobs'';
                     origin_fundecs = origin_fundecs' >}, job)
        with Not_found ->
            if entryfn_jobs <> [] && (otherfn_jobs = [] || self#want_process_entryfn)  then
                (* Do forward search *)
                let score job = -. (float_of_int (List.length job.decisionPath)) in (* Approximated execution path length *)
                let entryfn_jobs', job = get_job_with_highest_score score entryfn_jobs in
                Some ({< entryfn_jobs = entryfn_jobs';
                         otherfn_jobs = otherfn_jobs;
                         origin_fundecs = origin_fundecs';
                         entryfn_processed = entryfn_processed + 1 >}, job)
            else if otherfn_jobs <> [] then
                (* Do "backward" search *)
                (* TODO: include closest to entryfn first *)
                let score job =
                    let distance = get_distance_to_targets (failure_fn :: target_fundecs) job in
                    -. (float_of_int distance)
                in
                let otherfn_jobs', job = get_job_with_highest_score score otherfn_jobs in
                Some ({< otherfn_jobs = otherfn_jobs';
                         origin_fundecs = origin_fundecs';
                         otherfn_processed = otherfn_processed + 1 >}, job)
            else
                None

    method private want_process_entryfn =
        let total_processed = entryfn_processed + otherfn_processed in
        let ratio = !default_bidirectional_search_ratio in
        if total_processed = 0 then ratio > 0.0
        else (float_of_int entryfn_processed) /. (float_of_int total_processed) <= ratio

end

(** {1 Command-line options} *)

let options = [
    "--bidirectional-search-ratio",
        Arg.Set_float default_bidirectional_search_ratio,
        "<ratio> The fraction of computation dedicated to forward search (default: 0.5)";
]

