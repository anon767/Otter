(* A queue that ranks based on
 * 1. Whether a job is on bounding paths
 * 2. Distance to targets
 * 3. Whether the function is main(), i.e., forward search.
 *    Breath-first in terms of number of function calls.
 * 4. (Optional) How long a function has run (to avoid get stuck)
 *
 * TODO: optimize findmin
 *
 * Plan:
 * 1. Separate the only entry_fn job (forward search) from the remaining jobs.
 *    Define a ratio r% so that the forward search is done r% of the time. (TODO variating ratio?)
 * 2.
 *)
open OtterGraph
open OtterCore
open Types
open Job
open Cil

let high_score = infinity
let base_score = 100.0
let low_score = neg_infinity

(* TODO: a better metric. In particular,
 * 1. Bring in the min-cost estimate for each function call
 * 2. Account for the fact that a target may be reachable after the current function call returns *)
let distance_to_targets job target_fundecs =
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


class ['job] t targets_ref entry_fn = object
    val jobs = []
    val forward_search = false

    method put (job : 'job) =
        {< jobs = job::jobs >}

    method get =
        let score_function job =
            let origin_function = List.hd (List.rev job.state.callstack) in
            let target_fundecs = BackOtterTargets.get_fundecs (!targets_ref) in
            let distance = float_of_int (distance_to_targets job target_fundecs) in
            match job.boundingPaths with
            | Some(_::_) -> base_score *. 2.0 -. distance (* bounding paths have higher importance *)
            | Some([]) -> low_score
            | None ->
                if forward_search then
                    if origin_function == entry_fn then
                        -. (float_of_int (List.length job.decisionPath)) (* Almost BFS *)
                    else
                        low_score
                else
                    if origin_function == entry_fn then
                        low_score
                    else
                        base_score -. distance
        in
        let rest_jobs, returned_job_opt, _ = List.fold_left (
            fun (rest_jobs, returned_job_opt, score) job ->
                let new_score = score_function job in
                match returned_job_opt with
                | None -> rest_jobs, Some job, new_score
                | Some (returned_job) ->
                    if new_score > score then
                        returned_job :: rest_jobs, Some job, new_score
                    else
                        job :: rest_jobs, Some returned_job, score
        ) ([], None, 0.0) jobs
        in
        match returned_job_opt with
        | None -> None
        | Some (returned_job) ->
            Some ({< jobs = rest_jobs ; forward_search = not forward_search >}, returned_job)
end
