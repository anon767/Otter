open CilUtilities
open OtterCFG
open OtterCore
open Types
open Job
open Cil


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
