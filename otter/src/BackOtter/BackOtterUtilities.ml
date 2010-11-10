open DataStructures
open CilUtilities
open OtterCFG
open OtterCore
open Types
open Job
open Cil


let max_distance = max_int


let get_origin_function job = List.hd (List.rev job.state.callstack)


let shuffle lst =
    (* Hopefully no repeating indices... *)
    let lst = List.map (fun ele -> ele, Random.bits ()) lst in
    let lst = List.sort (fun (_,r1) (_,r2) -> Pervasives.compare r1 r2) lst in
    List.map (fun (ele,_) -> ele) lst


let get_job_with_highest_score ?(compare=Pervasives.compare) score_fn jobs =
    let get_job_with_highest_score () =
        let maxopt = List.fold_left (fun maxopt job' ->
            let score' = score_fn job' in
            match maxopt with
            | Some (job, score) ->
                if compare score score' < 0 then Some (job', score')
                else Some (job, score)
            | None -> Some (job', score')
        ) None (shuffle jobs) in
        match maxopt with
        | Some (job, _) -> List.filter (fun j -> j!=job) jobs, job
        | None -> failwith "get_job_with_highest_score assumes a non empty list"
    in
    Stats.time "BackOtterUtilities.get_job_with_highest_score" get_job_with_highest_score ()


let get_distance_to_targets target_fundecs job =
    let get_distance_to_targets () =
        if target_fundecs = [] then
            max_distance (* = max_int in DistanceToTargets *)
        else
            let source = Job.get_instruction job in
            let target_instrs = List.map (fun f -> Instruction.of_fundec job.Job.file f) target_fundecs in
            let context = Job.get_instruction_context job in
            DistanceToTargets.find_in_context source context target_instrs
    in
    Stats.time "BackOtterUtilities.get_distance_to_targets" get_distance_to_targets ()


let get_distance_to_targets_within_function target_fundecs job =
    let get_distance_to_targets_within_function () =
        let file = job.Job.file in
        let current_fundec = List.hd job.state.callstack in
        let callees = CilCallgraph.find_callees file current_fundec in
        let target_fundecs = List.filter (fun target_fundec -> List.memq target_fundec callees) target_fundecs in
        get_distance_to_targets target_fundecs job
    in
    Stats.time "BackOtterUtilities.get_distance_to_targets_within_function" get_distance_to_targets_within_function ()


let get_distance_from =
    let memotable = Hashtbl.create 0 in
    fun file f1 f2 ->
        try
            Hashtbl.find memotable (file, f1, f2)
        with Not_found ->
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
            let distance = bfs [(f1, 0)] in
            Hashtbl.add memotable (file, f1, f2) distance;
            distance

(* Timer *)
module StringMap = Map.Make (String)
type time_record = {
    time_elapsed : float;
    timer_started : bool;
}
let empty_time_record = {
    time_elapsed = 0.0;
    timer_started = false;
}
let global_timer = ref StringMap.empty
let time key f x =
    let time_record =
        try StringMap.find key (!global_timer)
        with Not_found -> empty_time_record
    in
    if time_record.timer_started then
        f x
    else begin
        global_timer := StringMap.add key {time_record with timer_started = true} (!global_timer);
        let starting_time = Unix.gettimeofday () in
        let return = f x in
        let time_elapsed = Unix.gettimeofday () -. starting_time in
        global_timer := StringMap.add key {
            time_elapsed = time_record.time_elapsed +. time_elapsed;
            timer_started = false;
        } (!global_timer);
        return
    end
let lookupTime key =
    let time_record =
        try StringMap.find key (!global_timer)
        with Not_found -> empty_time_record
    in
    time_record.time_elapsed
let keys () =
    StringMap.fold (fun k _ lst -> k :: lst) (!global_timer) []
