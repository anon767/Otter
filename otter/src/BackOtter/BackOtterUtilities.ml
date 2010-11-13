open DataStructures
open CilUtilities
open OtterCFG
open OtterCore
open Types
open Job
open Cil


let max_distance = max_int

let rec length =
    let memotable = Hashtbl.create 0 in
    function
        | [] -> 0
        | (lst:Decision.t list) ->
            try
                Hashtbl.find memotable lst
            with Not_found ->
                let len = length (List.tl lst) + 1 in
                Hashtbl.add memotable lst len;
                len

let rec rev_equals =
    let memotable = Hashtbl.create 0 in
    fun eq (lst1:Decision.t list) (lst2:Decision.t list) n -> (* Assume length lst1 >= n *)
        if n <= 0 then
            true, lst1, lst2
        else
            try
                Hashtbl.find memotable (eq, lst1, lst2, n)
            with Not_found ->
                let return =
                    let b, suf1, suf2 = rev_equals eq (List.tl lst1) lst2 (n-1) in
                    b && eq (List.hd lst1) (List.hd suf2), suf1, (List.tl suf2)
                in
                Hashtbl.add memotable (eq, lst1, lst2, n) return;
                return

let rec get_last_element =
    let memotable = Hashtbl.create 0 in
    function
        | [] -> invalid_arg "get_last_element: empty list"
        | [ele] -> ele
        | _ :: tail as lst ->
            try
                Hashtbl.find memotable lst
            with Not_found ->
                let last_ele = get_last_element tail in
                Hashtbl.add memotable lst last_ele;
                last_ele


let get_origin_function job = Stats.time "get_origin_function" get_last_element job.state.callstack


let get_origin_function_from_job_result job_result = Stats.time "get_origin_function_from_job_result" get_last_element job_result.result_state.callstack


let rec lex_compare cmp lst1 lst2 =
    match lst1, lst2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | h1::t1, h2::t2 ->
        let c = cmp h1 h2 in
        if c = 0 then lex_compare cmp t1 t2 else c


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
