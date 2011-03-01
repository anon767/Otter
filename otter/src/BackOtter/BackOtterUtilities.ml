open DataStructures
open CilUtilities
open OcamlUtilities
open OtterCFG
open OtterCore
open State
open Cil


let max_distance = max_int

let length =
    let length = Memo.memo_rec "BackOtterUtilities.length" begin fun length (lst : Decision.t list) ->
        match lst with
            | [] -> 0
            | _::rest -> length rest + 1
    end in
    fun lst -> Profiler.global#call "BackOtterUtilities.length" begin fun () ->
        length lst
    end

let rev_equals =
    let module Memo = Memo.Make (struct
        type t = (Decision.t -> Decision.t -> bool) * Decision.t list * Decision.t list * int
        let hash = Hashtbl.hash
        let equal (xeq, x1, x2, xb) (yeq, y1, y2, yb) =
            (* TODO: should the lists be compared structurally instead? *)
            xeq == yeq && x1 == y1 && x2 == y2 && xb == yb
    end) in
    let rev_equals = Memo.memo_rec "BackOtterUtilities.rev_equals"
        begin fun rev_equals (eq, lst1, lst2, n) -> (* Assume length lst1 >= n *)
            if n <= 0 then
                (true, lst1, lst2)
            else
                let b, suf1, suf2 = rev_equals (eq, List.tl lst1, lst2, n - 1) in
                (b && eq (List.hd lst1) (List.hd suf2), suf1, (List.tl suf2))
        end
    in
    fun eq lst1 lst2 n -> Profiler.global#call "BackOtterUtilities.rev_equals" begin fun () ->
        rev_equals (eq, lst1, lst2, n)
    end

let get_last_element =
    let get_last_element = Memo.memo_rec "BackOtterUtilities.get_last_element"
        begin fun get_last_element lst -> match lst with
            | [] -> invalid_arg "get_last_element: empty list"
            | [ele] -> ele
            | _ :: tail -> get_last_element tail
        end
    in
    fun lst -> Profiler.global#call "BackOtterUtilities.get_last_element" begin fun () ->
        match lst with
            | [] -> invalid_arg "get_last_element: empty list"
            | [ele] -> ele
            | lst -> get_last_element lst
    end


let get_origin_function job = Profiler.global#call "get_origin_function" (fun () -> get_last_element job#state.callstack)


let get_origin_function_from_job_result job_result = Profiler.global#call "get_origin_function_from_job_result" (fun () -> get_last_element job_result#state.callstack)


let rec lex_compare cmp lst1 lst2 =
    match lst1, lst2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | h1::t1, h2::t2 ->
        let c = cmp h1 h2 in
        if c = 0 then lex_compare cmp t1 t2 else c


let get_distance_from =
    let module Memo = Memo.Make (struct
        type t = CilData.CilFile.t * CilData.CilFundec.t * CilData.CilFundec.t
        let hash (file, f1, f2) = Hashtbl.hash (CilData.CilFile.hash file, CilData.CilFundec.hash f1, CilData.CilFundec.hash f1)
        let equal (xfile, xf1, xf2) (yfile, yf1, yf2) =
            CilData.CilFile.equal xfile yfile && CilData.CilFundec.equal xf1 yf1 && CilData.CilFundec.equal xf2 yf2
    end) in
    let get_distance_from = Memo.memo "BackOtterUtilities.get_distance_from"
        begin fun (file, f1, f2) ->
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
        end
    in
    fun file f1 f2 -> Profiler.global#call "BackOtterUtilities.get_distance_from" begin fun () ->
        get_distance_from (file, f1, f2)
    end

(* Returns a function that callee can transitively be inlined in *)
let rec get_transitive_unique_caller file callee = 
    let can_inline_function caller callee =
        (* Either caller is "simple", or callee is "main". The latter is a hack. *)
        let ret = 
            callee.svar.vname = "main" ||
            match caller.smaxstmtid with Some size -> size < 5 | None -> false  (* TODO: true if caller does not branch *)
        in
        (if ret then Output.debug_printf "Inline %s in %s@." callee.svar.vname caller.svar.vname);
        ret
    in
    match CilCallgraph.find_callers file callee with
    | [ caller ] when can_inline_function caller callee -> get_transitive_unique_caller file caller
    | _ -> callee
