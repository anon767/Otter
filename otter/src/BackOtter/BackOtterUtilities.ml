open DataStructures
open CilUtilities
open OcamlUtilities
open OtterCFG
open OtterCore
open State
open Cil


let arg_function_inlining = ref true

let length =
    let module Memo = Memo.Make (struct
        type t = Decision.t list 
        let hash = Hashtbl.hash
        let rec equal x y = match x,y with
            | hx :: x, hy :: y -> Decision.equal hx hy && equal x y
            | [], [] -> true
            | _, _ -> false
    end) in
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


(* Returns a function that callee can transitively be inlined in *)
let rec get_transitive_unique_caller file callee = 
    let can_inline_function caller callee =
        if (!arg_function_inlining) then
            (* Either caller is "simple", or callee is "main". The latter is a hack. *)
            let ret = 
                callee.svar.vname = "main" ||
                match caller.smaxstmtid with Some size -> size < 5 | None -> false  (* TODO: true if caller does not branch *)
            in
            (if ret then Output.debug_printf "Inline %s in %s@." callee.svar.vname caller.svar.vname);
            ret
        else
            false
    in
    match CilCallgraph.find_callers file callee with
    | [ caller ] when can_inline_function caller callee -> get_transitive_unique_caller file caller
    | _ -> callee

let options = [
	"--no-function-inlining",
		Arg.Clear arg_function_inlining,
		" Disable function inlining in BackOtter";
] 
