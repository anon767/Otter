open DataStructures
open CilUtilities
open OcamlUtilities
open OtterCFG
open OtterCore
open State
open Cil


let rev_equals =
    let module Memo = Memo.Make (struct
        type t = (Decision.t -> Decision.t -> bool) * DecisionPath.t * DecisionPath.t * int
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
                let b, suf1, suf2 = rev_equals (eq, DecisionPath.tl lst1, lst2, n - 1) in
                (b && eq (DecisionPath.hd lst1) (DecisionPath.hd suf2), suf1, (DecisionPath.tl suf2))
        end
    in
    fun eq lst1 lst2 n -> Profiler.global#call "BackOtterUtilities.rev_equals" begin fun () ->
        rev_equals (eq, lst1, lst2, n)
    end


let get_origin_function =
    let rec get_last_element = function
        | [ x ] -> x
        | x::xs -> get_last_element xs
        | [] -> assert false
    in
    fun job ->
        Profiler.global#call "get_origin_function" begin fun () ->
            get_last_element job#state.callstack
        end

