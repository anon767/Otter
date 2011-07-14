open OcamlUtilities
open CilUtilities
open Cil

module FundecSet = Set.Make(CilUtilities.CilData.CilFundec) 
module StringSet = Set.Make(struct type t = string let compare = Pervasives.compare end)

let arg_function_inlining = ref false
let bypassed_fname = ref StringSet.empty
let madeready_fname = ref StringSet.empty

let is_inlinable fundec = (!arg_function_inlining) && match fundec.smaxstmtid with Some size -> size < 5 | None -> false  (* TODO: true if caller does not branch *)
let is_bypassed fundec = StringSet.mem fundec.svar.vname (!bypassed_fname)
let is_madeready fundec = StringSet.mem fundec.svar.vname (!madeready_fname)

let add_madeready fundec = madeready_fname := StringSet.add fundec.svar.vname (!madeready_fname)

(** Return callers of callee, considering function inlining and bypassing *)
let find_callers =
    let module Memo = Memo.Make (Module.CombineHashedTypes (CilData.CilFile) (CilData.CilFundec)) in
    let find_callers = Memo.memo "FunctionManager.find_callers"
        begin fun (file, callee) ->
            let rec find_callers file callee =
                let callers = CilUtilities.CilCallgraph.find_callers file callee in
                List.fold_left (
                    fun s f ->
                        if is_inlinable f || is_bypassed f then FundecSet.union (find_callers file f) s
                        else FundecSet.add f s
                ) FundecSet.empty callers
            in
            find_callers file callee
        end
    in
    fun file callee -> find_callers (file, callee)

let is_ready_to_run =
    let module Hashtbl = Hashtbl.Make (Module.CombineHashedTypes (CilData.CilFile) (CilData.CilFundec)) in
    let hashtbl = Hashtbl.create 8 in
    fun file fundec -> Profiler.global#call "FunctionManager.is_ready_to_run" begin fun () ->
        (* FIXME: This hashing will be wrong if line targets can be deleted *)
        if Hashtbl.mem hashtbl (file, fundec) then true else
        let is_ready_to_run =
            (* Functions containing paths reaching line targets *) 
            let target_fundecs = BackOtterTargets.get_target_fundecs () in
            if List.memq fundec target_fundecs then true else
                (* Functions containing line targets *)
                let line_target_fundecs =
                    let line_targets = BackOtterTargetTracker.get_line_targets file in
                    List.map (fun line_target -> line_target.OtterCFG.Instruction.fundec) line_targets
                in
                if List.memq fundec line_target_fundecs then true else
                    (* Callers of target functions, defined by find_callers *)
                    let callers = List.fold_left (fun s f -> FundecSet.union s (find_callers file f)) FundecSet.empty (target_fundecs @ line_target_fundecs) in
                    if FundecSet.mem fundec callers then true else
                        (* Some functions made ready by the user *)
                        if is_madeready fundec then true else false
        in
        (if is_ready_to_run then Hashtbl.add hashtbl (file, fundec) true);
        is_ready_to_run
    end

let options = [
    "--function-inlining",
        Arg.Set arg_function_inlining,
        " Enable function inlining in BackOtter";
    "--bypassed-functions",
        Arg.String begin fun str ->
            let args = Str.split (Str.regexp ",") str in
            List.iter (fun arg -> bypassed_fname := StringSet.add arg (!bypassed_fname)) args
        end,
        "<fname[,fname]> Functions which BackOtter bypasses (run their callers directly). Default is empty list.";
    "--starter-functions",
        Arg.String begin fun str ->
            let args = Str.split (Str.regexp ",") str in
            List.iter (fun arg -> madeready_fname := StringSet.add arg (!madeready_fname)) args
        end,
        "<fname[,fname]> Functions regarded as ready to run in BackOtter. Default is empty list.";
]
