open DataStructures
open OcamlUtilities
open CilUtilities
open OtterBytes
open OtterCore
open OtterQueue
open OtterReporter
open OtterDriver
open OtterGraph
open Graph
open Bytes
open Types
open Job
open Cil

class ['reason] reporter ?max_nodes ?max_paths ?max_abandoned targets_ref =
object
    inherit ['reason] BasicReporter.t ?max_nodes ?max_paths ?max_abandoned () as super
    val targets_ref = targets_ref
    method report completion =
        begin match completion with
            (* TODO: also include `Failure when --exceptions-as-failures is enabled *)
            | Complete (Abandoned (`FailureReached, _ , job_result)) ->
                let fundec = List.hd (List.rev job_result.result_state.callstack) in
                let failing_path = job_result.result_decision_path in
                targets_ref := BackOtterTargets.add fundec failing_path (!targets_ref)
            | _ -> ()
        end;
        super#report completion
end

(* Cil feature for call-chain backwards Otter *)
let arg_assertfn = ref "__FAILURE"

(* __FAILURE() *)
let otter_failure_interceptor job job_queue interceptor =
    match job.instrList with
        | Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc)::_
            when varinfo.Cil.vname = (!arg_assertfn) ->
            let job_result = {
                result_file = job.Job.file;
                result_state = job.Job.state;
                result_history = job.Job.exHist;
                result_decision_path = job.Job.decisionPath;
            } in
            let loc = Job.get_loc job in
            let job_state = Complete (Abandoned (`FailureReached, loc, job_result)) in
            job_state, job_queue
        | _ ->
            interceptor job job_queue


(* If it's a call to a target, insert bounding paths
 * If bounding_paths already exist, bypass *)
let bounding_path_insertion_interceptor targets_ref job job_queue interceptor =
    if job.boundingPaths != None then
        interceptor job job_queue
    else
        match job.decisionPath with
        | DecisionFuncall(_, fundec) :: decisions ->
            let targets = !targets_ref in
            if BackOtterTargets.mem fundec targets then
                let failing_paths = BackOtterTargets.find fundec targets in
                let bounding_paths = List.map (fun path -> List.append path job.decisionPath) failing_paths in
                let job = {job with boundingPaths = Some bounding_paths} in
                interceptor job job_queue
            else
                interceptor job job_queue
        | _ -> interceptor job job_queue

(* Eliminate inconsistent bounding paths *)
let bounding_path_elimination_interceptor targets_ref job job_queue interceptor =
    let suffix decision_path bounding_path =
        (* Return 0 if pre==lst, 1 if pre is strictly prefix, -1 if pre is not a prefix *)
        let rec prefix eq pre lst =
            match pre, lst with
            | d1::pre', d2::lst' -> if eq d1 d2 then prefix eq pre' lst' else -1
            | [], [] -> 0
            | [], _ -> 1
            | _, _ -> -1
        in
        prefix decision_equals (List.rev decision_path) (List.rev bounding_path)
    in
    match job.boundingPaths with
    | Some paths ->
        let agreed_bounding_paths = List.filter (fun path -> suffix job.decisionPath path >= 0) paths in
        (* TODO: catch the oncoming failure? *)
        let job = {job with boundingPaths = Some agreed_bounding_paths} in
        interceptor job job_queue
    | None ->
        interceptor job job_queue

let callchain_backward_se file =
    (* Entry function set by --entryfn (default: main) *)
    let entry_fn = FindCil.fundec_by_name file (!OtterJob.Job.entry_function) in

    (* Map fundecs to potential decision lists
     * This is a reference used by the queue and the two interceptors. *)
    let targets_ref = ref BackOtterTargets.empty in

    (* A queue that prioritizes jobs *)
    let queue = new BackOtterQueue.t targets_ref entry_fn in

    (* Create one job for each function containing __FAILURE, and for main.
     * Job for main is put below *)
    let jobs =
        let assertfn =
          let fname = !arg_assertfn in
          try FindCil.fundec_by_name file fname
          with Not_found -> FormatPlus.failwith "Assertion function %s not found" fname
        in
        List.map (fun fundec -> OtterJob.FunctionJob.make file fundec)
            (CilCallgraph.find_callers file assertfn)
    in
    (* Add these jobs into the queue *)
    let queue = List.fold_left (fun queue job -> queue#put job) queue jobs in

    let reporter = new reporter targets_ref in

    (* Define interceptor *)
    let interceptor =
        let (>>>) = Interceptor.(>>>) in
        Interceptor.set_output_formatter_interceptor
        >>> (bounding_path_insertion_interceptor targets_ref)
        >>> (bounding_path_elimination_interceptor targets_ref)
        >>> otter_failure_interceptor
        >>> BuiltinFunctions.libc_interceptor
        >>> BuiltinFunctions.interceptor
    in
    let reporter = Driver.main_loop ~interceptor ~queue reporter (OtterJob.Job.get_default file) in
    reporter


let doit file =

    Format.printf "@\n@\nCall-chain backward Symbolic Execution@\n@\n";
    let startTime = Unix.gettimeofday () in
    (* Set signal handlers to catch timeouts and interrupts *)
    let old_ALRM_handler =
        Sys.signal Sys.sigalrm
            (Sys.Signal_handle (fun _ -> raise (SignalException "Timed out!")))
    and old_INT_handler =
        Sys.signal Sys.sigint
            (Sys.Signal_handle (fun _ -> raise (SignalException "User interrupt!")))
    in
    (* Set a timer *)
    ignore (Unix.alarm !Executeargs.arg_timeout);

    Executeargs.arg_cfg_pruning := true;
    Core.prepare_file file;

    let results = callchain_backward_se file in

    (* Turn off the alarm and reset the signal handlers *)
    ignore (Unix.alarm 0);
    Sys.set_signal Sys.sigalrm old_ALRM_handler;
    Sys.set_signal Sys.sigint old_INT_handler;

    (* print the results *)
    Output.set_formatter (new Output.plain);
    Output.printf "%s@\n@\n" (Executedebug.get_log ());
    Output.printf "\nSTP was invoked %d times. (%d cache hits; %d misses)\n" !Stp.stp_count !Stp.cacheHits !Stp.cacheMisses;

    let executionTime = (Unix.gettimeofday ()) -. startTime
    and stpTime = Stats.lookupTime "STP" in
    Output.printf "It ran for %.2f s, which is %.2f%% of the total %.2f s execution.\n"
        stpTime (100. *. stpTime /. executionTime) executionTime;
    Output.printf "  It took %.2f s to construct the formulas for the expressions inside 'if(...)'s,
        %.2f s to construct and %.2f s to assert the path conditions,
        and %.2f s to solve the resulting formulas.\n\n"
        (Stats.lookupTime "convert conditional")
        (Stats.lookupTime "STP construct")
        (Stats.lookupTime "STP doassert")
        (Stats.lookupTime "STP query");
    (if !Executeargs.arg_simplify_path_condition then
        Output.printf "It took %.2f s to simplify path conditions.\n"
           (Stats.lookupTime "Simplify PC")
    else ());

    Output.printf "Hash-consing: hits=%d misses=%d\n" (!Bytes.hash_consing_bytes_hits) (!Bytes.hash_consing_bytes_misses);
    Output.printf "Bytes eval caching: hits=%d misses=%d\n\n" (!MemOp.bytes_eval_cache_hits) (!MemOp.bytes_eval_cache_misses);
    if (!Stp.print_stp_queries) then (
        Format.printf "Stp queries: @\n";
        List.iter (fun (pc, pre, guard, truth_value, time) ->
            List.iter (Format.printf "PC: @[%a@]@\n" BytesPrinter.bytes) pc;
            Format.printf "PRE: @[%a@]@\n" BytesPrinter.guard pre;
            Format.printf "QUERY: @[%a@]@\n" BytesPrinter.guard guard;
            Format.printf "TRUTH: @[%s@]@\n" (if truth_value then "True" else "False");
            Format.printf "TIME: @[%.2f@]@\n" time;
            Format.printf "--------------------------------------------------@\n"
        ) (!Stp.stp_queries)
    );
    (* TODO: output results *)
    ignore results


let feature = {
    Cil.fd_name = "backotter";
    Cil.fd_enabled = ref false;
    Cil.fd_description = "Call-chain backwards symbolic executor for C";
    Cil.fd_extraopt = [
        ("--assertfn",
        Arg.Set_string arg_assertfn,
        "<fname> Assertion function to look for in the call-chain-backward mode (default: __FAILURE)");
    ];
    Cil.fd_post_check = true;
    Cil.fd_doit = doit
}
