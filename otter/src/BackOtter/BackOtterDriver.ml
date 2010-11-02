open OcamlUtilities
open CilUtilities
open OtterBytes
open OtterCore
open OtterDriver
open Bytes
open Types
open Job
open Cil


class ['self] target_tracker delegate entry_fn targets_ref =
object (_ : 'self)
    val delegate = delegate
    method report job_state =
        (* convert executions from non-entry functions to Truncated *)
        let job_state' = match job_state with
            | Job.Complete (Job.Return (return_code, job_result))
                    when List.hd (List.rev job_result.result_state.callstack) != entry_fn ->
                Job.Complete (Job.Truncated (`SummaryReturn return_code, job_result))
            | Job.Complete (Job.Exit (return_code, job_result))
                    when List.hd (List.rev job_result.result_state.callstack) != entry_fn ->
                Job.Complete (Job.Truncated (`SummaryExit return_code, job_result))
            | Job.Complete (Job.Abandoned (reason, location, job_result))
                    when List.hd (List.rev job_result.result_state.callstack) != entry_fn ->
                Job.Complete (Job.Truncated (`SummaryAbandoned (reason, location), job_result))
            | _ ->
                job_state
        in
        let delegate' = delegate#report job_state' in
        (* Extract failing path from failure, and create target function.
         * This is run after the delegate so that the "Extract..." message is output after the failure message. *)
        let extract_failing_path job_result =
            let fundec = List.hd (List.rev job_result.result_state.callstack) in
            let failing_path = job_result.result_decision_path in
            let _ = Output.debug_printf "@\n=> Extract the following failing path for function %s:@\n" fundec.svar.vname in
            let _ = Output.debug_printf "@[%a@]@\n@\n" Decision.print_decisions failing_path in
            targets_ref := BackOtterTargets.add fundec failing_path (!targets_ref)
        in
        begin match job_state with
            | Job.Complete (Job.Abandoned (`FailureReached, _ , job_result)) -> extract_failing_path job_result
            | Job.Complete (Job.Abandoned (_, _, job_result)) when !BackOtterReporter.arg_exceptions_as_failures -> extract_failing_path job_result
            | _ -> ()
        end;
        {< delegate = delegate' >}

    method should_continue = delegate#should_continue

    method completed = delegate#completed

    method delegate = delegate
end


(* Cil feature for BackOtter *)
let arg_failurefn = ref "__FAILURE"

(* __FAILURE() *)
let otter_failure_interceptor job job_queue interceptor =
    match job.instrList with
        | Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc)::_
            when varinfo.Cil.vname = (!arg_failurefn) ->
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


let max_function_name_length = ref 0
let set_output_formatter_interceptor job job_queue interceptor =
    let origin_function_name = (List.hd (List.rev job.state.callstack)).svar.vname in
    let depth = List.length job.state.path_condition in
    let loc = Job.get_loc job in
    let label =
        if loc = Cil.locUnknown then
            Format.sprintf "%*s [%d,%d] : " (!max_function_name_length) origin_function_name job.jid depth
        else
            Format.sprintf "%*s [%d,%d] %s:%d : " (!max_function_name_length) origin_function_name job.jid depth (Filename.basename loc.Cil.file) loc.Cil.line
    in
    Output.set_formatter (new Output.labeled label);
    interceptor job job_queue


let callchain_backward_se reporter entry_job =
    let file = entry_job.Job.file in

    (* Entry function set by --entryfn (default: main) *)
    let entry_fn = List.hd entry_job.state.callstack in

    (* Setup max_function_name_length, to be used in set_output_formatter_interceptor *)
    let all_reachable_functions = entry_fn :: (CilCallgraph.find_transitive_callees file entry_fn) in
    max_function_name_length := List.fold_left (fun len fundec ->
        max len (String.length fundec.svar.vname)) 0 all_reachable_functions;

    (* Map fundecs to potential decision lists
     * This is a reference used by the queue and the two interceptors. *)
    let targets_ref = ref BackOtterTargets.empty in

    (* Failure function set by --failurefn (default: __FAILURE) *)
    let failure_fn =
      let fname = !arg_failurefn in
      try FindCil.fundec_by_name file fname
      with Not_found -> FormatPlus.failwith "Failure function %s not found" fname
    in

    (* A queue that prioritizes jobs *)
    let queue = new BackOtterQueue.t file targets_ref entry_fn failure_fn in

    (* Add entry_job into the queue *)
    let queue = queue#put entry_job in

    (* Overlay the target tracker on the reporter *)
    let target_tracker = new target_tracker reporter entry_fn targets_ref in

    (* Define interceptor *)
    let interceptor =
        let (>>>) = Interceptor.(>>>) in
            set_output_formatter_interceptor
        >>> otter_failure_interceptor
        >>> BuiltinFunctions.libc_interceptor
        >>> BuiltinFunctions.interceptor
    in
    let target_tracker = Driver.main_loop interceptor queue target_tracker in

    (* Output failing paths for entry_fn *)
    Output.must_printf "@\n@\n";
    List.iter (fun decisions ->
        Output.must_printf "Failing path: @[%a@]@\n" Decision.print_decisions decisions)
        (BackOtterTargets.get entry_fn (!targets_ref));
    target_tracker#delegate


let doit file =
    (* connect Cil's debug flag to Output *)
    Output.arg_print_debug := !Errormsg.debugFlag;

    Output.must_printf "@\n@\nBackOtter: Bi-directional Symbolic Executor@\n@\n";
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

    let entry_job = OtterJob.Job.get_default file in
    let results = callchain_backward_se (new BackOtterReporter.t ()) entry_job in

    (* Turn off the alarm and reset the signal handlers *)
    ignore (Unix.alarm 0);
    Sys.set_signal Sys.sigalrm old_ALRM_handler;
    Sys.set_signal Sys.sigint old_INT_handler;

    (* print the results *)
    Output.set_formatter (new Output.plain);
    Output.printf "%s@\n@\n" (Executedebug.get_log ());
    Output.printf "\nSTP was invoked %d times (%d cache hits).\n" !Stp.stp_count !Stp.cacheHits;

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
    Output.printf "Counter statistics:@\n";
    let counter_stats = DataStructures.NamedCounter.report () in
    List.iter (fun (name, value) -> Output.printf "%s : %d@\n" name value) counter_stats;
    if (!Stp.print_stp_queries) then (
        Output.must_printf "Stp queries: @\n";
        List.iter (fun (pc, pre, guard, truth_value, time) ->
            List.iter (Output.must_printf "PC: @[%a@]@\n" BytesPrinter.bytes) pc;
            Output.printf "PRE: @[%a@]@\n" BytesPrinter.guard pre;
            Output.printf "QUERY: @[%a@]@\n" BytesPrinter.guard guard;
            Output.printf "TRUTH: @[%s@]@\n" (if truth_value then "True" else "False");
            Output.printf "TIME: @[%.2f@]@\n" time;
            Output.printf "--------------------------------------------------@\n"
        ) (!Stp.stp_queries)
    );
    (* TODO: output results *)
    ignore results


let feature = {
    Cil.fd_name = "backotter";
    Cil.fd_enabled = ref false;
    Cil.fd_description = "Call-chain backwards symbolic executor for C";
    Cil.fd_extraopt = [
        ("--failurefn",
        Arg.Set_string arg_failurefn,
        "<fname> Failure function to look for in BackOtter (default: __FAILURE)");
    ] @ BackOtterReporter.options @ BackOtterQueue.options;
    Cil.fd_post_check = true;
    Cil.fd_doit = doit
}
