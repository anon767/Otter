open OcamlUtilities
open CilUtilities
open OtterBytes
open OtterCore
open Bytes
open Types
open Job
open Cil

let default_conditionals_forking_level = ref max_int

class ['self] target_tracker delegate entry_fn targets_ref =
object (_ : 'self)
    val delegate = delegate
    method report job_state =
        let original_job_state = job_state in

        (* convert executions that report repeated abandoned paths to Truncated *)
        let job_state = match job_state with
            | Job.Complete (Job.Abandoned (`FailureReached , location , job_result)) ->
                let fundec = BackOtterUtilities.get_origin_function_from_job_result job_result in
                (* Failing path has least recent decision first. See the comment in BidirectionalQueue. *)
                let failing_path = List.rev job_result.result_decision_path in
                begin try
                    targets_ref := BackOtterTargets.add fundec failing_path (!targets_ref);
                    job_state
                with Invalid_argument _ ->
                    Job.Complete (Job.Truncated (`SummaryAbandoned (`FailureReached, location), job_result))
                end
            | Job.Complete (Job.Abandoned (`Failure msg, location, job_result)) when !BackOtterReporter.arg_exceptions_as_failures ->
                let fundec = BackOtterUtilities.get_origin_function_from_job_result job_result in
                (* Failing path has least recent decision first. See the comment in BidirectionalQueue. *)
                let failing_path = List.rev job_result.result_decision_path in
                begin try
                    targets_ref := BackOtterTargets.add fundec failing_path (!targets_ref);
                    job_state
                with Invalid_argument _ ->
                    Job.Complete (Job.Truncated (`SummaryAbandoned (`Failure msg, location), job_result))
                end
            | _ ->
                job_state
        in
        (* convert executions from non-entry functions to Truncated *)
        let job_state = match job_state with
            | Job.Complete (Job.Return (return_code, job_result))
                    when BackOtterUtilities.get_origin_function_from_job_result job_result != entry_fn ->
                Job.Complete (Job.Truncated (`SummaryReturn return_code, job_result))
            | Job.Complete (Job.Exit (return_code, job_result))
                    when BackOtterUtilities.get_origin_function_from_job_result job_result != entry_fn ->
                Job.Complete (Job.Truncated (`SummaryExit return_code, job_result))
            | Job.Complete (Job.Abandoned (reason, location, job_result))
                    when BackOtterUtilities.get_origin_function_from_job_result job_result != entry_fn ->
                Job.Complete (Job.Truncated (`SummaryAbandoned (reason, location), job_result))
            | _ ->
                job_state
        in
        let delegate = delegate#report job_state in

        (* Print failing path. This is run after delegate#report so the failing path is printed after the failure message. *)
        let print_failing_path job_result =
            let fundec = BackOtterUtilities.get_origin_function_from_job_result job_result in
            let failing_path = List.rev job_result.result_decision_path in
            Output.debug_printf "@\n=> Extract the following failing path for function %s:@\n" fundec.svar.vname;
            Output.debug_printf "@[%a@]@\n@\n" Decision.print_decisions failing_path;
        in
        begin match original_job_state with
            | Job.Complete (Job.Abandoned (`FailureReached, _ , job_result)) ->
                Output.printf "target_tracker: FailureReached@\n";
                print_failing_path job_result
            | Job.Complete (Job.Abandoned (`Failure msg, _, job_result)) when !BackOtterReporter.arg_exceptions_as_failures ->
                Output.printf "target_tracker: Failure (%s)@\n" msg;
                print_failing_path job_result
            | _ -> ()
        end;
        {< delegate = delegate >}

    method should_continue = delegate#should_continue

    method completed = delegate#completed

    method delegate = delegate
end


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


(** Main symbolic execution loop. Copied from OtterCore.Driver. *)
let main_loop entry_fn timer_ref interceptor queue reporter =
    (* compose the interceptor with the core symbolic executor *)
    let step = fun job -> fst (interceptor job () Statement.step) in
    let rec run (queue, reporter) =
        try
            match queue#get with
            | Some (queue, job) ->
                let result =
                    (* The difference between timing here and timing in BidirectionalQueue is that
                     * here we only time the stepping of the job, whereas in BidirectionalQueue we
                     * also include the time of getting a job. *)
                    let result = Stats.timethis step job in
                    let time_elapsed = !Stats.lastTime in
                    let fundec = BackOtterUtilities.get_origin_function job in
                    let entry_time, other_time = !timer_ref in
                    timer_ref := (
                        if fundec == entry_fn then
                            (entry_time +. time_elapsed, other_time)
                        else
                            (entry_time, other_time +. time_elapsed));
                    result
                in
                let rec process_result (queue, reporter as work) result k =
                    let reporter = reporter#report result in
                    if reporter#should_continue then match result with
                        | Job.Active job ->
                            k (queue#put job, reporter)
                        | Job.Fork (result::results) ->
                            process_result work result (fun work -> process_result work (Job.Fork results) k)
                        | Job.Fork [] ->
                            k work
                        | Job.Complete completion ->
                            k (queue, reporter)
                        | Job.Paused _ ->
                            invalid_arg "unexpected Job.Paused"
                    else
                        reporter
                in
                process_result (queue, reporter) result run
            | None ->
                reporter
        with Types.SignalException s ->
            (* if we got a signal, stop and return the completed results *)
            Output.set_mode Output.MSG_MUSTPRINT;
            Output.printf "%s@\n" s;
            reporter
    in
    run (queue, reporter)


let callchain_backward_se ?(random_seed=(!Executeargs.arg_random_seed))
                          ?(targets_ref=ref BackOtterTargets.empty)
                          ?(f_queue=BackOtterQueue.get_default_fqueue targets_ref)
                          ?(b_queue=BackOtterQueue.get_default_bqueue targets_ref)
                          ?ratio reporter entry_job =

	Random.init random_seed;

    let file = entry_job.Job.file in

    (* Entry function set by --entryfn (default: main) *)
    let entry_fn = List.hd entry_job.state.callstack in

    (* Setup max_function_name_length, to be used in set_output_formatter_interceptor *)
    let all_reachable_functions = entry_fn :: (CilCallgraph.find_transitive_callees file entry_fn) in
    max_function_name_length := List.fold_left (fun len fundec ->
        max len (String.length fundec.svar.vname)) 0 all_reachable_functions;

    (* Failure function set by --failurefn (default: __FAILURE) *)
    let failure_fn =
      let fname = !Executeargs.arg_failurefn in
      try FindCil.fundec_by_name file fname
      with Not_found -> FormatPlus.failwith "Failure function %s not found" fname
    in

    (* Timer. Currently just a pair of times *)
    let timer_ref = ref (0.0, 0.0) in

    (* Add failure_fn as a target *)
    targets_ref := BackOtterTargets.add failure_fn [] (!targets_ref);

    (* A queue that prioritizes jobs *)
    let queue = new BidirectionalQueue.t ?ratio file targets_ref timer_ref entry_fn failure_fn entry_job f_queue b_queue in

    (* Overlay the target tracker on the reporter *)
    let target_tracker = new target_tracker reporter entry_fn targets_ref in

    (* Define interceptor *)
    let interceptor =
        let (>>>) = Interceptor.(>>>) in
            set_output_formatter_interceptor
        >>> BuiltinFunctions.libc_interceptor
        >>> BuiltinFunctions.interceptor
        >>> (
            let level = !default_conditionals_forking_level  in
            if level < max_int then
                OtterExtensions.ConditionalsForking.interceptor ~limit:level
            else
                Interceptor.identity_interceptor
        )
    in
    let target_tracker = main_loop entry_fn timer_ref interceptor queue target_tracker in

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

    let find_tag_name tag assocs = List.assoc tag (List.map (fun (a,b)->(b,a)) assocs) in
    Output.must_printf "Forward strategy: %s@\n" (find_tag_name (!BackOtterQueue.default_fqueue) BackOtterQueue.queues);
    Output.must_printf "Backward function pick: %s@\n" (find_tag_name (!BackwardRank.default_brank) BackwardRank.queues);
    Output.must_printf "Backward strategy: %s@\n" (find_tag_name (!BackOtterQueue.default_bqueue) BackOtterQueue.queues);
    Output.must_printf "Ratio: %0.2f@\n" !BidirectionalQueue.default_bidirectional_search_ratio ;

    let entry_job = OtterJob.Job.get_default file in
    let reporter = callchain_backward_se (new BackOtterReporter.t ()) entry_job in

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
    let nodes, paths, abandoned = reporter#get_stats in
    Output.printf "Number of nodes: %d@\n" nodes;
    Output.printf "Number of paths: %d@\n" paths;
    Output.printf "Number of abandoned: %d@\n" abandoned;
    List.iter (fun key ->
        Output.printf "%s: %.2f s@\n" key (BackOtterUtilities.lookupTime key)
    ) (BackOtterUtilities.keys ());
    ()


(** {1 Command-line options} *)

let options = [
    "--conditionals-forking-level",
        Arg.Set_int default_conditionals_forking_level,
        "<level> Set levels for conditionals forking (default: max_int (== don't use))";
]


let feature = {
    Cil.fd_name = "backotter";
    Cil.fd_enabled = ref false;
    Cil.fd_description = "Call-chain backwards symbolic executor for C";
    Cil.fd_extraopt = options @ BackOtterReporter.options @ BidirectionalQueue.options @ BackOtterQueue.options @ BackwardRank.options;
    Cil.fd_post_check = true;
    Cil.fd_doit = doit
}
