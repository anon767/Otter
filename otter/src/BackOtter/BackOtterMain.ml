open OcamlUtilities
open CilUtilities
open OtterBytes
open OtterCore
open Bytes
open State
open Job
open Cil

let default_conditionals_forking_limit = ref max_int

let callchain_backward_se ?(random_seed=(!Executeargs.arg_random_seed))
                          ?(f_queue=BackOtterQueue.get_default_fqueue ())
                          ?(b_queue=BackOtterQueue.get_default_bqueue ())
                          ?ratio reporter entry_job =

	Random.init random_seed;
    BackOtterTimer.reset_time ();
    BackOtterTargets.reset_targets ();

    let file = entry_job#file in

    (* Entry function set by --entryfn (default: main) *)
    let entry_fn = List.hd entry_job#state.callstack in

    (* Setup max_function_name_length, to be used in set_output_formatter_interceptor *)
    BackOtterInterceptor.set_max_function_name_length (entry_fn :: (CilCallgraph.find_transitive_callees file entry_fn));

    (* Failure function set by --failurefn (default: __FAILURE) *)
    let failure_fn =
      let fname = !Executeargs.arg_failurefn in
      try FindCil.fundec_by_name file fname
      with Not_found -> FormatPlus.failwith "Failure function %s not found" fname
    in

    (* Add failure_fn as a target *)
    BackOtterTargets.add_path failure_fn [];

    (* Wrap queues with ContentQueue *)
    let f_queue = new ContentQueue.t f_queue in
    let b_queue = new ContentQueue.t b_queue in

    (* when arg_line_targets != [], add appropriate jobs in bqueue *)
    (* TODO: let BidirectionalQueue decide which sub-queue to put into *)
    let starter_fundecs = List.fold_left (fun starter_fundecs (file_name, line_num) ->
        let fundec = CovToFundec.of_line (file_name, line_num) in
        if List.memq fundec starter_fundecs then starter_fundecs else fundec::starter_fundecs)
    [] (!BackOtterInterceptor.arg_line_targets) in
    List.iter (fun f -> Output.debug_printf "Function containing coverage targets: %s@\n" f.svar.vname) starter_fundecs;
    let b_queue = List.fold_left (fun b_queue fundec ->
        let job = new OtterJob.FunctionJob.t file ~points_to:(!BidirectionalQueue.default_points_to file) fundec in
        b_queue#put job
    ) b_queue starter_fundecs in

    (* A queue that prioritizes jobs *)
    let queue = new BidirectionalQueue.t ?ratio file entry_fn failure_fn entry_job f_queue b_queue starter_fundecs in

    (* Overlay the target tracker on the reporter *)
    let target_tracker = new BackOtterTargetTracker.t reporter entry_fn in

    (* Define interceptor *)
    let interceptor =
        let (>>>) = Interceptor.(>>>) in
            BackOtterInterceptor.set_output_formatter_interceptor
        >>> BackOtterInterceptor.line_target_interceptor
        >>> Interceptor.function_pointer_interceptor
        >>> BuiltinFunctions.interceptor
        >>> (
            let limit = !default_conditionals_forking_limit  in
            if limit < max_int then
                OtterExtensions.ConditionalsForking.interceptor ~limit:limit
            else
                Interceptor.identity_interceptor
        )
    in
    let queue, target_tracker = BackOtterDriver.main_loop entry_fn interceptor queue target_tracker in

    (* Output failing paths for non-entry_fn *)
    List.iter (fun fundec ->
        if fundec != entry_fn then (
            Output.debug_printf "@\nFailing path(s) for %s:@\n" fundec.svar.vname;
            List.iter (fun decisions ->
                Output.debug_printf "Failing path: @[%a@]@\n" Decision.print_decisions decisions)
                (BackOtterTargets.get_paths fundec)
        )
    ) (BackOtterTargets.get_target_fundecs ());

    (* Output failing paths for entry_fn *)
    Output.must_printf "@\nFailing path(s) for %s:@\n" entry_fn.svar.vname;
    List.iter (fun decisions ->
        Output.must_printf "Failing path: @[%a@]@\n" Decision.print_decisions decisions)
        (BackOtterTargets.get_paths entry_fn);

    (queue, target_tracker#delegate)


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

    (try

        Core.prepare_file file;
        CovToFundec.prepare_file file;

        let find_tag_name tag assocs = List.assoc tag (List.map (fun (a,b)->(b,a)) assocs) in
        Output.must_printf "Forward strategy: %s@\n" (find_tag_name (!BackOtterQueue.default_fqueue) BackOtterQueue.queues);
        Output.must_printf "Backward function pick: %s@\n" (find_tag_name (!FunctionRanker.default_brank) FunctionRanker.queues);
        Output.must_printf "Backward strategy: %s@\n" (find_tag_name (!BackOtterQueue.default_bqueue) BackOtterQueue.queues);
        Output.must_printf "Ratio: %0.2f@\n" !BidirectionalQueue.default_bidirectional_search_ratio ;

        let entry_job = OtterJob.Job.get_default file in
        let _, reporter = callchain_backward_se (new BackOtterReporter.t ()) entry_job in

        (* print the results *)
        Output.set_formatter (new Output.plain);
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
        Output.myflush ();

        Report.print_report reporter#completed

    with State.SignalException s ->
        Output.set_mode Output.MSG_MUSTPRINT;
        Output.printf "%s@\n" s);

    (* Turn off the alarm and reset the signal handlers *)
    ignore (Unix.alarm 0);
    Sys.set_signal Sys.sigalrm old_ALRM_handler;
    Sys.set_signal Sys.sigint old_INT_handler;

    (** TODO: provide a way to force full-width profile printing *)
    Format.set_margin 120;
    Format.printf "Global profile:@\n@\n  @[%t@]@." Profiler.global#printer;

    Format.printf "Done.@\n"


(** {1 Command-line options} *)

let options = [
    "--conditionals-forking-limit",
        Arg.Set_int default_conditionals_forking_limit,
        "<limit> Set the limit in conditionals forking (default: max_int (== don't use))";
] @
    Otter.Executemain.options @
    BackOtterInterceptor.options @ 
    BackOtterReporter.options @ 
    BackOtterTimer.options @ 
    BackOtterQueue.options @ 
    BidirectionalQueue.options @ 
    FunctionRanker.options


let feature = {
    Cil.fd_name = "backotter";
    Cil.fd_enabled = ref false;
    Cil.fd_description = "Call-chain backwards symbolic executor for C";
    Cil.fd_extraopt = options;
    Cil.fd_post_check = true;
    Cil.fd_doit = doit
}

