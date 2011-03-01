open OcamlUtilities
open CilUtilities
open OtterBytes
open OtterCore
open Bytes
open State
open Job
open Cil

let arg_ignore_targets_in_entryfn = ref false
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
    let entry_fn = ProgramPoints.get_entry_fundec file in

    (* failure function set by --failurefn (default: __FAILURE) *)
    let failure_fn = ProgramPoints.get_failure_fundec file in

    BackOtterTargets.add_path failure_fn [];

    (* Setup max_function_name_length, to be used in set_output_formatter_interceptor *)
    BackOtterInterceptor.set_max_function_name_length (entry_fn :: (CilCallgraph.find_transitive_callees file entry_fn));

    (* Wrap queues with ContentQueue *)
    let f_queue = new ContentQueue.t f_queue in
    let b_queue = new ContentQueue.t b_queue in

    (* when arg_line_targets != [], add appropriate jobs in bqueue *)
    (* TODO: let BidirectionalQueue decide which sub-queue to put into *)
    let starter_fundecs = List.fold_left (fun starter_fundecs (file_name, line_num) ->
        Output.must_printf "Line target: %s:%d in function " file_name line_num;
        let r = 
            try
                let fundec = CovToFundec.of_line (file_name, line_num) in
                Output.must_printf "%s " fundec.svar.vname;
                let fundec = BackOtterUtilities.get_transitive_unique_caller file fundec in
                if List.memq fundec starter_fundecs then
                    starter_fundecs 
                else if !arg_ignore_targets_in_entryfn && CilData.CilFundec.equal fundec entry_fn then 
                    (Output.must_printf "(ignored)"; starter_fundecs)
                else fundec::starter_fundecs
            with Not_found -> 
                Output.must_printf "(missing) ";
                starter_fundecs (* There're lines that KLEE counts as instructions but Otter doesn't. *)
        in
        Output.must_printf "@\n";
        r
    ) [] (!LineTargets.arg_line_targets) in
    List.iter (fun f -> Output.debug_printf "Function containing coverage targets: %s@." f.svar.vname) starter_fundecs;
    let b_queue = List.fold_left (fun b_queue fundec ->
        if CilData.CilFundec.equal fundec entry_fn then 
            if BidirectionalQueue.is_bidirectional_search ratio then b_queue
            else b_queue#put entry_job
        else
            b_queue#put (BackOtterJob.get_function_job file fundec)
    ) b_queue starter_fundecs in

    (* A queue that prioritizes jobs *)
    let queue = new BidirectionalQueue.t ?ratio file entry_job f_queue b_queue starter_fundecs in

    (* Overlay the target tracker on the reporter *)
    let target_tracker = new BackOtterTargetTracker.t reporter entry_fn in

    (* Define interceptor *)
    let interceptor =
        let (>>>) = Interceptor.(>>>) in
            BackOtterInterceptor.set_output_formatter_interceptor
        >>> LineTargets.line_target_interceptor
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
            Output.debug_printf "@\nFailing path(s) for %s:@." fundec.svar.vname;
            List.iter (fun decisions ->
                Output.debug_printf "Failing path: @[%a@]@." Decision.print_decisions decisions)
                (BackOtterTargets.get_paths fundec)
        )
    ) (BackOtterTargets.get_target_fundecs ());

    (* Output failing paths for entry_fn *)
    Output.must_printf "@\nFailing path(s) for %s:@." entry_fn.svar.vname;
    List.iter (fun decisions ->
        Output.must_printf "Failing path: @[%a@]@." Decision.print_decisions decisions)
        (BackOtterTargets.get_paths entry_fn);

    (queue, target_tracker#delegate)


let doit file =
    (* connect Cil's debug flag to Output *)
    Output.arg_print_debug := !Errormsg.debugFlag;

    Output.must_printf "@\n@\nBackOtter: Bi-directional Symbolic Executor@\n@.";

    UserSignal.using_signals begin fun () -> try
        Core.prepare_file file;
        CovToFundec.prepare_file file;

        let find_tag_name tag assocs = List.assoc tag (List.map (fun (a,b)->(b,a)) assocs) in
        Output.must_printf "Forward strategy: %s@." (find_tag_name (!BackOtterQueue.default_fqueue) BackOtterQueue.queues);
        Output.must_printf "Backward function pick: %s@." (find_tag_name (!FunctionRanker.default_brank) FunctionRanker.queues);
        Output.must_printf "Backward strategy: %s@." (find_tag_name (!BackOtterQueue.default_bqueue) BackOtterQueue.queues);
        Output.must_printf "Ratio: %0.2f@." !BidirectionalQueue.default_bidirectional_search_ratio ;

        let entry_job = BackOtterJob.get_default file in
        let _, reporter = callchain_backward_se (new BackOtterReporter.t ()) entry_job in

        (* print the results *)
        Output.set_formatter (new Output.plain);
        Output.printf "@\nSTP was invoked %d times (%d cache hits).@." !Stp.stp_count !Stp.cacheHits;

        Output.printf "Counter statistics:@.";
        let counter_stats = DataStructures.NamedCounter.report () in
        List.iter (fun (name, value) -> Output.printf "%s : %d@." name value) counter_stats;
        if (!Stp.print_stp_queries) then (
            Output.must_printf "Stp queries:@.";
            List.iter (fun (pc, pre, guard, truth_value, time) ->
                List.iter (Output.must_printf "PC: @[%a@]@." BytesPrinter.bytes) pc;
                Output.printf "PRE: @[%a@]@." BytesPrinter.guard pre;
                Output.printf "QUERY: @[%a@]@." BytesPrinter.guard guard;
                Output.printf "TRUTH: %s@." (if truth_value then "True" else "False");
                Output.printf "TIME: %.2f@." time;
                Output.printf "--------------------------------------------------@."
            ) (!Stp.stp_queries)
        );
        let nodes, paths, abandoned = reporter#get_stats in
        Output.printf "Number of nodes: %d@." nodes;
        Output.printf "Number of paths: %d@." paths;
        Output.printf "Number of abandoned: %d@." abandoned;

        Report.print_report reporter#completed

    with UserSignal.UserInterrupt | UserSignal.TimedOut as exn ->
        (* TODO: move this into callchain_backwards_se *)
        Output.set_mode Output.MSG_MUSTPRINT;
        Output.printf "%s@." (Printexc.to_string exn)
    end;

    (* TODO: provide a way to force full-width profile printing *)
    Format.set_margin 120;
    Format.printf "Global profile:@\n@\n  @[%t@]@." Profiler.global#printer;
    Format.printf "@[%t@]@." Memo.statistics_printer;

    Format.printf "Done.@."


(** {1 Command-line options} *)

let options = [
    "--conditionals-forking-limit",
        Arg.Set_int default_conditionals_forking_limit,
        "<limit> Set the limit in conditionals forking (default: max_int (== don't use))";
	"--ignore-targets-in-entryfn",
		Arg.Set arg_ignore_targets_in_entryfn,
		" If set, BackOtter will ignore targets in the entry function.";
] @
    BackOtterReporter.options @ 
    BackOtterTimer.options @ 
    BackOtterQueue.options @ 
    BackOtterJob.options @ 
    BidirectionalQueue.options @ 
    FunctionRanker.options @
    LineTargets.options


let feature = {
    Cil.fd_name = "backotter";
    Cil.fd_enabled = ref false;
    Cil.fd_description = "Call-chain backwards symbolic executor for C";
    Cil.fd_extraopt = options;
    Cil.fd_post_check = true;
    Cil.fd_doit = doit
}

