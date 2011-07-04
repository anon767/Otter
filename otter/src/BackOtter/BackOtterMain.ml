open OcamlUtilities
open CilUtilities
open OtterBytes
open OtterCore
open OtterCFG
open Bytes
open State
open Job
open Cil

let default_conditionals_forking_limit = ref max_int

let callchain_backward_se ?(random_seed=(!Executeargs.arg_random_seed))
                          reporter file =

    Random.init random_seed;
    BackOtterTimer.reset_time ();
    BackOtterTargets.reset_targets ();

    let entry_job = BackOtterJob.get_default file in

    (* Entry function set by --entryfn (default: main) *)
    let entry_fn = ProgramPoints.get_entry_fundec file in

    begin try
        (* failure function set by --failurefn (default: __FAILURE) *)
        let failure_fn = ProgramPoints.get_failure_fundec file in

        (* call to __FAILURE() must be a line target *)
        List.iter (BackOtterTargetTracker.add_line_target file) (Instruction.call_sites (Instruction.of_fundec file failure_fn))
    with Failure _ -> 
        Output.debug_printf "Failure function not found"
    end;

    (* A queue that prioritizes jobs *)
    let queue = 
        if (!BidirectionalQueue.default_bidirectional_search_ratio) >= 1.0 then 
            (* Degenerates to pure forward Otter *)
            BackOtterQueue.get_default_fqueue ()
        else
            (* when get_line_targets != [], add appropriate jobs in bqueue *)
            let module FundecSet = Set.Make(CilData.CilFundec) in
            let starter_fundecs = List.fold_left (fun starter_fundecs instruction ->
                Output.debug_printf "Line target: %a in function " Instruction.printer instruction;
                try
                    let fundec = instruction.Instruction.fundec in
                    Output.debug_printf "%s " fundec.svar.vname;
                    let transitive_callers = CilCallgraph.find_transitive_callers file fundec in
                    List.fold_left (fun starter_fundecs fundec -> FundecSet.add fundec starter_fundecs) starter_fundecs (fundec :: transitive_callers)
                with Not_found ->
                    Output.debug_printf "(missing) ";
                    starter_fundecs (* There're lines that KLEE counts as instructions but Otter doesn't. *)
            ) FundecSet.empty (BackOtterTargetTracker.get_line_targets file) in
            let starter_fundecs = FundecSet.elements starter_fundecs in

            List.iter (fun f -> Output.debug_printf "Transitive callers of targets: %s@." f.svar.vname) starter_fundecs;

            let queue = new BidirectionalQueue.t file in

            (* Add non-entryfn jobs *)
            let queue = List.fold_left (fun queue fundec ->
                if CilData.CilFundec.equal fundec entry_fn then
                    queue (* bypass the entry_fn; it's added into entryfn_queue by below *)
                else
                    queue#put (BackOtterJob.get_function_job file fundec)
            ) queue starter_fundecs in
            queue
    in

    (* Add the entryfn job *)
    let queue = queue#put entry_job in

    (* Define jnterceptor *)
    let interceptor =
        let (>>>) = Interceptor.(>>>) in
            BackOtterInterceptor.set_output_formatter_interceptor
        >>> OtterExtensions.Gcov.interceptor
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
    let step job =
        DataStructures.NamedCounter.incr "step";
        (* The difference between timing here and timing in BidirectionalQueue is that
         * here we only time the stepping of the job, whereas in BidirectionalQueue we
         * also include the time of getting a job. *)
        let fundec = BackOtterUtilities.get_origin_function job in
        let tkind = if fundec == entry_fn then `TKindEntry else `TKindOther in
        BackOtterTimer.time tkind begin fun () ->
            (job : _ #Info.t)#try_run
                (fun job -> interceptor job Statement.step)
                ~catch_finish:(BackOtterTargetTracker.process_completed entry_fn)
        end
    in
    let queue, reporter = OtterDriver.Driver.main_loop step queue reporter in

    (* Flush gcovfiles *)
    OtterExtensions.Gcov.flush_gcovfiles ();

    (* Output failing paths for non-entry_fn *)
    List.iter (fun fundec ->
        if fundec != entry_fn then (
            Output.debug_printf "@\nFailing path(s) for %s:@." fundec.svar.vname;
            List.iter (fun decisions ->
                Output.debug_printf "Failing path: @[%a@]@." DecisionPath.print decisions)
                (BackOtterTargets.get_paths fundec)
        )
    ) (BackOtterTargets.get_target_fundecs ());

    (* Output failing paths for entry_fn *)
    Output.set_mode Output.MSG_REPORT;
    Output.printf "@\nFailing path(s) for %s:@." entry_fn.svar.vname;
    List.iter (fun decisions ->
        Output.printf "Failing path: @[%a@]@." DecisionPath.print decisions)
        (BackOtterTargets.get_paths entry_fn);

    OtterDriver.Driver.flush_queue queue reporter


let doit file =
    (* connect Cil's debug flag to Output *)
    Output.arg_print_debug := !Errormsg.debugFlag;

    Output.printf "@\n@\nBackOtter: Bi-directional Symbolic Executor@\n@.";

    UserSignal.using_signals begin fun () -> try
        Core.prepare_file file;

        let find_tag_name tag assocs = List.assoc tag (List.map (fun (a,b)->(b,a)) assocs) in
        Output.printf "Forward strategy: %s@." (find_tag_name (!BackOtterQueue.default_fqueue) BackOtterQueue.queues);
        Output.printf "Backward function ranking: %s@." (find_tag_name (!FunctionRanker.default_function_rank) FunctionRanker.queues);
        Output.printf "Backward strategy: %s@." (find_tag_name (!BackOtterQueue.default_bqueue) BackOtterQueue.queues);
        Output.printf "Ratio: %0.2f@." !BidirectionalQueue.default_bidirectional_search_ratio ;

        let queue, reporter = callchain_backward_se (new BackOtterReporter.t ()) file in

        (* print the results *)
        Output.set_formatter (new Output.plain);

        Output.printf "Counter statistics:@.";
        let counter_stats = DataStructures.NamedCounter.report () in
        List.iter (fun (name, value) -> Output.printf "%s : %d@." name value) counter_stats;
        let steps, paths, abandoned = reporter#get_stats in
        Output.printf "Number of steps: %d@." steps;
        Output.printf "Number of paths: %d@." paths;
        Output.printf "Number of abandoned: %d@." abandoned;

        Report.print_report reporter#completed

    with UserSignal.UserInterrupt | UserSignal.TimedOut as exn ->
        (* TODO: move this into callchain_backwards_se *)
        Output.set_mode Output.MSG_REPORT;
        Output.printf "%s@." (Printexc.to_string exn)
    end;

    (* TODO: provide a way to force full-width profile printing *)
    Format.set_margin 120;
    if !Profiler.do_profiling then Format.printf "Global profile:@\n@\n  @[%t@]@." Profiler.global#printer;
    Format.printf "@[%t@]@." Memo.statistics_printer;

    Format.printf "Done.@."


(** {1 Command-line options} *)

let options = [
    "--conditionals-forking-limit",
        Arg.Set_int default_conditionals_forking_limit,
        "<limit> Set the limit in conditionals forking (default: max_int (== don't use))";
] @
    BackOtterTimer.options @ 
    BackOtterQueue.options @ 
    BackOtterJob.options @ 
    BackOtterJobExtension.options @ 
    BackOtterTargetTracker.options @ 
    BackOtterUtilities.options @ 
    BidirectionalQueue.options @ 
    FunctionRanker.options @
    OtterExtensions.Gcov.options 


let feature = {
    Cil.fd_name = "backotter";
    Cil.fd_enabled = ref false;
    Cil.fd_description = "Call-chain backwards symbolic executor for C";
    Cil.fd_extraopt = options;
    Cil.fd_post_check = true;
    Cil.fd_doit = doit
}

