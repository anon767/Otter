open OcamlUtilities
open CilUtilities
open OtterBytes
open OtterCore
open Bytes
open State
open Job
open Cil

let default_conditionals_forking_limit = ref max_int

let arg_line_targets = ref []

class ['self] target_tracker delegate entry_fn targets_ref =
object (_ : 'self)
    val delegate = delegate
    method report job_state =
        let original_job_state = job_state in

        (* convert executions that report repeated abandoned paths to Truncated *)
        let job_state = match job_state with
            | Job.Complete (Job.Abandoned (`FailureReached, job_result)) ->
                let fundec = BackOtterUtilities.get_origin_function_from_job_result job_result in
                (* Failing path has least recent decision first. See the comment in BidirectionalQueue. *)
                let failing_path = List.rev job_result#decision_path in
                begin try
                    targets_ref := BackOtterTargets.add fundec failing_path (!targets_ref);
                    job_state
                with Invalid_argument _ ->
                    Job.Complete (Job.Truncated (`SummaryAbandoned (`FailureReached, Job.get_loc job_result), job_result))
                end
            | Job.Complete (Job.Abandoned (`Failure msg, job_result)) when !BackOtterReporter.arg_exceptions_as_failures ->
                let fundec = BackOtterUtilities.get_origin_function_from_job_result job_result in
                (* Failing path has least recent decision first. See the comment in BidirectionalQueue. *)
                let failing_path = List.rev job_result#decision_path in
                begin try
                    targets_ref := BackOtterTargets.add fundec failing_path (!targets_ref);
                    job_state
                with Invalid_argument _ ->
                    Job.Complete (Job.Truncated (`SummaryAbandoned (`Failure msg, Job.get_loc job_result), job_result))
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
            | Job.Complete (Job.Abandoned (reason, job_result))
                    when BackOtterUtilities.get_origin_function_from_job_result job_result != entry_fn ->
                Job.Complete (Job.Truncated (`SummaryAbandoned (reason, Job.get_loc job_result), job_result))
            | _ ->
                job_state
        in
        let delegate = delegate#report job_state in

        (* Print failing path. This is run after delegate#report so the failing path is printed after the failure message. *)
        let print_failing_path job_result =
            let fundec = BackOtterUtilities.get_origin_function_from_job_result job_result in
            let failing_path = List.rev job_result#decision_path in
            Output.debug_printf "@\n=> Extract the following failing path for function %s:@\n" fundec.svar.vname;
            Output.debug_printf "@[%a@]@\n@\n" Decision.print_decisions failing_path;
        in
        begin match original_job_state with
            | Job.Complete (Job.Abandoned (`FailureReached, job_result)) ->
                Output.printf "target_tracker: FailureReached@\n";
                print_failing_path job_result
            | Job.Complete (Job.Abandoned (`Failure msg, job_result)) when !BackOtterReporter.arg_exceptions_as_failures ->
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
    let origin_function_name = (List.hd (List.rev job#state.callstack)).svar.vname in
    let depth = List.length job#state.path_condition in
    let loc = Job.get_loc job in
    let label =
        if loc = Cil.locUnknown then
            Format.sprintf "%*s [%d,%d] : " (!max_function_name_length) origin_function_name job#path_id depth
        else
            Format.sprintf "%*s [%d,%d] %s:%d : " (!max_function_name_length) origin_function_name job#path_id depth (Filename.basename loc.Cil.file) loc.Cil.line
    in
    Output.set_formatter (new Output.labeled label);
    interceptor job job_queue


(** An interceptor that emits FailureReached when some (file, line) in arg_line_targets is encountered. *)
let line_target_interceptor job job_queue interceptor =
    let loc = Job.get_loc job in
    if List.mem (loc.Cil.file, loc.Cil.line) (!arg_line_targets) then
        Complete (Abandoned (`FailureReached, job)), job_queue
    else
        interceptor job job_queue


(** Main symbolic execution loop. Copied from OtterCore.Driver. *)
let main_loop entry_fn interceptor queue reporter =
    (* set up a checkpoint to rollback to upon SignalException *)
    let checkpoint = ref (queue, reporter) in
    try
        (* compose the interceptor with the core symbolic executor *)
        let step = fun job -> fst (interceptor job () Statement.step) in
        let rec run (queue, reporter) =
            checkpoint := (queue, reporter);
            match queue#get with
                | Some (queue, job) ->
                    let result =
                        (* The difference between timing here and timing in BidirectionalQueue is that
                         * here we only time the stepping of the job, whereas in BidirectionalQueue we
                         * also include the time of getting a job. *)
                        let fundec = BackOtterUtilities.get_origin_function job in
                        let tkind = if fundec == entry_fn then `TKindEntry else `TKindOther in
                        BackOtterTimer.time tkind (fun () -> step job)
                    in
                    let rec process_result (queue, reporter) result =
                        let reporter = reporter#report result in
                        match result with
                            | Job.Active job ->
                                (queue#put job, reporter)
                            | Job.Fork results ->
                                List.fold_left process_result (queue, reporter) results
                            | Job.Complete completion ->
                                (queue, reporter)
                            | Job.Paused _ ->
                                invalid_arg "unexpected Job.Paused"
                    in
                    let queue, reporter = process_result (queue, reporter) result in
                    if reporter#should_continue then
                        run (queue, reporter)
                    else
                        (queue, reporter)
                | None ->
                    (queue, reporter)
        in
        run (queue, reporter)
    with State.SignalException s ->
        (* if we got a signal, stop and return the checkpoint results *)
        Output.set_mode Output.MSG_MUSTPRINT;
        Output.printf "%s@\n" s;
        !checkpoint


let callchain_backward_se ?(random_seed=(!Executeargs.arg_random_seed))
                          ?(targets_ref=ref BackOtterTargets.empty)
                          ?(f_queue=BackOtterQueue.get_default_fqueue targets_ref)
                          ?(b_queue=BackOtterQueue.get_default_bqueue targets_ref)
                          ?ratio reporter entry_job =

	Random.init random_seed;
    BackOtterTimer.reset_time ();

    let file = entry_job#file in

    (* Entry function set by --entryfn (default: main) *)
    let entry_fn = List.hd entry_job#state.callstack in

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

    (* Add failure_fn as a target *)
    targets_ref := BackOtterTargets.add failure_fn [] (!targets_ref);

    (* Wrap queues with ContentQueue *)
    let f_queue = new ContentQueue.t f_queue in
    let b_queue = new ContentQueue.t b_queue in

    (* when arg_line_targets != [], add appropriate jobs in bqueue *)
    let starter_fundecs = List.fold_left (fun starter_fundecs (file_name, line_num) ->
        let fundec = CovToFundec.of_line (file_name, line_num) in
        if List.memq fundec starter_fundecs then starter_fundecs else fundec::starter_fundecs)
    [] (!arg_line_targets) in
    List.iter (fun f -> Output.debug_printf "Function containing coverage targets: %s@\n" f.svar.vname) starter_fundecs;
    let b_queue = List.fold_left (fun b_queue fundec ->
        let job = new OtterJob.FunctionJob.t file ~points_to:(!BidirectionalQueue.default_points_to file) fundec in
        b_queue#put job
    ) b_queue starter_fundecs in

    (* A queue that prioritizes jobs *)
    let queue = new BidirectionalQueue.t ?ratio file targets_ref entry_fn failure_fn entry_job f_queue b_queue starter_fundecs in

    (* Overlay the target tracker on the reporter *)
    let target_tracker = new target_tracker reporter entry_fn targets_ref in

    (* Define interceptor *)
    let interceptor =
        let (>>>) = Interceptor.(>>>) in
            set_output_formatter_interceptor
        >>> line_target_interceptor
        >>> BuiltinFunctions.interceptor
        >>> (
            let limit = !default_conditionals_forking_limit  in
            if limit < max_int then
                OtterExtensions.ConditionalsForking.interceptor ~limit:limit
            else
                Interceptor.identity_interceptor
        )
    in
    let queue, target_tracker = main_loop entry_fn interceptor queue target_tracker in

    (* Output failing paths for non-entry_fn *)
    List.iter (fun fundec ->
        if fundec != entry_fn then (
            Output.debug_printf "@\nFailing path(s) for %s:@\n" fundec.svar.vname;
            List.iter (fun decisions ->
                Output.debug_printf "Failing path: @[%a@]@\n" Decision.print_decisions decisions)
                (BackOtterTargets.get fundec (!targets_ref))
        )
    ) (BackOtterTargets.get_fundecs (!targets_ref));

    (* Output failing paths for entry_fn *)
    Output.must_printf "@\nFailing path(s) for %s:@\n" entry_fn.svar.vname;
    List.iter (fun decisions ->
        Output.must_printf "Failing path: @[%a@]@\n" Decision.print_decisions decisions)
        (BackOtterTargets.get entry_fn (!targets_ref));

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

    begin try

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
        Output.printf "%s@\n" s
    end;

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
    ("--line-targets",
        Arg.String begin fun str ->
            let args = Str.split (Str.regexp ",") str in
            let re = Str.regexp "\\(.*\\):\\(.*\\)" in
            List.iter (fun arg ->
                if Str.string_match re arg 0 then
                    let file = Str.matched_group 1 arg in
                    let line = int_of_string (Str.matched_group 2 arg) in
                    arg_line_targets := (file, line)::(!arg_line_targets)
                else
                    failwith "Error in parsing --line-targets"
            ) args
        end,
        "<line[,lines]> Lines in the form file:linenum[,file:linenum...]. Default is empty list.\n");

    "--conditionals-forking-limit",
        Arg.Set_int default_conditionals_forking_limit,
        "<limit> Set the limit in conditionals forking (default: max_int (== don't use))";
]


let feature = {
    Cil.fd_name = "backotter";
    Cil.fd_enabled = ref false;
    Cil.fd_description = "Call-chain backwards symbolic executor for C";
    Cil.fd_extraopt = options @ BackOtterReporter.options @ BackOtterTimer.options @ BidirectionalQueue.options @ BackOtterQueue.options @ FunctionRanker.options;
    Cil.fd_post_check = true;
    Cil.fd_doit = doit
}
