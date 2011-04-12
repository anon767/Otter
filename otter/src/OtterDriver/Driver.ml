open OcamlUtilities
open CilUtilities
open OtterCore
open OtterQueue
open OtterReporter

(**/**)
let (>>>) = Interceptor.(>>>)
(**/**)


(** Main symbolic execution loop. *)
let main_loop interceptor queue reporter =
    (* set up a checkpoint to rollback to upon SignalException *)
    let checkpoint = ref (queue, reporter) in
    try
        (* compose the interceptor with the core symbolic executor *)
        let step = fun job -> 
            BasicReporter.convert_non_failure_abandoned_to_truncated (interceptor job Statement.step)
        in
        let rec run (queue, reporter) =
            checkpoint := (queue, reporter);
            match queue#get with
                | Some (queue, job) ->
                    let result = step job in
                    let rec process_result (queue, reporter) result =
                        let reporter = reporter#report result in
                        match result with
                            | Job.Active job ->
                                (queue#put job, reporter)
                            | Job.Fork results ->
                                List.fold_left process_result (queue, reporter) results
                            | Job.Complete completion ->
                                (queue, reporter)
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
    with UserSignal.UserInterrupt | UserSignal.TimedOut as exn ->
        (* if we got a signal, stop and return the checkpoint results *)
        Output.set_mode Output.MSG_REPORT;
        Output.printf "%s@." (Printexc.to_string exn);
        let (queue, reporter) = !checkpoint in
        (* For each job in queue, make it Abandoned and report it *)
        let rec run (queue, reporter) =
            match queue#get with
                | Some (queue, job) ->
                    Log.set_output_formatter job;
                    let result = Job.Complete (Job.Abandoned (`Failure "(Path execution not finished)", job)) in
                    let reporter = reporter#report result in
                    run (queue, reporter)
                | None ->
                    (queue, reporter)
        in
        run (queue, reporter)


let run ?(random_seed=(!Executeargs.arg_random_seed))
        ?(interceptor=Interceptor.identity_interceptor)
        ?(queue=Queue.get_default ())
        reporter
        file =
    Random.init random_seed;
    let job =
        let mainfn = ProgramPoints.get_main_fundec file in
        let entryfn = ProgramPoints.get_entry_fundec file in
        if mainfn == entryfn then
            (* create a job for the file, with the commandline arguments set to the file name
             * and the arguments from the '--arg' option *)
            new OtterJob.FileJob.t file (file.Cil.fileName::!ProgramPoints.command_line)
        else
            (* create a job that starts at entry_function *)
            new OtterJob.FunctionJob.t file entryfn
    in
    let queue = queue#put job in
    main_loop interceptor queue reporter


(** {1 Precomposed drivers for common use cases} *)

(** Driver using the core symbolic executor only. *)
let run_core reporter file =
    run reporter file

(** As with {!run}, using the core symbolic executor and core built-in functions. *)
let run_basic reporter file =
    let interceptor =
        Interceptor.set_output_formatter_interceptor
        >>> Interceptor.function_pointer_interceptor
        >>> BuiltinFunctions.interceptor
    in
    run ~interceptor reporter file

(** As with {!run}, using the core symbolic executor, core and libc built-in functions. *)
let run_with_libc reporter file =
    let interceptor =
        Interceptor.set_output_formatter_interceptor
        >>> Interceptor.function_pointer_interceptor
        >>> BuiltinFunctions.libc_interceptor
        >>> BuiltinFunctions.interceptor
    in
    run ~interceptor reporter file

