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
        (* TODO: remove job_queue from interceptors/Statement.step *)
        let step = fun job -> fst (interceptor job () Statement.step) in
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
    with Types.SignalException s ->
        (* if we got a signal, stop and return the checkpoint results *)
        Output.set_mode Output.MSG_MUSTPRINT;
        Output.printf "%s@\n" s;
        let (queue, reporter) = !checkpoint in
        (* For each job in queue, make it Abandoned and report it *)
        let rec run (queue, reporter) =
            match queue#get with
                | Some (queue, job) ->
                    let result = Job.Complete(Job.Abandoned(`Failure "Timed Out", Job.get_loc job,Job.get_result_from_job job)) in
                    let reporter = reporter#report result in
                    run (queue, reporter)
                | None ->
                    (queue, reporter)
        in
        run (queue, reporter)


(* This is "Otter" in OtterBenchmark *)
let run ?(random_seed=(!Executeargs.arg_random_seed))
        ?(interceptor=Interceptor.identity_interceptor)
        ?(queue=Queue.get_default ())
        reporter
        job =
	Random.init random_seed;
    let queue = queue#put job in
    main_loop interceptor queue reporter

(** {1 Precomposed drivers for common use cases} *)

(** Driver using the core symbolic executor only. *)
let run_core reporter job =
    run reporter job

(** As with {!run}, using the core symbolic executor and core built-in functions. *)
let run_basic reporter job =
    let interceptor =
        Interceptor.set_output_formatter_interceptor
        >>> BuiltinFunctions.interceptor
    in
    run ~interceptor reporter job

(** As with {!run}, using the core symbolic executor, core and libc built-in functions. *)
let run_with_libc reporter job =
    let interceptor =
        Interceptor.set_output_formatter_interceptor
        >>> BuiltinFunctions.libc_interceptor
        >>> BuiltinFunctions.interceptor
    in
    run ~interceptor reporter job

