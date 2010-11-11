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
    (* compose the interceptor with the core symbolic executor *)
    (* TODO: remove job_queue from interceptors/Statement.step *)
    let step = fun job -> fst (interceptor job () Statement.step) in
    let rec run (queue, reporter) = match queue#get with
        | Some (queue, job) ->
            let result_opt =
                try
                    Some (step job)
                with Types.SignalException s ->
                    (* if we got a signal, stop and return the completed results *)
                    Output.set_mode Output.MSG_MUSTPRINT;
                    Output.printf "%s@\n" s;
                    None
            in
            begin match result_opt with
                | Some result ->
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
            end
        | None ->
            reporter
    in
    run (queue, reporter)

let run ?(interceptor=Interceptor.identity_interceptor)
        ?(queue=Queue.get_default ())
        reporter
        job =
	Random.init (!Executeargs.arg_random_seed);
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

