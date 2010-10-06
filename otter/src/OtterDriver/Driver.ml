open OcamlUtilities
open CilUtilities
open OtterCore
open OtterQueue
open OtterReporter

(**/**)
let (>>>) = Interceptor.(>>>)
(**/**)

let default_bound = ref 0

(** Locate the function corresponding to {!Executeargs.entryfn}. *)
let find_entryfn file =
	let fname = !Executeargs.arg_entryfn in
	try
		FindCil.fundec_by_name file fname
	with Not_found ->
		FormatPlus.failwith "Entry function %s not found" fname

(** Main symbolic execution loop. *)
let main_loop ?(max_abandoned_jobs=max_int) ?interceptor:interceptor_opt ?(queue=Queue.get_default ()) reporter job =
    (* compose the interceptor with the core symbolic executor *)
    (* TODO: remove job_queue from interceptors/Statement.step *)
    let step = match interceptor_opt with
        | Some interceptor -> fun job -> fst (interceptor job () Statement.step)
        | None -> fun job -> fst (Statement.step job ())
    in
    let queue = queue#put job in
    let rec run queue reporter num_abandoned_jobs = match queue#get with
        | Some (queue, job) ->
            let result_opt =
                try
                    let result = step job in
                    let rec process_result (queue, reporter, num_abandoned_jobs) job_result =
                        if num_abandoned_jobs <= 0 then
                            let rec make_empty queue = match queue#get with
                                | Some (queue, _) -> make_empty queue
                                | None -> queue
                            in
                            (make_empty queue, reporter, num_abandoned_jobs)
                        else
                            match job_result with
                            | Job.Active job -> (queue#put job, reporter, num_abandoned_jobs)
                            | Job.Fork results -> List.fold_left process_result (queue, reporter, num_abandoned_jobs) results
                            | Job.Complete ((Job.Abandoned _) as completion) ->
                                    (queue, reporter#report completion, num_abandoned_jobs - 1)
                            | Job.Complete completion -> (queue, reporter#report completion, num_abandoned_jobs)
                            | Job.Paused _ -> invalid_arg "unexpected Job.Paused"
                    in
                    let queue, reporter, num_abandoned_jobs = process_result (queue, reporter, num_abandoned_jobs) result in
                    Some (queue, reporter, num_abandoned_jobs)
                with Types.SignalException s ->
                    (* if we got a signal, stop and return the completed results *)
                    Output.set_mode Output.MSG_MUSTPRINT;
                    Output.printf "%s@\n" s;
                    None
            in
            begin match result_opt with
                | Some (queue, reporter, num_abandoned_jobs) -> run queue reporter num_abandoned_jobs
                | _ -> reporter
            end
        | None ->
            reporter
    in
    run queue reporter max_abandoned_jobs


(** {1 Precomposed drivers for common use cases} *)

let run ?max_abandoned_jobs ?interceptor ?queue job =
    (main_loop ?max_abandoned_jobs ?interceptor ?queue (BasicReporter.make ()) job)#completed

(** Driver using the core symbolic executor only. *)
let run_core job =
    run job

(** Driver using the core symbolic executor and the core built-in functions *)
let run_basic job =
    let interceptor =
        Interceptor.set_output_formatter_interceptor
        >>> BuiltinFunctions.interceptor
    in
    run ~interceptor job

(** Driver using the core symbolic executor, core built-in functions and libc built-in functions *)
let run_with_libc job =
    let interceptor =
        Interceptor.set_output_formatter_interceptor
        >>> BuiltinFunctions.libc_interceptor
        >>> BuiltinFunctions.interceptor
    in
    run ~interceptor job


(** {1 Command-line options} *)

let options = [
    "--bound",
        Arg.Set_int default_bound,
        "<bound> Bound the number of paths to symbolically execute to completion (default: unbounded)";
]

