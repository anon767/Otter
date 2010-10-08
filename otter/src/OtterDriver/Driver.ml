open OcamlUtilities
open CilUtilities
open OtterCore
open OtterQueue
open OtterReporter

(**/**)
let (>>>) = Interceptor.(>>>)
(**/**)

let default_max_nodes = ref 0
let default_max_paths = ref 0
let default_max_abandoned = ref 0

(** Locate the function corresponding to {!Executeargs.entryfn}. *)
let find_entryfn file =
	let fname = !Executeargs.arg_entryfn in
	try
		FindCil.fundec_by_name file fname
	with Not_found ->
		FormatPlus.failwith "Entry function %s not found" fname

(** Main symbolic execution loop. *)
let main_loop
        ?(max_nodes=max 0 !default_max_nodes)
        ?(max_paths=max 0 !default_max_paths)
        ?(max_abandoned=max 0 !default_max_abandoned)
        ?interceptor:interceptor_opt
        ?(queue=Queue.get_default ())
        reporter
        job =
    (* compose the interceptor with the core symbolic executor *)
    (* TODO: remove job_queue from interceptors/Statement.step *)
    let step = match interceptor_opt with
        | Some interceptor -> fun job -> fst (interceptor job () Statement.step)
        | None -> fun job -> fst (Statement.step job ())
    in
    let queue = queue#put job in
    let rec run (nodes, paths, abandoned, queue, reporter) = match queue#get with
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
                    let rec process_result (nodes, paths, abandoned, queue, reporter as work) result k = match result with
                        | Job.Active job ->
                            k (nodes, paths, abandoned, queue#put job, reporter)
                        | Job.Fork (result::results) ->
                            process_result work result (fun work -> process_result work (Job.Fork results) k)
                        | Job.Fork [] ->
                            k work
                        | Job.Complete completion ->
                            let reporter = reporter#report completion in
                            if (max_paths = 0 || paths < max_paths)
                                    && (max_abandoned = 0 || abandoned < max_abandoned) then
                                k (nodes, paths + 1, abandoned + (match completion with Job.Abandoned _ -> 1 | _ -> 0), queue, reporter)
                            else
                                reporter
                        | Job.Paused _ ->
                            invalid_arg "unexpected Job.Paused"
                    in
                    process_result (nodes, paths, abandoned, queue, reporter) result begin fun (nodes, paths, abandoned, queue, reporter) ->
                        if max_nodes = 0 || nodes < max_nodes then
                            run (nodes + 1, paths, abandoned, queue, reporter)
                        else
                            reporter
                    end
                | None ->
                    reporter
            end
        | None ->
            reporter
    in
    run (1, 1, 1, queue, reporter)


(** {1 Precomposed drivers for common use cases} *)

(** Driver using {!OtterReporter.BasicReporter}. *)
let run ?max_nodes ?max_paths ?max_abandoned ?interceptor ?queue job =
    (main_loop ?max_nodes ?max_paths ?max_abandoned ?interceptor ?queue (BasicReporter.make ()) job)#completed

(** As with {!run}, using the core symbolic executor only as well as default bounds. *)
let run_core job =
    run job

(** As with {!run}, using the core symbolic executor and core built-in functions, as well as default bounds. *)
let run_basic job =
    let interceptor =
        Interceptor.set_output_formatter_interceptor
        >>> BuiltinFunctions.interceptor
    in
    run ~interceptor job

(** As with {!run}, using the core symbolic executor, core and libc built-in functions, as well as default bounds. *)
let run_with_libc job =
    let interceptor =
        Interceptor.set_output_formatter_interceptor
        >>> BuiltinFunctions.libc_interceptor
        >>> BuiltinFunctions.interceptor
    in
    run ~interceptor job


(** {1 Command-line options} *)

let options = [
    "--max-nodes",
        Arg.Set_int default_max_nodes,
        "<bound> Bound the number of nodes in the execution tree to explore (default: unbounded)";
    "--max-paths",
        Arg.Set_int default_max_paths,
        "<bound> Bound the number of paths to execute to completion (default: unbounded)";
    "--max-abandoned",
        Arg.Set_int default_max_abandoned,
        "<bound> Bound the number of abandoned paths to return (default: unbounded)";
]

