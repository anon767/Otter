open OcamlUtilities
open CilUtilities
open OtterCore
open Executeargs
open Types
open Job
open Interceptor


(* set up the file for symbolic execution *)
let prepare_file file =
	(* makeCFGFeature must precede the call to getProgInfo. *)
	Cilly.makeCFGFeature.Cil.fd_doit file;

	Cil.iterGlobals file begin function
		| Cil.GFun(fundec,_) ->
			(* Reset sids to be unique only within a function, rather than globally, so that they will be given
				consistent sids across different analysis runs even if different files are merged
			*)
			ignore (List.fold_left (fun n stmt -> stmt.Cil.sid <- n; succ n) 0 fundec.Cil.sallstmts);

		| _ ->
			()
	end;

	if !Executeargs.arg_noinit_unreachable_globals then
		Coverage.computeReachableCode file;

	Coverage.prepare_file file


let find_entryfn file =
	let fname = !Executeargs.arg_entryfn in
	try
		FindCil.fundec_by_name file fname
	with Not_found ->
		FormatPlus.failwith "Entry function %s not found" fname



(** GET JOB **)

let get_job_list = function
	| [] -> None
	| h::t -> Some (h, t)


(** PROCESS RESULT **)

let output_completion_info completion =
(* log some interesting errors *)
	match completion with
		| Abandoned (reason, loc, { result_state=state; result_history=hist }) ->
			Output.set_mode Output.MSG_MUSTPRINT;
			Output.printf "Error \"%a\"@ occurs at %a.@\n"
				Report.abandoned_reason reason Printcil.loc loc;
			if !Executeargs.arg_print_callstack then
				Output.printf "Call stack:@\n  @[%a@]@\n" (Printer.callingContext_list "@\n") state.callContexts;
			Output.printf "Abandoning path.@\n"

		| _ ->
			()

let rec process_result result completed job_queue =
	match result with
		| Active job ->
			(completed, job::job_queue)

		| Fork states ->
			List.fold_left (fun (completed, job_queue) state -> process_result state completed job_queue) (completed, job_queue) states

		| Complete completion ->
			output_completion_info completion;
			((completion::completed), job_queue)

		| _ ->
			(completed, job_queue)


(** MAIN LOOP **)

let main_loop get_job interceptor process_result job_queue =
	let rec main_loop job_queue completed =
		match get_job job_queue with
			| Some (job, job_queue) ->
				let result_opt =
					try
						let result, job_queue = interceptor job job_queue in
						let completed, job_queue = process_result result completed job_queue in
						Some (job_queue, completed)
					with SignalException s ->
						(* if we got a signal, stop and return the completed results *)
						Output.set_mode Output.MSG_MUSTPRINT;
						Output.printf "%s@\n" s;
						None
				in
				begin match result_opt with
					| Some (job_queue, completed) -> main_loop job_queue completed
					| None -> completed
				end
			| None ->
				completed
	in
	main_loop job_queue []

let run job =
	main_loop
		get_job_list
		(
			set_output_formatter_interceptor @@
			BuiltinFunctions.interceptor @@
			Statement.step
		)
		process_result
		[job]

let run_with_libc job =
	main_loop
		get_job_list
		(
			set_output_formatter_interceptor @@
			BuiltinFunctions.interceptor @@
			BuiltinFunctions.libc_interceptor @@
			Statement.step
		)
		process_result
		[job]

