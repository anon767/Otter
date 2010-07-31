open Executeargs
open Types
open Interceptors


(** GET JOB **)

let get_job_list = function
	| [] -> None
	| h::t -> Some (h, t)


(** PROCESS RESULT **)

let output_completion_info completion =
(* log some interesting errors *)
	match completion with
		| Types.Abandoned (msg, loc, { result_state=state; result_history=hist }) ->
			Output.set_mode Output.MSG_MUSTPRINT;
			Output.printf "Error \"%s\" occurs at %s\n%sAbandoning path\n"
			msg (To_string.location loc)
			(
				if Executeargs.print_args.arg_print_callstack then
					"Call stack:\n"^(To_string.callstack state.callContexts)
				else
					""
			);
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

let main_loop get_job interceptor process_result job_queue : job_completion list =
	let rec main_loop job_queue completed : job_completion list =
		match !signalStringOpt with
			| Some s ->
				(* if we got a signal, stop and return the completed results *)
				Output.set_mode Output.MSG_MUSTPRINT;
				Output.print_endline s;
				completed
			| None ->
				match get_job job_queue with
					| None -> completed
					| Some (job, job_queue) ->
						let result, job_queue = interceptor job job_queue in
						let completed, job_queue = process_result result completed job_queue in
						main_loop job_queue completed
	in
	main_loop job_queue []

let init job = 
	main_loop
		get_job_list
		(
			set_output_formatter_interceptor @@
			Builtin_function.interceptor @@ 
			Core.step
		)
		process_result
		[job]

let init_with_libc job = 
	main_loop
		get_job_list
		(
			set_output_formatter_interceptor @@
			Builtin_function.interceptor @@ 
			Builtin_function.libc_interceptor @@
			Core.step
		)
		process_result
		[job]

