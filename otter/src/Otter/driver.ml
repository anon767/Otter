open Cil
open Ternary
open Bytes
open BytesUtility
open Types
open Executeargs
open Cilutility
open Utility


(** GET JOB **)

let get_job_loc job =
	match job.instrList with
		| [] -> (Cil.get_stmtLoc job.stmt.skind)
		| _ -> 
			let instr = match job.instrList with 
				| i::tl -> i 
				| _ -> assert false 
			in
			(Cil.get_instrLoc instr)

let get_job_list = function
	| [] -> None
	| h::t -> Some (h, t)

let get_job_priority_queue job_queue = 
	if Jobs.has_next_runnable job_queue then
		Some ((Jobs.take_next_runnable job_queue), job_queue)
	else
		None


(** INTERCEPTORS **)

(* TODO: find a good place to put this definition instead of duplicating it *)
let (@@) i1 i2 = fun a b -> i1 a b i2

let identity_interceptor job job_queue interceptor =
	interceptor job job_queue

let old_job_id = ref 0
let set_output_formatter_interceptor job job_queue interceptor = 
	if !old_job_id <> job.jid then (
		if not Executeargs.run_args.arg_cfg_pruning then
		(
			Output.set_mode Output.MSG_REG;
			Output.print_endline "***** Changing running job *****"
		);
		old_job_id := job.jid
	);
	Output.formatter := ((new Output.basic_formatter job.jid (List.length job.state.path_condition) (get_job_loc job)) 
		:> Output.formatter_base);
	interceptor job job_queue

let intercept_function_by_name_internal target_name replace_func job job_queue interceptor =
	(* Replace a C function with Otter code *)
	(* replace_func retopt exps loc job job_queue *)
	match job.instrList with
		| Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc)::_ when varinfo.Cil.vname = target_name ->
			replace_func retopt exps loc job job_queue
		| _ -> 
			interceptor job job_queue

let intercept_function_by_name_external target_name replace_name job job_queue interceptor =
	(* Replace a C function with another C function *)
	(* requires running Cilutility.init_func_table *)
	match job.instrList with
		| Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc)::t when varinfo.Cil.vname = target_name ->
			let job = 
				{job with
					instrList = Cil.Call(retopt, Cil.Lval(Cil.Var((Hashtbl.find Cilutility.func_table replace_name).Cil.svar), Cil.NoOffset), exps, loc)::t;
				}
			in
			Output.set_mode Output.MSG_REG;
			Output.print_endline (Format.sprintf "Transformed Call %s to Call %s" target_name replace_name);
			(* Don't allow any other intercepters to transform the name again *)
			Core.step job job_queue 
		| _ -> 
			interceptor job job_queue

let intercept_function_by_name_external_cascading target_name replace_name job job_queue interceptor =
	(* Replace a C function with another C function *)
	(* requires running Cilutility.init_func_table *)
	match job.instrList with
		| Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc)::t when varinfo.Cil.vname = target_name ->
			let job = 
				{job with
					instrList = Cil.Call(retopt, Cil.Lval(Cil.Var((Hashtbl.find Cilutility.func_table replace_name).Cil.svar), Cil.NoOffset), exps, loc)::t;
				}
			in
			Output.set_mode Output.MSG_REG;
			Output.print_endline (Format.sprintf "Transformed Call %s to Call %s" target_name replace_name);
			(* allow any intercepters to transform the name again *)
			(Active job, job_queue) 
		| _ -> 
			interceptor job job_queue

let try_with_job_abandoned_interceptor try_interceptor job job_queue interceptor =
	let result, jq = try_interceptor job job_queue (fun j jq -> (Paused j, jq)) in
	match result with
		| Complete (Abandoned (_, _, _)) -> interceptor job job_queue (* try_interceptor failed; move on *)
		| Paused j -> interceptor j jq (* try_interceptor passed on control *)
		| _ -> (result, jq) (* try_interceptor did not fail and did not pass on control *)

let intercept_extended_otter_functions job job_queue interceptor = 
	let exec = Builtin_function.call_wrapper in
	let call = Builtin_function.simple_call_wrapper in
	(

	(* intercept builtin functions *)
	(intercept_function_by_name_internal "__builtin_alloca"        (exec Builtin_function.libc___builtin_alloca)) @@
	(intercept_function_by_name_internal "malloc"                  (exec Builtin_function.libc___builtin_alloca)) @@
	(intercept_function_by_name_internal "free"                    (call Builtin_function.libc_free)) @@
	(intercept_function_by_name_internal "__builtin_va_arg_fixed"  (call Builtin_function.libc___builtin_va_arg)) @@
	(intercept_function_by_name_internal "__builtin_va_arg"        (call Builtin_function.libc___builtin_va_arg)) @@
	(intercept_function_by_name_internal "__builtin_va_copy"       (call Builtin_function.libc___builtin_va_copy)) @@
	(intercept_function_by_name_internal "__builtin_va_end"        (call Builtin_function.libc___builtin_va_end)) @@
	(intercept_function_by_name_internal "__builtin_va_start"      (call Builtin_function.libc___builtin_va_start)) @@
	(* memset defaults to the C implimentation on failure *)
	(try_with_job_abandoned_interceptor 
	(intercept_function_by_name_internal "memset"                  (call Builtin_function.libc_memset))) @@
	(intercept_function_by_name_internal "memset__concrete"        (call Builtin_function.libc_memset__concrete)) @@
	(intercept_function_by_name_internal "exit"                    (     Builtin_function.libc_exit)) @@
	(intercept_function_by_name_internal "__TRUTH_VALUE"           (call Builtin_function.otter_truth_value)) @@
	(intercept_function_by_name_internal "__GIVEN"                 (call Builtin_function.otter_given)) @@
	(intercept_function_by_name_internal "__SYMBOLIC_STATIC"       (exec Builtin_function.otter_symbolic_static)) @@
	(intercept_function_by_name_internal "__EVAL"                  (exec Builtin_function.otter_evaluate)) @@
	(intercept_function_by_name_internal "__EVALSTR"               (exec Builtin_function.otter_evaluate_string)) @@
	(intercept_function_by_name_internal "__SYMBOLIC_STATE"        (exec Builtin_function.otter_symbolic_state)) @@
	(intercept_function_by_name_internal "__ASSUME"                (exec Builtin_function.otter_assume)) @@
	(intercept_function_by_name_internal "__PATHCONDITION"         (exec Builtin_function.otter_path_condition)) @@
	(intercept_function_by_name_internal "__ASSERT"                (exec Builtin_function.otter_assert)) @@
	(intercept_function_by_name_internal "__ITE"                   (call Builtin_function.otter_if_then_else)) @@
	(intercept_function_by_name_internal "AND"                     (call (Builtin_function.otter_boolean_op Cil.LAnd))) @@
	(intercept_function_by_name_internal "OR"                      (call (Builtin_function.otter_boolean_op Cil.LOr))) @@
	(intercept_function_by_name_internal "NOT"                     (call Builtin_function.otter_boolean_not)) @@
	(intercept_function_by_name_internal "__COMMENT"               (exec Builtin_function.otter_comment)) @@
	(intercept_function_by_name_internal "__BREAKPT"               (exec Builtin_function.otter_break_pt)) @@
	
	(* pass on the job when none of those match *)
	interceptor

	) job job_queue


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

		| Big_Fork states ->
			List.fold_left (fun (completed, job_queue) state -> process_result state completed job_queue) (completed, job_queue) states

		| Complete completion ->
			output_completion_info completion;
			((completion::completed), job_queue)

		| _ -> 
			(completed, job_queue)

let rec process_result_priority_queue result completed job_queue =
	match result with
		| Active job ->
			Jobs.add_runnable job_queue job;
			(completed, job_queue)

		| Big_Fork states ->
			List.fold_left (fun (completed, job_queue) state -> process_result_priority_queue state completed job_queue) (completed, job_queue) states

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
			intercept_extended_otter_functions @@ 
			Core.step
		)
		process_result
		[job]

