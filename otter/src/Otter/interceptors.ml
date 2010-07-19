open Executeargs
open Types

let get_job_loc job =
	match job.instrList with
		| [] -> (Cil.get_stmtLoc job.stmt.Cil.skind)
		| _ -> 
			let instr = match job.instrList with 
				| i::tl -> i 
				| _ -> assert false 
			in
			(Cil.get_instrLoc instr)


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

let otter_functions_interceptor job job_queue interceptor = 
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
	(intercept_function_by_name_internal "__PRINT_STATE"           (exec Builtin_function.otter_print_state)) @@
	(intercept_function_by_name_internal "__CURRENT_STATE"         (exec Builtin_function.otter_current_state)) @@
	(intercept_function_by_name_internal "__COMPARE_STATE"         (exec Builtin_function.otter_compare_state)) @@
	(intercept_function_by_name_internal "__ASSERT_EQUAL_STATE"    (exec Builtin_function.otter_assert_equal_state)) @@

	(* pass on the job when none of those match *)
	interceptor

	) job job_queue

