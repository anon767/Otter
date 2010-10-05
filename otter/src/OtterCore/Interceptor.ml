open OcamlUtilities
open CilUtilities
open Types
open Job

let (@@) i1 i2 = fun a b -> i1 a b i2
let (>>>) i1 i2 = fun a b k -> i1 a b (fun a b -> i2 a b k)

let identity_interceptor job job_queue interceptor =
	interceptor job job_queue

let old_job_id = ref 0
let set_output_formatter_interceptor job job_queue interceptor = 
	if !old_job_id <> job.jid then (
		if not !Executeargs.arg_cfg_pruning then
		(
			Output.set_mode Output.MSG_REG;
			Output.printf "***** Changing running job *****@\n"
		);
		old_job_id := job.jid
	);
	let depth = List.length job.state.path_condition in
	let loc = Job.get_loc job in
	let label =
		if loc = Cil.locUnknown then
			Format.sprintf "[%d,%d] : " job.jid depth
		else
			Format.sprintf "[%d,%d] %s:%d : " job.jid depth loc.Cil.file loc.Cil.line
	in
	Output.set_formatter (new Output.labeled label);
	interceptor job job_queue

let intercept_function_by_name_internal target_name replace_func job job_queue interceptor =
	(* Replace a C function with Otter code *)
	(* replace_func retopt exps loc job job_queue *)
	match job.instrList with
		| Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc)::_ when varinfo.Cil.vname = target_name ->
			(replace_func job retopt exps, job_queue)
		| _ -> 
			interceptor job job_queue

let intercept_function_by_name_external target_name replace_name job job_queue interceptor =
	(* Replace a C function with another C function *)
	match job.instrList with
		| Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc)::t when varinfo.Cil.vname = target_name ->
			let job = 
				{job with
					instrList = Cil.Call(retopt, Cil.Lval(Cil.Var((FindCil.fundec_by_name job.file replace_name).Cil.svar), Cil.NoOffset), exps, loc)::t;
				}
			in
			Output.set_mode Output.MSG_REG;
			Output.printf "Transformed Call %s to Call %s@\n" target_name replace_name;
			(* Don't allow any other intercepters to transform the name again *)
			Statement.step job job_queue 
		| _ -> 
			interceptor job job_queue

let intercept_function_by_name_external_cascading target_name replace_name job job_queue interceptor =
	(* Replace a C function with another C function *)
	match job.instrList with
		| Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc)::t when varinfo.Cil.vname = target_name ->
			let job = 
				{job with
					instrList = Cil.Call(retopt, Cil.Lval(Cil.Var((FindCil.fundec_by_name job.file replace_name).Cil.svar), Cil.NoOffset), exps, loc)::t;
				}
			in
			Output.set_mode Output.MSG_REG;
			Output.printf "Transformed Call %s to Call %s@\n" target_name replace_name;
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

