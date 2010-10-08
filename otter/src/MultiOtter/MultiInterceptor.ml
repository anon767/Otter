open OtterCore
open OtterJob

let (@@) = Interceptor.(@@)
let (@@@) i1 i2 = fun a b c -> i1 a b c i2

let intercept_multi_function_by_name_internal target_name replace_func (job:Job.job) (multijob:MultiTypes.multijob) job_queue interceptor =
	(* Replace a C function with Otter code *)
	(* replace_func retopt exps loc job multijob job_queue *)
	match job.Job.instrList with
		| Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc)::_ when varinfo.Cil.vname = target_name ->
			let (job_result, multijob) = replace_func job multijob retopt exps in
				(job_result, (multijob, job_queue))
		| _ -> 
			interceptor job multijob job_queue

let unpack_job_interceptor job job_queue interceptor =
	let multijob, job_queue = job_queue in
	interceptor job multijob job_queue

let repack_job_interceptor job multijob job_queue interceptor =
	interceptor job (multijob, job_queue)
