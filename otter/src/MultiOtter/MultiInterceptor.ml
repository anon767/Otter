open OtterCore
open MultiTypes
open Job

let (@@) = Interceptor.(@@)
let (@@@) i1 i2 = fun a b c -> i1 a b c i2

let intercept_multi_function_by_name_internal target_name replace_func job multijob job_queue interceptor =
	(* Replace a C function with Otter code *)
	match job#instrList with
		| (Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc) as instr)::_ when varinfo.Cil.vname = target_name ->
			OcamlUtilities.Output.set_mode OcamlUtilities.Output.MSG_STMT;
			OcamlUtilities.Output.printf "@[%a@]@." Printcil.instr instr;
			let job_state, multijob, errors = replace_func job multijob retopt exps [] in
			if errors = [] then
				(job_state, (multijob, job_queue))
			else
				let abandoned_job_states = Statement.errors_to_abandoned_list job errors in
				(Job.Fork (job_state::abandoned_job_states), (multijob, job_queue))
		| _ -> 
			interceptor job multijob job_queue

let unpack_job_interceptor job job_queue interceptor =
	let multijob, job_queue = job_queue in
	interceptor job multijob job_queue

let repack_job_interceptor job multijob job_queue interceptor =
	interceptor job (multijob, job_queue)

let abandon_io_block_deadlock_interceptor job multijob job_queue interceptor =
    match multijob.current_metadata.priority with
      | IOBlock _ -> (* The best job availiable is blocking. This is a deadlock. Each job will be abandoned one at a time. *)
            (Complete (Abandoned (`Failure "Deadlock", job)), (multijob, job_queue))
      | TimeWait _ -> (* If we choose a TimeWait, convert it to a Running rather than just letting its time tick down *)
            interceptor job { multijob with current_metadata = { multijob.current_metadata with priority = Running } } job_queue
      | _ -> interceptor job multijob job_queue
