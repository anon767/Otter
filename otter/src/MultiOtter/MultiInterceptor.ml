open OtterCore
open MultiTypes
open Job

let (@@) = Interceptor.(@@)
let (@@@) i1 i2 = fun a b -> i1 a b i2

let intercept_multi_function_by_name_internal target_name replace_func job multijob interceptor =
	(* Replace a C function with Otter code *)
	match job#instrList with
		| (Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc) as instr)::_ when varinfo.Cil.vname = target_name ->
			OcamlUtilities.Output.set_mode OcamlUtilities.Output.MSG_STMT;
			OcamlUtilities.Output.printf "@[%a@]@." Printcil.instr instr;
			let job_state, multijob, errors = replace_func job multijob retopt exps [] in
			if errors = [] then
				(job_state, multijob)
			else
				let abandoned_job_states = Statement.errors_to_abandoned_list errors in
				(Job.Fork (job_state::abandoned_job_states), multijob)
		| _ -> 
			interceptor job multijob

let abandon_io_block_deadlock_interceptor job multijob interceptor =
    match multijob.current_metadata.priority with
      | IOBlock _ -> (* The best job availiable is blocking. This is a deadlock. Each job will be abandoned one at a time. *)
            (Complete (Abandoned (`Failure "Deadlock", job)), multijob)
      | TimeWait _ -> (* If we choose a TimeWait, convert it to a Running rather than just letting its time tick down *)
            interceptor job { multijob with current_metadata = { multijob.current_metadata with priority = Running } }
      | _ -> interceptor job multijob
