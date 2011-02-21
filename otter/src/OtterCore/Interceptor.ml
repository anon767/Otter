open OcamlUtilities
open CilUtilities
open State


let (@@) i1 i2 = fun a b -> i1 a b i2
let (>>>) i1 i2 = fun a b k -> i1 a b (fun a b -> i2 a b k)

let identity_interceptor job job_queue interceptor =
    interceptor job job_queue

let set_output_formatter_interceptor job job_queue interceptor =
    Log.set_output_formatter job;
    interceptor job job_queue

let intercept_function_by_name_internal target_name replace_func job job_queue interceptor =
    (* Replace a C function with Otter code *)
    let run_varinfo job retopt varinfo exps instr =
        Output.set_mode Output.MSG_STMT;
        Output.printf "%a@\n" Printcil.instr instr;
        Output.printf "Built-in function %s is run@\n" varinfo.Cil.vname;
        let job = job#with_decision_path (Decision.DecisionFuncall(instr, varinfo)::job#decision_path) in
        let job_state, errors = replace_func job retopt exps [] in
        if errors = [] then
            (job_state, job_queue)
        else
            let abandoned_job_states = Statement.errors_to_abandoned_list job errors in
            (Job.Fork (job_state::abandoned_job_states), job_queue)
    in
    match job#instrList with
        | (Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, _) as instr)::_ when varinfo.Cil.vname = target_name ->
            run_varinfo job retopt varinfo exps instr
        | (Cil.Call(retopt, Cil.Lval(Cil.Mem(fexp), Cil.NoOffset), exps, _) as instr)::_ ->
            (* Caution: This is done for each call to intercept_function_by_name_internal *)
            (* Caution: Errors from rval are ignored *)
            let _, bytes, _  = Expression.rval job fexp [] in 
            begin match bytes with
                | OtterBytes.Bytes.Bytes_FunPtr varinfo when varinfo.Cil.vname = target_name ->
                    run_varinfo job retopt varinfo exps instr
                | _ ->
                    interceptor job job_queue
            end
        | _ ->
            interceptor job job_queue

let intercept_function_by_name_external target_name replace_name job job_queue interceptor =
	(* Replace a C function with another C function *)
	match job#instrList with
		| Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc)::t when varinfo.Cil.vname = target_name ->
			let job =
				try
					let fundec = FindCil.fundec_by_name job#file replace_name in
					job#with_instrList (Cil.Call(retopt, Cil.Lval(Cil.Var(fundec.Cil.svar), Cil.NoOffset), exps, loc)::t)
				with Not_found ->
					FormatPlus.failwith "Cannot find fundec for %s" replace_name
			in
			Output.set_mode Output.MSG_REG;
			Output.printf "Transformed Call %s to Call %s@\n" target_name replace_name;
			(* Don't allow any other intercepters to transform the name again *)
			Statement.step job job_queue
		| _ ->
			interceptor job job_queue

let intercept_function_by_name_external_cascading target_name replace_name job job_queue interceptor =
	(* Replace a C function with another C function *)
	match job#instrList with
		| Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc)::t when varinfo.Cil.vname = target_name ->
			let job =
				try
					let fundec = FindCil.fundec_by_name job#file replace_name in
					job#with_instrList (Cil.Call(retopt, Cil.Lval(Cil.Var(fundec.Cil.svar), Cil.NoOffset), exps, loc)::t)
				with Not_found ->
					FormatPlus.failwith "Cannot find fundec for %s" replace_name
			in
			Output.set_mode Output.MSG_REG;
			Output.printf "Transformed Call %s to Call %s@\n" target_name replace_name;
			(* allow any intercepters to transform the name again *)
			(Job.Active job, job_queue)
		| _ ->
			interceptor job job_queue

let try_with_job_abandoned_interceptor try_interceptor job job_queue interceptor =
    try 
        let result, jq = try_interceptor job job_queue (fun j jq -> (Job.Paused j, jq)) in
        match result with
            | Job.Complete (Job.Abandoned (_, _)) -> interceptor job job_queue (* try_interceptor failed; move on *)
            | Job.Paused j -> interceptor j jq (* try_interceptor passed on control *)
            | _ -> (result, jq) (* try_interceptor did not fail and did not pass on control *)
    with Failure _ ->
        (* TODO: log this *)
        interceptor job job_queue

