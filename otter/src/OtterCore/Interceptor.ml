open OcamlUtilities
open CilUtilities
open State
open OtterBytes

let (@@) i1 i2 = fun a b -> i1 a b i2
let (>>>) i1 i2 = fun a b k -> i1 a b (fun a b -> i2 a b k)

let identity_interceptor job job_queue interceptor =
    interceptor job job_queue

let set_output_formatter_interceptor job job_queue interceptor =
    Log.set_output_formatter job;
    interceptor job job_queue

(** Interceptor which rewrites calls through function pointers into normal function calls *)
let function_pointer_interceptor job job_queue interceptor =
    match job#instrList with
      | Cil.Call(retopt, Cil.Lval(Cil.Mem(fexp), Cil.NoOffset), exps, loc) as instr::instrs ->
            let new_instrList varinfo =
                Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc) :: instrs
            in
            let job, bytes, errors  = Expression.rval job fexp [] in
            let getall fp =
                let varinfos_and_pres, errors = Bytes.conditional__fold begin fun (varinfos_and_pres, errors) pre leaf ->
                    match leaf with
                      | Bytes.Bytes_FunPtr varinfo ->
                            ((varinfo, pre)::varinfos_and_pres, errors)
                      | _ ->
                            (varinfos_and_pres, (job, `Failure "Invalid function pointer")::errors)
                end ([], errors) fp in
                let jobs = (job : #Info.t)#fork begin fun job (varinfo, pre) jobs ->
                    let job = MemOp.state__add_path_condition job (Bytes.guard__to_bytes pre) true in
                    let job = job#with_instrList (new_instrList varinfo) in
                    job::jobs
                end varinfos_and_pres [] in
                (jobs, varinfos_and_pres, errors)
            in
            let jobs, varinfos_and_pres, errors =
                begin match bytes with
                  | Bytes.Bytes_FunPtr varinfo ->
                        let job = job#with_instrList (new_instrList varinfo) in
                        ([job], [(varinfo, Bytes.Guard_True (* This [Guard_True] is just a placeholder *))], errors)
                  | Bytes.Bytes_Read(bytes2, offset, len) ->
                        let fp = (BytesUtility.expand_read_to_conditional bytes2 offset len) in
                        let (), fp = Bytes.conditional__prune ~test:(fun () pre guard -> ((), Stp.query_stp job#state.path_condition pre guard)) () fp in
                        getall fp
                  | Bytes.Bytes_Conditional(c) ->
                        getall c
                  | _ ->
                        ([], [], (job, `Failure (FormatPlus.sprintf "Invalid function ptr:@ @[%a@]" CilPrinter.exp fexp)) :: errors)
                end
            in
            Output.set_mode Output.MSG_FUNC;
            Output.printf "Call using function pointer:@\n@[%a@]@\n" Printcil.instr instr;
            begin match jobs with
              | _::_::_ -> (* if List.length jobs > 1 *)
                Output.printf "Function pointer can take multiple values; fork job %d to " job#path_id;
                List.iter2
                    (fun job (varinfo,_) -> Output.printf "(job %d,function %s)" job#path_id varinfo.Cil.vname)
                    (List.rev jobs) (* The call for #fork above reverses the list of jobs relative to the varinfos *)
                    varinfos_and_pres;
                Output.printf "@."
              | _ -> ()
            end;
            let job_states = List.rev_map (fun j -> Job.Active j) jobs in
            let abandoned_job_states = Statement.errors_to_abandoned_list job errors in
            let job_states = List.rev_append job_states abandoned_job_states in
            (Job.Fork job_states, job_queue)
      | _ -> interceptor job job_queue

let intercept_function_by_name_internal target_name replace_func job job_queue interceptor =
    (* Replace a C function with Otter code *)
    match job#instrList with
        | (Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, _) as instr)::_ when varinfo.Cil.vname = target_name ->
              Output.set_mode Output.MSG_STMT;
              Output.printf "@[%a@\n<built-in function>@]@." Printcil.instr instr;
              let job = job#with_decision_path (Decision.DecisionFuncall(instr, varinfo)::job#decision_path) in
              let job_state, errors = replace_func job retopt exps [] in
              let abandoned_job_states = Statement.errors_to_abandoned_list job errors in
              (Job.Fork (job_state::abandoned_job_states), job_queue)
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
			Output.printf "Transformed Call %s to Call %s@." target_name replace_name;
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
			Output.printf "Transformed Call %s to Call %s@." target_name replace_name;
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

