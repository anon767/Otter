open OcamlUtilities
open CilUtilities
open State
open OtterBytes

let (@@) i1 i2 = fun a -> i1 a i2
let (>>>) i1 i2 = fun a k -> i1 a (fun a -> i2 a k)

let identity_interceptor job interceptor =
    interceptor job

let set_output_formatter_interceptor job interceptor =
    Log.set_output_formatter job;
    interceptor job

(** Interceptor which rewrites calls through function pointers into normal function calls *)
let function_pointer_interceptor job interceptor =
    match job#instrList with
        | Cil.Call(retopt, Cil.Lval(Cil.Mem(fexp), Cil.NoOffset), exps, loc) as instr::instrs ->
            Output.set_mode Output.MSG_FUNC;
            Output.printf "Call using function pointer:@\n@[%a@]@." Printcil.instr instr;

            let new_instrList varinfo =
                Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc) :: instrs
            in

            let typ = Cil.typeOf fexp in
            let length = Cil.bitsSizeOf typ / 8 in
            let job, bytes = Expression.rval job fexp in

            (* get all valid target functions *)
            let module VarinfoSet = Set.Make (CilData.CilVar) in

            (* TODO: refactor and lift this pattern as it occurs in three places: Expression.deref, BuiltinFunctions.libc_free,
             * and Interceptor.function_pointer_interceptor *)
            let rec get_funptr job pre varinfos = function
                | Bytes.Bytes_FunPtr varinfo ->
                    (job, VarinfoSet.add varinfo varinfos, false)
                | Bytes.Bytes_Conditional c ->
                    Bytes.conditional__fold
                        ~pre
                        ~test:begin fun (job, varinfos, has_errors) pre guard ->
                            let job, truth =
                                (job : _ #Info.t)#profile_call "function_pointer_interceptor/Bytes_Conditional"
                                    (fun job -> (job, BytesSTP.query_stp (PathCondition.clauses job#state.path_condition) pre guard))
                            in
                            ((job, varinfos, has_errors), truth)
                        end
                        begin fun (job, varinfos, has_errors) pre' bytes ->
                            try
                                (* TODO: what to do if a job#fork happen? doesn't happen currently, but could
                                 * potentially be a performance problem *)
                                let job, varinfos, has_errors' = get_funptr job (Bytes.guard__and pre pre') varinfos bytes in
                                (job, varinfos, has_errors || has_errors')
                            with Failure msg ->
                                let job = (job : _ #Info.t)#fork_finish (Job.Abandoned (`Failure msg)) in
                                (job, varinfos, true)
                        end (job, varinfos, false) c
                | Bytes.Bytes_Read (bytes, offset, len) ->
                    get_funptr job pre varinfos (Bytes.make_Bytes_Conditional (BytesUtility.expand_read_to_conditional bytes offset length))
                | _ ->
                    let job = (job : _ #Info.t)#fork_finish (Job.Abandoned (`Failure (FormatPlus.sprintf "@[Invalid function pointer:@ %a@]" CilPrinter.exp fexp))) in
                    (job, varinfos, true)
            in
            let job, varinfos, has_errors = get_funptr job Bytes.guard__true VarinfoSet.empty bytes in

            begin match VarinfoSet.elements varinfos with
                | [] ->
                    (* No valid functions: just fail. *)
                    (job : _ #Info.t)#finish (Job.Truncated (`Failure "Invalid function pointer"))
                | [ varinfo ] ->
                    let job =
                        if has_errors then
                            let funptr_condition = Operator.eq [ (bytes, typ); (Bytes.make_Bytes_FunPtr varinfo, typ) ] in
                            MemOp.state__add_path_condition job funptr_condition true
                        else
                            job
                    in
                    job#with_instrList (new_instrList varinfo)
                | varinfos ->
                    Output.printf "@[Function pointer can take multiple values; fork job %d to@." job#node_id;
                    let job, varinfo = (job : _ #Info.t)#fork varinfos in
                    Output.printf "(job %d, function %s)@." job#node_id varinfo.Cil.vname;
                    let funptr_condition = Operator.eq [ (bytes, typ); (Bytes.make_Bytes_FunPtr varinfo, typ) ] in
                    let job = MemOp.state__add_path_condition job funptr_condition true in
                    let job = job#with_instrList (new_instrList varinfo) in
                    job
            end
      | _ ->
            interceptor job


exception Not_applicable

let intercept_function_by_name_internal target_name replace_func job interceptor =
    (* Replace a C function with Otter code *)
    match job#instrList with
        | (Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, _) as instr)::_ when varinfo.Cil.vname = target_name ->
            Output.set_mode Output.MSG_STMT;
            Output.printf "@[%a@\n<built-in function>@]@." Printcil.instr instr;
            begin try
                let job = job#append_decision_path (Decision.make_Decision_Funcall(instr, varinfo)) in
                replace_func job retopt exps
            with Not_applicable ->
                interceptor job
            end
        | _ ->
            interceptor job

let intercept_function_by_name_external target_name replace_name job interceptor =
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
			Statement.step job
		| _ ->
			interceptor job

let intercept_function_by_name_external_cascading target_name replace_name job interceptor =
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
			job
		| _ ->
			interceptor job

