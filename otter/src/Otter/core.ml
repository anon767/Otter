open Cil
open Ternary
open Bytes
open BytesUtility
open Types
open Executeargs
open Cilutility
open Utility

(** Get the file location for the current job instruction.
		@param job the job to get the current location from
		@return the file location
*)
let get_job_loc job =
	match job.instrList with
		| [] -> (Cil.get_stmtLoc job.stmt.skind)
		| _ -> 
			let instr = match job.instrList with 
				| i::tl -> i 
				| _ -> assert false 
			in
			(Cil.get_instrLoc instr)

let stmtInfo_of_job job =
	{ siFuncName = (List.hd job.state.callstack).svar.vname;
		siStmt = Cilutility.stmtAtEndOfBlock job.stmt; }

(** Return a new exHist with
		- block coverage updated by the addition of the statement at the
			end of the block containing [job.stmt]
		- line coverage is updated if [job.stmt] is an if, return, goto,
			or loop
		- if nextStmtOpt is [Some s] and [job.stmt] is the end of a block,
			edge coverage is updated by the addition of the edge from
			[job.stmt] to the end of [s]'s block
		- whichBranch tells us which branch we are taking for condition
			coverage, which only cares about [If] statements. whichBranch is
			ignored for other types of coverage, and it is entirely ignored
			if [job.stmt] is not an [If]. (Really, it might be better if
			whichBranch were a bool option, so we could pass in [None] if
			weren't at an [If], but this is more convenient.)
		Each form of coverage is only updated if it was requested. *)
let addStmtCoverage job whichBranch nextStmtOpt =
	{ job.exHist with
			coveredLines =
				if run_args.arg_line_coverage
				then match job.stmt.skind with
						If(_,_,_,loc)
					| Cil.Return(_,loc)
					| Goto(_,loc)
					| Loop(_,loc,_,_) ->
							LineSet.add (loc.file,loc.line) job.exHist.coveredLines
					| _ -> job.exHist.coveredLines
				else LineSet.empty;
			coveredBlocks =
				if run_args.arg_block_coverage
				then StmtInfoSet.add (stmtInfo_of_job job) job.exHist.coveredBlocks
				else StmtInfoSet.empty;
			coveredEdges =
				if run_args.arg_edge_coverage
				then (
					match nextStmtOpt with
							Some nextStmt when job.stmt == Cilutility.stmtAtEndOfBlock job.stmt ->
									let funcName = (List.hd job.state.callstack).svar.vname in
									EdgeSet.add
										({ siFuncName = funcName; siStmt = job.stmt; },
										 { siFuncName = funcName;
											 siStmt = Cilutility.stmtAtEndOfBlock nextStmt; })
										job.exHist.coveredEdges
						| _ -> job.exHist.coveredEdges
				) else EdgeSet.empty;
			coveredConds =
				if run_args.arg_cond_coverage
				then (
						match job.stmt.skind with
								If _ -> CondSet.add (stmtInfo_of_job job,whichBranch) job.exHist.coveredConds
						| _ -> job.exHist.coveredConds
				) else CondSet.empty;
			executionPath =
				if run_args.arg_path_coverage && job.stmt == Cilutility.stmtAtEndOfBlock job.stmt
				then (
					{ siFuncName = (List.hd job.state.callstack).svar.vname; siStmt = job.stmt; } :: job.exHist.executionPath
				) else (
					job.exHist.executionPath
				)
	}

let addInstrCoverage job instr =
	let instrLoc = get_instrLoc instr in
	{ job.exHist with coveredLines =
			LineSet.add (instrLoc.file,instrLoc.line) job.exHist.coveredLines; }

let add_guard_to_state state guard = (*big hack; there should be a nicer way to do this*)
	MemOp.state__add_path_condition state (Bytes_Conditional(Bytes.IfThenElse(guard, Unconditional(lazy_int_to_bytes 1), Unconditional(lazy_int_to_bytes 0)))) true

let function_from_exp state exp args: (state * fundec) list =
	match exp with
		| Lval(Var(varinfo), NoOffset) ->
			begin			
				try
					[(state, Cilutility.search_function varinfo)]
				with Not_found ->
					failwith ("Function "^varinfo.vname^" not found.")
			end

		| Lval(Mem(exp2), NoOffset) ->
			let state, bytes = Eval.rval state exp2 in
			let rec getall fp =
				let fold_func acc pre leaf =
					let acc =
						match leaf with
							| Bytes_FunPtr(fundec,_) -> 
								(add_guard_to_state state pre, fundec)::acc
							| _ -> acc (* should give a warning here about a non-valid function pointer*)
					in
					(acc, Unconditional(leaf))
				in
				let acc, fp = Bytes.conditional__map_fold ~test:(Stp.query_guard state.path_condition) fold_func [] fp in
				(*Output.set_mode Output.MSG_MUSTPRINT;				
				Output.print_endline (To_string.bytes (Bytes_Conditional(fp)));*)
				acc
			in
			begin match bytes with
				| Bytes_FunPtr(fundec,_) -> [(state, fundec)]
				| Bytes_Read(bytes2, offset, len) -> 
					let fp = (BytesUtility.expand_read_to_conditional bytes2 offset len) in
					(*Output.print_endline (To_string.bytes (Bytes_Conditional(fp)));*)
					(getall fp)
				| Bytes_Conditional(c) ->
					(getall c)

				| _ -> failwith ("Non-constant function ptr not supported : "^(To_string.exp exp2))
			end
		| _ ->
			failwith ("Non-constant function ptr not supported : "^(To_string.exp exp))

let exec_fundec job state instr fundec lvalopt exps = 
	let stmt = job.stmt in

	(* evaluate the arguments *)
	let state, argvs = List.fold_right begin fun exp (state, argvs) ->
		let state, bytes = Eval.rval state exp in
		(state, bytes::argvs)
	end exps (state, []) in

	(* [stmt] is an [Instr], so it can't have two successors. If
	 [func] returns, then [stmt] has exactly one successor. If
	 [func] is [exit] or has the [noreturn] attribute, [stmt]
	 has no successor. *)
	let callContext = match stmt.succs with
		| []  -> NoReturn instr
		| [h] -> Source (lvalopt,stmt,instr,h)
		| _   -> assert false
	in
	let state = MemOp.state__start_fcall state callContext fundec argvs in

	(*
	(* If fundec is the function to be examined *)
	if Executeargs.run_args.arg_examfn = fundec.svar.vname then
		InvInput.examine state fundec;
	*)

	(* Update the state, the next stmt to execute, and whether or
	 not we're in a tracked function. *)
	Active { job with
			state = state;
			stmt = List.hd fundec.sallstmts;
			inTrackedFn = StringSet.mem fundec.svar.vname run_args.arg_fns; }

let exec_instr_call job instr lvalopt fexp exps =
	let state, exHist = job.state, job.exHist in

	let rec process_func_list func_list =
		match func_list with
			| [] -> []
			| (state, fundec)::t -> 
				let job_state =
					try
						(exec_fundec job state instr fundec lvalopt exps)
					with Failure msg ->
						if run_args.arg_failfast then failwith msg;
						let result = { result_state = state; result_history = exHist } in
						Complete (Types.Abandoned (msg, get_job_loc job, result))
				in
				job_state::(process_func_list t)
	in
	let f = (process_func_list (function_from_exp state fexp exps)) in
	match f with
		| _::_::_ -> Big_Fork(f)
		| [a] -> a
		| [] -> failwith "No valid function found!"


let exec_instr job =
	assert (job.instrList <> []);
	let printInstr instr =
		Output.set_mode Output.MSG_STMT;
		Output.print_endline (To_string.instr instr)
	in

	let instr,tail = match job.instrList with i::tl -> i,tl | _ -> assert false in
	let job = { job with instrList = tail; } in

	(* Within instructions, we have to update line coverage (but not
		 statement or edge coverage). *)
	let job =
		if job.inTrackedFn && run_args.arg_line_coverage
		then { job with exHist = addInstrCoverage job instr; }
		else job
	in

	(* Since we've used the makeCFGFeature, an [Instr] is a series of
	   [Set]s and [Asm]s, possibly terminated with a [Call]. *)
	match instr with
		 | Set(cil_lval, exp, loc) ->
			printInstr instr;
			let state = job.state in
			let state, lval = Eval.lval state cil_lval in
			let state, rv = Eval.rval state exp in
			let state = MemOp.state__assign state lval rv in
			let nextStmt = if tail = [] then List.hd job.stmt.succs else job.stmt in
			Active { job with state = state; stmt = nextStmt }
		| Call(lvalopt, fexp, exps, loc) ->
			assert (tail = []);
			printInstr instr;
			exec_instr_call job instr lvalopt fexp exps
		| Asm _ ->
			Output.set_mode Output.MSG_MUSTPRINT;
			Output.print_endline "Warning: ASM unsupported";
			printInstr instr;
			Active (if tail = [] (* Update job.stmt if this instr ends this stmt *)
							then { job with stmt = List.hd job.stmt.succs }
							else job)

let exec_stmt job =
	assert (job.instrList = []);
	let state,stmt = job.state,job.stmt in

	let nextExHist ?(whichBranch=false) nextStmtOpt =
		if job.inTrackedFn
		then addStmtCoverage job whichBranch nextStmtOpt
		else job.exHist
	in

	Output.set_mode Output.MSG_STMT;
	Output.print_endline (To_string.stmt stmt);
	match stmt.skind with
		| Instr [] ->
				let nextStmt = match stmt.succs with [x] -> x | _ -> assert false in
				Active { job with stmt = nextStmt; exHist = nextExHist (Some nextStmt); }
		| Instr (instrs) ->
			(* We can certainly update block coverage here, but not
				 necessarily edge coverage. If instrs contains a call, we
				 don't know for certain that we'll traverse the edge from this
				 statement to its successor: the call might never return. So
				 if there is a call, we postpone (by passing None as the next
				 stmt) recording the edge until the return. (Line coverage for
				 the instructions themselves are handled in exec_instr.) *)
				let nextStmtOpt =
					if List.exists (function Call _ -> true | _ -> false) instrs
					then None
					else Some (match stmt.succs with [x] -> x | _ -> assert false)
				in
			Active { job with instrList = instrs; exHist = nextExHist nextStmtOpt; }
		| Cil.Return (expopt, loc) ->
				begin
					match state.callContexts with
						| Runtime::_ -> (* completed symbolic execution (e.g., return from main) *)
								Output.set_mode Output.MSG_MUSTPRINT;
								Output.print_endline "Program execution finished";
(*
								if run_args.arg_line_coverage then (
									Report.printPath state job.exHist;
									Report.printLines job.exHist.coveredLines
								);
								Output.set_mode Output.MSG_REG;
								Output.print_endline
									("Path condition:\n" ^
										 (To_string.humanReadablePc state.path_condition job.exHist.bytesToVars));
*)
								let state, retval = match expopt with
									| None ->
										(state, None)
									| Some exp ->
										let state, retval = Eval.rval state exp in
										(state, Some retval)
								in
								Complete (Types.Return
									(retval, { result_state = state; result_history = nextExHist None; }))
						| (Source (destOpt,callStmt,_,nextStmt))::_ ->
								let state2 =
									match expopt, destOpt with
										| Some exp, Some dest ->
												(* evaluate the return expression in the callee frame *)
												let state, rv = Eval.rval state exp in
												let state = MemOp.state__end_fcall state in
												(* evaluate the assignment in the caller frame *)
												let state, lval = Eval.lval state dest in
												MemOp.state__assign state lval rv
										| _, _ ->
												(* If we are not returning a value, or if we
													 ignore the result, just end the call *)
												MemOp.state__end_fcall state
								in
								let callingFuncName = (List.hd state2.callstack).svar.vname in
								(* Update the state, stmt, exHist, and whether or not
									 we're in a tracked function *)
								let job' = { job with
									state = state2;
									stmt = nextStmt;
									inTrackedFn = StringSet.mem callingFuncName run_args.arg_fns;
								} in
								(* When a function returns, we have to record the
									 coverage within the returning function and also the
									 *edge* in the calling function from the call
									 instruction's block to the next block. *)
								(* First, record coverage for the returning function: *)
								let exHist = nextExHist None in (* [None] because we don't currently track returns as edges for purposes of coverage *)
								Active { job' with exHist =
										(* Now record the edge in the calling function. We
											 can't use nextExHist because the edge we want
											 is 'from the past' rather than 'into the
											 future'; so we have to check whether we should
											 in fact record coverage. (We have to use job'
											 (not job) here and in a few lines.) *)
										if job'.inTrackedFn && run_args.arg_edge_coverage
										then { exHist with coveredEdges =
												EdgeSet.add
													({ siFuncName = callingFuncName; siStmt = callStmt; }, (* A call ends a block, so use callStmt directly *)
													 stmtInfo_of_job job')
													exHist.coveredEdges
												 }
										else exHist
											 }
						| (NoReturn _)::_ ->
								failwith "Return from @noreturn function"
						| [] ->
								(* there should always be a Runtime at the start of the list *)
								assert false
				end
		| Goto (stmtref, loc) ->
				Active { job with stmt = !stmtref; exHist = nextExHist (Some !stmtref); }
		| If (exp, block1, block2, loc) ->
				begin
				(* try a branch *)
					let try_branch state pcopt block =
						let nextState = match pcopt with
							| Some(pc) -> MemOp.state__add_path_condition state pc true
							| None -> state
						in
						let nextStmt = match stmt.succs with
								[succ] -> succ (* This happens for 'if (...);', with nothing on either branch *)
							| [succF;succT] -> (* The successors are in reverse order: false then true *)
									(* Just making sure I understand this correctly *)
									assert ((block1.bstmts = [] || List.hd block1.bstmts == succT) &&
														(block2.bstmts = [] || List.hd block2.bstmts == succF));
									if block == block1 then succT else succF
							| _ -> assert false (* Impossible: there must be 1 or 2 successors *)
						in
						(nextState, nextStmt)
					in
 
					let state, rv = Eval.rval state exp in
 
					Output.set_mode Output.MSG_GUARD;
					if(Output.need_print Output.MSG_GUARD) then
						begin
							Output.print_endline ("Check if the following holds:");
							Output.print_endline (To_string.bytes rv);
							Output.print_endline ("Under the path condition:");
							let pc_str = (Utility.print_list To_string.bytes state.path_condition " AND ") in
							Output.print_endline (if String.length pc_str = 0 then "(nil)" else pc_str);
						end;
 
					let state, truth = MemOp.eval_with_cache state state.path_condition rv in
 
					Output.set_mode Output.MSG_REG;
					if truth == True then
						begin
							Output.print_endline "True";
							let nextState,nextStmt = try_branch state None block1 in
							let job' = { job with state = nextState; stmt = nextStmt; } in
							Active { job' with exHist = nextExHist (Some nextStmt) ~whichBranch:true; }
						end
					else if truth == False then
						begin
							Output.print_endline "False";
							let nextState,nextStmt = try_branch state None block2 in
							let job' = { job with state = nextState; stmt = nextStmt; } in
							Active { job' with exHist = nextExHist (Some nextStmt) ~whichBranch:false; }
						end
					else
						begin
							Output.print_endline "Unknown\n";
							
							let nextStateT,nextStmtT = try_branch state (Some rv) block1 in
							let nextStateF,nextStmtF = try_branch state (Some (logicalNot rv)) block2 in

							let job' =
								(* TODO: move the below into an interceptor in PathMerging *)
								if run_args.arg_merge_paths then (
									(* Add in the new merge points *)
									{ job with mergePoints =
											List.fold_left (fun set s -> StmtInfoSet.add s set) job.mergePoints
												begin match Hashtbl.find_all ifToJoinPointsHash (stmtInfo_of_job job) with
													| [] ->
															begin match state.callContexts with
																| (Source (_,_,_,nextStmt))::_ ->
																		Output.print_endline "Will merge upon function return";
																		(* nextStmt is in the calling function, not the current function,
																			 so we call (nth _ 1). *)
																		[{ siFuncName = (List.nth state.callstack 1).svar.vname ; siStmt = nextStmt }]
																| _ ->
																		[]
															end
													| x -> x
												end; }
								) else ( (* We aren't merging paths, so job' is just job *)
									job
								)
							in
							(* Create two jobs, one for each branch. Since we
								 continue executing the false branch immediately, let
								 that job inherit the old jid. Give the true job a new
								 jid. *)
							let trueJob = { job' with
																state = nextStateT;
																stmt = nextStmtT;
																exHist = nextExHist (Some nextStmtT) ~whichBranch:true;
                                                                (*parent = Some job';
                                                                 *)
														 		jid = Utility.next_jid; } in
							let falseJob = { job' with
																 state = nextStateF;
																 stmt = nextStmtF;
                                                                 (* parent = Some job';
                                                                  *)
																 exHist =  nextExHist (Some nextStmtF) ~whichBranch:false; } in
							Output.set_mode Output.MSG_MUSTPRINT;
							Output.printf "Branching on %s at %s. %s\nJob %d is the true branch and job %d is the false branch.\n\n"
								 (To_string.exp exp)
								 (To_string.location loc)
								 (if Executeargs.print_args.arg_print_callstack then
									 "Call stack:\n"^
									(To_string.callstack state.callContexts)
								 else ""
								 )
								 trueJob.jid falseJob.jid;
							Big_Fork [Active trueJob; Active falseJob]
						end
				end
		| Block(block)
		| Loop (block, _, _, _) ->
				(* A [Loop]'s block always has a non-empty bstmts. (See
					 Cil.succpred_stmt.)
					 This is not true for [Block]s, but it *does* seem to be
					 true for [Block]s which are not under [If]s, so we're okay. *)
				let nextStmt = List.hd block.bstmts in
				Active { job with stmt = nextStmt; exHist = nextExHist (Some nextStmt); }
		| _ -> failwith "Not implemented yet"


let step job job_queue =
	try
		match job.instrList with
			| [] -> (exec_stmt job, job_queue)
			| _ -> (exec_instr job, job_queue)
	with
		| Failure msg ->
			if run_args.arg_failfast then failwith msg;
			let result = { result_state = job.state; result_history = job.exHist } in
			(Complete (Types.Abandoned (msg, get_job_loc job, result)), job_queue)
