open Cil
open Ternary
open Bytes
open BytesUtility
open Types
open Executeargs
open Cilutility
open Utility

(*TODO: eventually this function won't be needed in Driver *)
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

let exec_symbolic state lvalopt exps exHist nextExHist =
(* There are 2 ways to use __SYMBOLIC:
	 (1) '__SYMBOLIC(&x);' gives x a fresh symbolic value and associates
	 that value with the variable x.
	 (2) 'x = __SYMBOLIC(n);' assigns to x a fresh symbolic value which
	 is not associated to any variable. If n > 0, n is the number of
	 symbolic bytes desired; if n <= 0, a number of symbolic bytes equal
	 to the size of x is returned. (If you manage to get something like
	 'x = __SYMBOLIC();' past CIL despite the disagreement in the number
	 of arguments, this behaves like the n <= 0 case.) *)
	begin match exps with
		| [AddrOf (Var varinf, NoOffset as cil_lval)]
		| [CastE (_, AddrOf (Var varinf, NoOffset as cil_lval))] ->
			(* If we are given a variable's address, we track the symbolic value.
				 But make sure we don't give the same variable two different values. *)
			if List.exists (fun (_,v) -> v == varinf) exHist.bytesToVars
			then Errormsg.s
				(Errormsg.error "Can't assign two tracked values to variable %s" varinf.vname);

			let state, (_, size as lval) = Eval.lval state cil_lval in
			let symbBytes = bytes__symbolic size in
			Output.set_mode Output.MSG_MUSTPRINT;
			Output.print_endline (varinf.vname ^ " = " ^ (To_string.bytes symbBytes));
			(* TODO: do something like this for
			 * argument initialization when start SE
			 * in the middle 
			 *)
			nextExHist := { exHist with bytesToVars = (symbBytes,varinf) :: exHist.bytesToVars; };
			MemOp.state__assign state lval symbBytes
		| _ ->
			(* Any symbolic value not directly given to a variable by a call to
				 __SYMBOLIC(&<var>) does not get tracked. *)
			begin match lvalopt with
				| None -> failwith "Incorrect usage of __SYMBOLIC(): symbolic value generated and ignored"
				| Some lval ->
					let state, (_, size as lval) = Eval.lval state lval in
					let state, ssize = match exps with
						| [] -> (state, size)
						| [CastE (_, h)] | [h] ->
							let state, bytes = Eval.rval state h in
							let newsize = bytes_to_int_auto bytes in
							(state, if newsize <= 0 then size else newsize)
						| _ -> failwith "__SYMBOLIC takes at most one argument"
					in
					MemOp.state__assign state lval (bytes__symbolic ssize)
			end
	end

let exec_aspect state instr exps advice =
	let state, argvs = 
		List.fold_right 
			begin fun exp (state, argvs) ->
				let state, bytes = Eval.rval state exp in
				(state, bytes::argvs)
			end exps (state, [])
	in
	advice state argvs instr

let exec_current_state state exps =
	let state, bytes = Eval.rval state (List.hd exps) in
	let key = bytes_to_int_auto bytes in
	Output.set_mode Output.MSG_MUSTPRINT;
	Output.printf "Record state %d\n" key;
	MemOp.index_to_state__add key state;
	state

let exec_print_state state =
	Output.set_mode Output.MSG_MUSTPRINT;
	let arg_print_char_as_int = Executeargs.print_args.Executeargs.arg_print_char_as_int in
	Executeargs.print_args.Executeargs.arg_print_char_as_int <- true;
	let module BOSMap = Utility.MakeMap (
		struct
		type t = memory_block * bytes * int  (* block, offset, size *)
		let compare = Pervasives.compare				
		end)	
	in
	let bosmap = ref BOSMap.empty in
	let printStringString s1 s2 =
		Output.print_endline (s1 ^ " = " ^ s2)
	in
	let rec printVarFieldsBytes varname typ bytes off =
		(* break down each local by its fields *)
		(* canonicalize concrete values by their array rep*)
		match typ with
			| TComp (compinfo,_) -> 
				List.iter 
					(fun fieldinfo -> 
						printVarFieldsBytes 
							(varname^"."^fieldinfo.fname) 
							fieldinfo.ftype bytes 
							(off + fst(Cil.bitsOffset typ (Field(fieldinfo,NoOffset)))/8)
					)
				  compinfo.cfields
			| _ -> 
				let size = (Cil.bitsSizeOf typ/8) in
				let rec p b = 
					match b with
						| Bytes_Constant const ->  p (constant_to_bytes const)
						| Bytes_ByteArray ba -> To_string.bytes (Bytes_ByteArray(ImmutableArray.sub ba off size))
						| Bytes_Address (block, boff) -> 
							if off = 0 && size = (bitsSizeOf voidPtrType / 8) then begin
								bosmap := BOSMap.add (block,boff,size) None (!bosmap);
								To_string.bytes b
							end else
								failwith (Printf.sprintf "PRINT STATE: Reading part of a Bytes_Address: %s %d %d" (To_string.bytes b) off size)
						| _ -> "("^(To_string.bytes b)^","^(string_of_int off)^","^(string_of_int size)^")"
				in 
				let rhs = p bytes in 
				printStringString varname rhs
	in
	let printVarBytes var bytes =
		printVarFieldsBytes var.vname var.vtype bytes 0 
	in
	let printVar var lval_block =
		if Cilutility.isConstType var.vtype then () 
		else
			match lval_block with
				| Immediate (Unconditional (block, _)) ->
					begin match (MemoryBlockMap.find block state.block_to_bytes) with
						| Immediate bytes -> printVarBytes var bytes
						| Deferred _ -> printStringString var.vname "(deferred)"
					end
				(* TODO: print something useful *)
				| Immediate (IfThenElse _) ->
					printStringString var.vname "(IfThenElse)"
				| Immediate (ConditionalException _) ->
					printStringString var.vname "(ConditionalException)"
				| Deferred _ ->
					printStringString var.vname "(deferred)"
	in
	
	Output.print_endline "#BEGIN PRINTSTATE";
	Output.print_endline "#Globals:";
	VarinfoMap.iter printVar state.global;
	Output.print_endline "#Locals:";
	VarinfoMap.iter printVar (List.hd state.locals);
	Output.print_endline "#Formals:";
	VarinfoMap.iter printVar (List.hd state.formals);
	(* explore only one level of memory *)
	bosmap := BOSMap.mapi begin fun (block,off,size) des -> match des with
		| Some _ -> des
		| None -> Some (snd(MemOp.state__deref state (conditional__lval_block (block, off), size)))
	end (!bosmap);
	Output.print_endline "#Memory: (one level)";
	BOSMap.iter (fun (block,off,size) des -> 
		let sdes =
			match des with 
				| None -> "None"
				| Some b -> To_string.bytes b
		in
		Output.print_endline ((To_string.bytes (Bytes_Address(block, off))) ^ " -> " ^ sdes)
	)
	(!bosmap);
	Output.print_endline "#END PRINTSTATE";
	Executeargs.print_args.Executeargs.arg_print_char_as_int <- arg_print_char_as_int;
	state

let exec_compare_state state exps =
	let state, bytes0 = Eval.rval state (List.nth exps 0) in
	let state, bytes1 = Eval.rval state (List.nth exps 1) in
	let key0 = bytes_to_int_auto bytes0 in
	let key1 = bytes_to_int_auto bytes1 in
	Output.set_mode Output.MSG_MUSTPRINT;
	Output.printf "Compare states %d and %d\n" key0 key1;
	begin try
		let s0 = try MemOp.index_to_state__get key0 
		with Not_found -> (
		   	Output.set_mode Output.MSG_MUSTPRINT;
		   	Output.printf "Warning: snapshot %d is absent\n" key0;
			raise Not_found
		)
		in
		let s1 = try MemOp.index_to_state__get key1
			with Not_found -> (
			Output.set_mode Output.MSG_MUSTPRINT;
			Output.printf "Warning: snapshot %d is absent\n" key1;
			raise Not_found
		)
		in
		Output.set_mode Output.MSG_MUSTPRINT;
		ignore (MemOp.cmp_states s0 s1);
		state
	with Not_found -> 
	   	Output.set_mode Output.MSG_MUSTPRINT;
	   	Output.printf "Compare states fail\n";
		state
	end

let exec_assert_equal_state state exps =
	let state, bytes0 = Eval.rval state (List.nth exps 0) in
	let key0 = bytes_to_int_auto bytes0 in
	begin try 
		let s0 = MemOp.index_to_state__get key0 in
		Output.set_mode Output.MSG_MUSTPRINT;
		if MemOp.cmp_states s0 state then
			Output.print_endline "AssertEqualState satisfied";
			MemOp.index_to_state__add key0 state; 
			state
	with Not_found -> 
		MemOp.index_to_state__add key0 state; 
		state
	end

let exec_func state func job instr lvalopt exps loc op_exps = 
	let exHist,stmt = job.exHist,job.stmt in
	begin match func with
		| Function.Ordinary (fundec) ->					
					(* TODO: do a casting if necessary: look at fundec.sformals, varinfo.vtype *)
					(* exps may be longer than fundec.sformals *)
					(* goal: create a list of targetted types:
						for regular arguments - look at fundec.sformals
						for vargs - same as the expressions *)
						(* NECESSARY? *)
						(*
						let rec exptyps_f lefts rights = match lefts,rights with
							| vh::vt,ah::at -> vh.vtype::(exptyps_f vt at)
							| [],ah::at -> (Cil.typeOf ah)::(exptyps_f [] at)
							| vh::vt,[] -> failwith "unreachable"
							| [],[] -> []
						in
						let exptyps = exptyps_f fundec.sformals exps in
						let argvs = (List.map2 (fun exp typ -> 
							let targetted_typ = Cil.typeOf exp in
							let final_exp = if targetted_typ == typ 
								then exp 
								else exp (*CastE(targetted_typ,exp) *)
							in
							Eval.rval state final_exp) exps exptyps) 
						*)
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
			(* If fundec is the function to be examined *)
			begin
				let examfn = Executeargs.run_args.arg_examfn in
					if examfn = fundec.svar.vname  then
            failwith "YAML not supported"
						(*InvInput.examine state fundec*)
					else ()
			end;

			(* Update the state, the next stmt to execute, and whether or
				 not we're in a tracked function. *)
			Active { job with
						state = state;
						stmt = List.hd fundec.sallstmts;
						inTrackedFn = StringSet.mem fundec.svar.vname run_args.arg_fns; }
		| _ ->
				let nextExHist = ref exHist in
				let state_end = begin match func with
					| Function.Symbolic -> exec_symbolic state lvalopt exps exHist nextExHist	
					| Function.Aspect(pointcut, advice) -> exec_aspect state instr exps advice
					| Function.CurrentState -> exec_current_state state exps
					| Function.PrintState -> exec_print_state state
					| Function.CompareState -> exec_compare_state state exps
					| Function.AssertEqualState -> exec_assert_equal_state state exps
					| _ -> failwith "unreachable exec_instr_call"
						
				end in (* inner [match func] *)

				let nextStmt =
					(* [stmt] is an [Instr] which doesn't end with a call to a
						 [noreturn] function, so it has exactly one successor. *)
					match stmt.succs with
							[h] -> h
						| _ -> assert false
				in

				(* We didn't add the outgoing edge in exec_stmt because the
					 call might have never returned. Since there isn't an
					 explicit return (because we handle the call internally), we
					 have to add the edge now. *)
				if job.inTrackedFn && run_args.arg_edge_coverage then
					nextExHist := { !nextExHist with coveredEdges =
							EdgeSet.add (stmtInfo_of_job job,
													 { siFuncName = (List.hd job.state.callstack).svar.vname;
														 siStmt = Cilutility.stmtAtEndOfBlock nextStmt; })
							!nextExHist.coveredEdges; };

				(* Update state, the stmt to execute, and exHist (which may
					 have gotten an extra bytesToVar mapping added to it). *)
				Active { job with state = state_end; stmt = nextStmt; exHist = !nextExHist; }

	end (* outer [match func] *)

let exec_instr_call job instr lvalopt fexp exps loc =
	let state,exHist,stmt = job.state,job.exHist,job.stmt in

	let op_exps state exps binop =
		let rec impl exps =
			match exps with
				| [] -> failwith "AND/OR must take at least 1 argument"
				| h::[] -> h
				| h:: tail -> let t = impl tail in BinOp(binop, h, t, Cil.intType)
		in
		Eval.rval state (impl exps)
	in
	let rec process_func_list func_list =
		match func_list with
			| [] -> []
			| (state, func)::t -> 
				let job_state =
					try
						(exec_func state func job instr lvalopt exps loc op_exps)
					with Failure msg ->
						if run_args.arg_failfast then failwith msg;
						let result = { result_state = job.state; result_history = job.exHist } in
						Complete (Types.Abandoned (msg, loc, result))
				in
				job_state::(process_func_list t)
	in
	let f = (process_func_list (Function.from_exp state fexp exps)) in
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
			exec_instr_call job instr lvalopt fexp exps loc
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


(*
let printJob (state,stmt) =
	Output.printf "path condition = %s\n" (To_string.bytes_list state.path_condition);
	Output.printf "statement:\n%s\n"
		(try Pretty.sprint 100 (Cil.d_stmt () stmt)
		 with Errormsg.Error -> "<error printing statement>")
*)


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

let otter_core_interceptor job job_queue =
	try
		match job.instrList with
			| [] -> (exec_stmt job, job_queue)
			| _ -> (exec_instr job, job_queue)
	with
		| Failure msg ->
			if run_args.arg_failfast then failwith msg;
			let result = { result_state = job.state; result_history = job.exHist } in
			(Complete (Types.Abandoned (msg, get_job_loc job, result)), job_queue)

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
			otter_core_interceptor job job_queue 
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
			otter_core_interceptor
		)
		process_result
		[job]

