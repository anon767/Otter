open Cil
open Types
open Executeargs


let eval_with_cache state pc bytes =
    (Stp.eval pc bytes,state) 
  (*
  match MemOp.state__get_bytes_eval_cache state bytes with
    | Some (boolval) -> ((if boolval then Stp.True else Stp.False), state)
    | None ->
        let truth = Stp.eval pc bytes in
          if truth = Stp.True then (truth,MemOp.state__add_bytes_eval_cache state bytes true)
          else if truth = Stp.False then (truth,MemOp.state__add_bytes_eval_cache state bytes false)
          else (truth,state)
   *)

(** Remove a NOT from a bytes, or add one. The type of the bytes may
		be lost. *)
let logicalNegateBytes = function
		Bytes_Op(OP_LNOT,[bytes,_]) -> bytes
	| bytes -> make_Bytes_Op(OP_LNOT,[(bytes, Cil.intType)])

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
							Some nextStmt ->
								if job.stmt == Cilutility.stmtAtEndOfBlock job.stmt
								then
									let funcName = (List.hd job.state.callstack).svar.vname in
									EdgeSet.add
										({ siFuncName = funcName; siStmt = job.stmt; },
										 { siFuncName = funcName;
											 siStmt = Cilutility.stmtAtEndOfBlock nextStmt; })
										job.exHist.coveredEdges
								else job.exHist.coveredEdges
						| _ -> job.exHist.coveredEdges
				) else EdgeSet.empty;
			coveredConds =
                                if run_args.arg_cond_coverage
                                then (
                                        match job.stmt.skind with
                                                If _ -> CondSet.add (stmtInfo_of_job job,whichBranch) job.exHist.coveredConds
                                        | _ -> job.exHist.coveredConds
                                ) else CondSet.empty;
	}

let addInstrCoverage job instr =
	let instrLoc = get_instrLoc instr in
	{ job.exHist with coveredLines =
			LineSet.add (instrLoc.file,instrLoc.line) job.exHist.coveredLines; }

let exec_instr_call job instr blkOffSizeOpt fexp exps loc =
	let state,exHist,stmt = job.state,job.exHist,job.stmt in

	let op_exps exps binop =
		let rec impl exps =
			match exps with
				| [] -> failwith "AND/OR must take at least 1 argument"
				| h::[] -> h
				| h:: tail -> let t = impl tail in BinOp(binop, h, t, Cil.voidType)
		in
		Eval.rval state (impl exps)
	in

	let func = Function.from_exp state fexp exps in
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
				let argvs = (List.map (fun exp -> Eval.rval state exp) exps) in
				(* [stmt] is an [Instr], so it can't have two successors. If
					 [func] returns, then [stmt] has exactly one successor. If
					 [func] is [exit] or has the [noreturn] attribute, [stmt]
					 has no successor. *)
				let callContext = match stmt.succs with
					| []  -> NoReturn instr
					| [h] -> Source (blkOffSizeOpt,stmt,instr,h)
					| _   -> assert false
				in
				let state = MemOp.state__start_fcall state callContext fundec argvs in
				(* Update the state, the next stmt to execute, and whether or
					 not we're in a tracked function. *)
				Active { job with
									 state = state;
									 stmt = List.hd fundec.sallstmts;
									 inTrackedFn = StringSet.mem fundec.svar.vname run_args.arg_fns; }
		| _ ->
				try (
					let nextExHist = ref exHist in
				let state_end = begin match func with
					| Function.Builtin (builtin) ->
						let (state2,bytes) = builtin state exps in
						begin
							match blkOffSizeOpt with
								| None ->
									state2
								| Some dest ->
									MemOp.state__assign state2 dest bytes
						end

(*
                    | Function.DataStructureOp (dsop) -> 
                        dsop state blkOffSizeOpt exps
*)

                    | Function.StringEqual -> 
                        (* The function evaluates to a (symbolic) integer value.
                         * 1 - Equal
                         * 0 - Not equal
                         * symbolic - depends *)

                        (* Maybe instead write a function that returns whether
                         * an expression is true, false or unknown
                         *)
                        
                        
                        state

                    | Function.Clone ->
                        if List.length exps <> 3 then failwith "Clone takes 3 arguments" else
				        let argvs = (List.map (fun exp -> Eval.rval state exp) exps) in
                        let target_ptr_bytes = List.nth argvs 0 in
                        let source_ptr_bytes = List.nth argvs 1 in
                        let length_int_bytes = List.nth argvs 2 in
                        begin
                        match target_ptr_bytes,source_ptr_bytes,length_int_bytes with
                          | Bytes_Address(Some(target_block),target_offset),
                            Bytes_Address(Some(source_block),source_offset),
                            Bytes_Constant(_)
                            ->
                              begin
                              let length_int_val = Convert.bytes_to_int_auto length_int_bytes in
                              let source_bytes = MemOp.state__get_bytes_from_lval state (source_block,source_offset,length_int_val) in
                              let (state2,cloned_bytes) = MemOp.state__clone_bytes state source_bytes in
                              let state3 = MemOp.state__assign state2 (Lval_Block (target_block, target_offset),length_int_val) cloned_bytes in
                                state3
                              end
                          | _ -> failwith "Clone error"
                       end

                    | Function.Given -> 
						begin match blkOffSizeOpt with
							| None -> state 
							| Some dest ->
                                let truthvalue = 
                                  begin
                                  if List.length exps <> 2 then 
                                    failwith "__GIVEN takes 2 arguments"
                                  else
                                  let given = Eval.rval state (List.nth exps 0) in
				                  let rv = Eval.rval state (List.nth exps 1 ) in
				                  let (truth,state) = eval_with_cache state (given::state.path_condition) rv in
				                  if truth == Stp.True then Convert.lazy_int_to_bytes 1 
				                  else if truth == Stp.False then Convert.lazy_int_to_bytes 0
				                  else MemOp.bytes__symbolic 4
                                  end
                                in
								MemOp.state__assign state dest truthvalue
						end
                    | Function.TruthValue -> 
						begin match blkOffSizeOpt with
							| None -> state 
							| Some dest ->
                                let truthvalue = 
                                  Convert.lazy_int_to_bytes
                                  begin
                                  if List.length exps = 0 then 0 else
				                  let rv = Eval.rval state (List.hd exps) in
				                  let (truth,state) = eval_with_cache state state.path_condition rv in
				                  if truth == Stp.True then 1
				                  else if truth == Stp.False then -1
				                  else 0
                                  end
                                in
								MemOp.state__assign state dest truthvalue
						end

					| Function.Symbolic -> (
(* There are 2 ways to use __SYMBOLIC:
	 (1) '__SYMBOLIC(&x);' gives x a fresh symbolic value and associates
	 that value with the variable x.
	 (2) 'x = __SYMBOLIC(n);' assigns to x a fresh symbolic value which
	 is not associated to any variable. If n > 0, n is the number of
	 symbolic bytes desired; if n <= 0, a number of symbolic bytes equal
	 to the size of x is returned. (If you manage to get something like
	 'x = __SYMBOLIC();' past CIL despite the disagreement in the number
	 of arguments, this behaves like the n <= 0 case.) *)
							match exps with
								| [AddrOf (Var varinf, NoOffset as lval)]
								| [CastE (_, AddrOf (Var varinf, NoOffset as lval))] ->
										(* If we are given a variable's address, we track the symbolic value.
											 But make sure we don't give the same variable two different values. *)
										if List.exists (fun (_,v) -> v == varinf) exHist.bytesToVars
										then Errormsg.s
											(Errormsg.error "Can't assign two tracked values to variable %s" varinf.vname);

										let size = (Cil.bitsSizeOf (Cil.typeOfLval lval))/8 in
										let lvals = Eval.lval state lval in
										let symbBytes = (MemOp.bytes__symbolic size ) in
										Output.set_mode Output.MSG_MUSTPRINT;
										Output.print_endline (varinf.vname ^ " = " ^ (To_string.bytes symbBytes));
										nextExHist := { exHist with bytesToVars = (symbBytes,varinf) :: exHist.bytesToVars; };
										MemOp.state__assign state (lvals,size) symbBytes
								| _ ->
										(* Any symbolic value not directly given to a variable by a call to
											 __SYMBOLIC(&<var>) does not get tracked. *)
										begin match blkOffSizeOpt with
											| None ->
													state
											| Some (lvals, size) ->
													let ssize = match exps with
														| [] -> size
														| [CastE (_, h)] | [h] ->
																let newsize = Convert.bytes_to_int_auto (Eval.rval state h) in
																if newsize <= 0 then size else newsize
														| _ -> failwith "__SYMBOLIC takes at most one argument"
													in
													MemOp.state__assign state (lvals, size (*ssize?*))
														(MemOp.bytes__symbolic ssize )
										end
						)

					| Function.SymbolicState ->
                        {state with
                             block_to_bytes = MemoryBlockMap.map 
                                (fun b -> match b with
                                     Bytes_FunPtr(_) -> b
                                   (* TODO: handle pointers by generating MayBytes trees based on alias analysis *)
                                   | _ -> MemOp.bytes__symbolic (MemOp.bytes__length b)
                                ) 
                                state.block_to_bytes;
                        }
					| Function.SymbolicStatic ->
							begin match blkOffSizeOpt with
								| None -> 
									state
								| Some (lvals, size as dest) ->
									let key = 
										if List.length exps == 0 then 0 else
										let size_bytes = Eval.rval state (List.hd exps) in
												Convert.bytes_to_int_auto size_bytes 
									in
									let state2 = if  MemOp.loc_table__has state (loc,key) then state
										else MemOp.loc_table__add state (loc,key) (MemOp.bytes__symbolic size)
									in
									let newbytes = MemOp.loc_table__get state2 (loc,key) in
										MemOp.state__assign state2 dest newbytes
							end												
(*
					| Function.Fresh ->
							begin match blkOffSizeOpt with
								| None -> 
									state
								| Some (block,offset,_) ->
									let id = Convert.bytes_to_int_auto (Eval.rval state (List.hd exps)) in
									let size = 1 in
										MemOp.state__assign state (block,offset,size) (MemOp.bytes__of_list [(MemOp.byte__symbolic_with_id id true)])
							end				
*)												
					| Function.NotFound ->
							begin
								match blkOffSizeOpt with
								| None -> 
										state
								| Some (lvals, size as dest) ->
										MemOp.state__assign state dest (MemOp.bytes__symbolic size)
							end
						
					| Function.Exit ->
						let exit_code = match exps with
							| exp1::_ -> Some (Eval.rval state exp1)
							| [] -> None
						in
						raise (Function.Notification_Exit (exit_code))
					
					| Function.Evaluate ->
						let pc = op_exps exps Cil.LAnd in
							Output.set_mode Output.MSG_MUSTPRINT;
							Output.print_endline ("    Evaluates to "^(To_string.bytes pc));
							state
							
					| Function.EvaluateString ->
							let exp = List.hd exps in
							let sizeexp = List.nth exps 1 in
							let str = match Eval.rval state exp with
								| Bytes_Address(Some(block),offset) -> 
									let size = 
                                      try Convert.bytes_to_int_auto (Eval.rval state sizeexp) with
                                          Failure(s) -> Output.print_endline s; 32
                                    in
									let bytes = MemOp.state__get_bytes_from_lval state (block,offset,size) in
									begin match bytes with
										| Bytes_ByteArray(bytearray) -> To_string.bytestring bytearray
										| Bytes_Constant(CInt64(i,_,_)) -> Int64.to_string i
										| _ -> "(complicate)"
									end
								| _ -> "(nil)"
							in
							Output.set_mode Output.MSG_MUSTPRINT;
							Output.print_endline ("Evaluates to string: \"" ^ (
                                if
                                    Executeargs.print_args.arg_print_no_escaped_string
                                then
                                    str
                                else
                                    String.escaped str
                                ) ^ "\"");
							state
												
					| Function.Assume ->
						let pc = op_exps exps Cil.LAnd in
							MemOp.state__add_path_condition state pc false
					
					| Function.PathCondition ->
						let pc_str = (Utility.print_list To_string.bytes state.path_condition " AND ") in
						Output.set_mode Output.MSG_MUSTPRINT;
						Output.print_endline (if String.length pc_str = 0 then "(nil)" else pc_str);
							state
															
					| Function.Assert -> 
						let post = op_exps exps Cil.LAnd in
							let (truth,state) = eval_with_cache state state.path_condition post in
							begin
								if truth == Stp.True then
									begin
									Output.set_mode Output.MSG_REG;
									Output.print_endline "Assertion satisfied."
									end
								else
									begin
									Output.set_mode Output.MSG_MUSTPRINT;
									Output.print_endline "Assertion not-satisfied (see error log).";
									let oldPrintNothingVal = print_args.arg_print_nothing in
									print_args.arg_print_nothing <- false; (* Allow printing for the log *)
									Executedebug.log "\n(****************************";
									Executedebug.log "Assertion:";
									Executedebug.log (Utility.print_list To_string.exp exps " and "); 
									Executedebug.log "which becomes";
									Executedebug.log (To_string.bytes post);
									Executedebug.log "can be false with the path condition:";
									(*let pc_str = To_string.humanReadablePc state.path_condition exHist.bytesToVars in *)
									(*let pc_str = (Utility.print_list To_string.bytes state.path_condition " AND ") in*)
									(*let pc_str = Utility.print_list To_string.bytes (Stp.getRelevantAssumptions state.path_condition post) " AND " in*)
									let pc_str = (Utility.print_list To_string.bytes state.path_condition " AND ") in
									Executedebug.log (if String.length pc_str = 0 then "(nil)" else pc_str);
									Executedebug.log "****************************)";
									print_args.arg_print_nothing <- oldPrintNothingVal;
									()
									end									
									
							end;
							state
												
					| Function.BooleanOp (binop) ->
							begin match blkOffSizeOpt with
								| None -> failwith "Unreachable BooleanOp"
								| Some dest ->
									let rv= op_exps exps binop in 
									MemOp.state__assign state dest rv
							end

					| Function.BooleanNot ->
							begin match blkOffSizeOpt with
								| None -> failwith "Unreachable BooleanNot"
								| Some dest ->
									let rv = Eval.rval state (UnOp(Cil.LNot, List.hd exps, Cil.voidType)) in
										MemOp.state__assign state dest rv
							end

					| Function.Aspect(pointcut, advice) ->
						let argvs = List.map (fun exp -> Eval.rval state exp) exps in
							advice state argvs instr
							
					| Function.BreakPt ->
						Output.set_mode Output.MSG_REG;
						Output.print_endline "Option (h for help):";
						Scanf.scanf "%d\n" 
						begin
							fun p1->
								Output.printf "sth\n";	
							state
						end
							
					| Function.Comment ->
						let exp = List.hd exps in
							Output.set_mode Output.MSG_MUSTPRINT;
							Output.print_endline ("COMMENT:"^(To_string.exp exp));
							state
					| Function.CurrentState ->
						let bytes = Eval.rval state (List.hd exps) in
						let key = Convert.bytes_to_int_auto bytes in
							Output.set_mode Output.MSG_MUSTPRINT;
							Output.printf "Record state %d\n" key;
							MemOp.index_to_state__add key state;
							state
													
					| Function.CompareState ->
						let bytes0 = Eval.rval state (List.nth exps 0) in
						let bytes1 = Eval.rval state (List.nth exps 1) in
						let key0 = Convert.bytes_to_int_auto bytes0 in
						let key1 = Convert.bytes_to_int_auto bytes1 in
						Output.set_mode Output.MSG_MUSTPRINT;
						Output.printf "Compare states %d and %d\n" key0 key1;
						begin try
						  let s0 = try MemOp.index_to_state__get key0 
						  with Not_found -> (
						   	Output.set_mode Output.MSG_MUSTPRINT;
						   	Output.printf "Warning: snapshot %d is absent\n" key0;
						   	    raise Not_found )
                          in
						  let s1 = try MemOp.index_to_state__get key1
						  with Not_found -> (
						   	Output.set_mode Output.MSG_MUSTPRINT;
						   	Output.printf "Warning: snapshot %d is absent\n" key1;
						   	    raise Not_found )
                          in
						    let output = MemOp.cmp_states s0 s1 in
						    	Output.set_mode Output.MSG_MUSTPRINT;
						    	Output.print_endline output;			
						    	state
                        with Not_found -> 
						   	Output.set_mode Output.MSG_MUSTPRINT;
						   	Output.printf "Compare states fail\n";
                            state
                        end
					| Function.AssertEqualState ->
						let bytes0 = Eval.rval state (List.nth exps 0) in
						let key0 = Convert.bytes_to_int_auto bytes0 in
						begin try 
							let s0 = MemOp.index_to_state__get key0 in
							Output.set_mode Output.MSG_MUSTPRINT;
							let output = MemOp.cmp_states s0 state in
							let output = if String.length output = 0 then "AssertEqualState satisfied" else output in
						    	Output.set_mode Output.MSG_MUSTPRINT;
						    	Output.print_endline output;			
						    	MemOp.index_to_state__add key0 state; 
									state
						with Not_found -> 
							MemOp.index_to_state__add key0 state; 
							state
						end
						
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

				) with Function.Notification_Exit exit_code ->
					(* If it was [Exit], there is no job to return *)
					Output.set_mode Output.MSG_MUSTPRINT;
					Output.print_endline ("exit() called with code "^(
                        match exit_code with None-> "(NONE)" | Some(code) -> To_string.bytes code));
(*
					if run_args.arg_line_coverage then (
						Report.printPath state exHist;
						Report.printLines exHist.coveredLines
					);
					Output.set_mode Output.MSG_REG;
					Output.print_endline
						("Path condition:\n" ^
							 (To_string.humanReadablePc state.path_condition exHist.bytesToVars));
*)
					Complete (Types.Exit (exit_code, { result_state = state; result_history = exHist; }))

	end (* outer [match func] *)
;;

let exec_instr job =
	assert (job.instrList <> []);
	let printInstr instr =
		Output.set_mode Output.MSG_STMT;
		Output.set_cur_loc (Cil.get_instrLoc instr);
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
		| Set(lval,exp,loc) ->
			printInstr instr;
			let lvals = Eval.lval job.state lval in
			let size = (Cil.bitsSizeOf (Cil.typeOfLval lval))/8 in
			let rv = Eval.rval job.state exp in
			let state = MemOp.state__assign job.state (lvals,size) rv in
			let nextStmt = if tail = [] then List.hd job.stmt.succs else job.stmt in
			Active { job with state = state; stmt = nextStmt }
		| Call(lvalopt, fexp, exps, loc) ->
			assert (tail = []);
			printInstr instr;
			let destOpt =
				match lvalopt with
					| None -> None
					| Some lval ->
							let lvals = Eval.lval job.state lval in
							let size = (Cil.bitsSizeOf (Cil.typeOfLval lval))/8 in
							Some (lvals,size)
			in
			exec_instr_call job instr destOpt fexp exps loc
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
	Output.set_cur_loc (Cil.get_stmtLoc stmt.skind);
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
								let retval = match expopt with
									| None -> None
									| Some exp -> Some (Eval.rval state exp)
								in
								let state = MemOp.state__end_fcall state in                                                                    
								Complete (Types.Return
									(retval, { result_state = state; result_history = nextExHist None; })) 
						| (Source (destOpt,callStmt,_,nextStmt))::_ ->
								let state2 =
									match expopt, destOpt with
										| Some exp, Some dest ->
												let rv = Eval.rval state exp in
												let state = MemOp.state__end_fcall state in
												MemOp.state__assign state dest rv
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
													 inTrackedFn =
										StringSet.mem callingFuncName run_args.arg_fns; }
								in
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
					let try_branch pcopt block =
						let nextState = match pcopt with
							| Some(pc) -> MemOp.state__add_path_condition state pc true
							| None -> state
						in
						if run_args.arg_edge_coverage && job.inTrackedFn then
							begin
								let which = (if block == block1 then fst else snd) in
								try
									let pcSet_ref = which (Hashtbl.find branches_taken (exp,loc)) in
									(* Add the path condition to the list of ways we take this branch *)
									pcSet_ref := PcHistSet.add (state.path_condition,job.exHist) !pcSet_ref
								with Not_found ->
									(* We haven't hit this conditional before. Initialize its entry in
										 the edge coverage table with the current path condition (and
										 an empty set for the other direction of the branch). *)
									Hashtbl.add
										branches_taken
										(exp,loc)
										(let res = (ref PcHistSet.empty, ref PcHistSet.empty) in
										which res := PcHistSet.singleton (state.path_condition,job.exHist);
										res)
							end;
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
 
					let rv = Eval.rval state exp in
 
					Output.set_mode Output.MSG_GUARD;
					if(Output.need_print Output.MSG_GUARD) then
						begin
							Output.print_endline ("Check if the following holds:");
							Output.print_endline (To_string.bytes rv);
							Output.print_endline ("Under the path condition:");
							let pc_str = (Utility.print_list To_string.bytes state.path_condition " AND ") in
							Output.print_endline (if String.length pc_str = 0 then "(nil)" else pc_str);
						end;
 
					let (truth,state) = eval_with_cache state state.path_condition rv in
 
					Output.set_mode Output.MSG_REG;
					if truth == Stp.True then
						begin
							Output.print_endline "True";
							let nextState,nextStmt = try_branch None block1 in
							let job' = { job with state = nextState; stmt = nextStmt; } in
							Active { job' with exHist = nextExHist (Some nextStmt) ~whichBranch:true; }
						end
					else if truth == Stp.False then
						begin
							Output.print_endline "False";
							let nextState,nextStmt = try_branch None block2 in
							let job' = { job with state = nextState; stmt = nextStmt; } in
							Active { job' with exHist = nextExHist (Some nextStmt) ~whichBranch:false; }
						end
					else
						begin
							Output.print_endline "Unknown\n";
							
							let nextStateT,nextStmtT = try_branch (Some rv) block1 in
							let nextStateF,nextStmtF = try_branch (Some (logicalNegateBytes rv)) block2 in

							let job' = 
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
														 		jid = Utility.next_id Output.jidCounter; } in
							let falseJob = { job' with
																 state = nextStateF;
																 stmt = nextStmtF;
																 exHist =  nextExHist (Some nextStmtF) ~whichBranch:false; } in
							Output.set_mode Output.MSG_MUSTPRINT;
							Output.printf "Branching on %s at %s. %s
Job %d is the true branch and job %d is the false branch.\n\n"
								 (To_string.exp exp)
								 (To_string.location loc)
                                 (if Executeargs.print_args.arg_print_callstack then
                                     "Call stack:\n"^
								    (To_string.callstack state.callContexts)
                                 else ""
                                 )
								 trueJob.jid falseJob.jid;
							Fork (trueJob, falseJob)	
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
;;

(*
let printJob (state,stmt) =
	Output.printf "path condition = %s\n" (To_string.bytes_list state.path_condition);
	Output.printf "statement:\n%s\n"
		(try Pretty.sprint 100 (Cil.d_stmt () stmt)
		 with Errormsg.Error -> "<error printing statement>")
*)

let cmp_memory blkToByt1 blkToByt2 =
	(* Split the blocks in s1 into those which are in s2 but map to
		 different bytes, those which map to the same bytes in s2, and
		 those which are not in s2 at all. *)
	let cmpSharedBlocks b2b1 b2b2 =
		let f block bytes1 (diffShared,sameShared,in1ButNot2) =
			try
				let bytes2 = MemoryBlockMap.find block b2b2 in
				if MemOp.diff_bytes bytes1 bytes2 then
					((block,bytes1,bytes2) :: diffShared, sameShared, in1ButNot2)
				else
					(diffShared, (block,bytes1) :: sameShared, in1ButNot2)
			with Not_found -> (diffShared, sameShared, (block,bytes1) :: in1ButNot2)
		in
		MemoryBlockMap.fold f b2b1 ([],[],[])
	in
	let sharedBlockDiffs,sharedBlockSames,blocksIn1ButNot2 =
		cmpSharedBlocks blkToByt1 blkToByt2 in

	(* List blocks (memory allocations) that are in s2 but not in s1 *)
	let cmpUnsharedBlocks b2b1 b2b2 =
	  let h b2b block bytes listOfDiffs =
	    if MemoryBlockMap.mem block b2b then listOfDiffs else
	      (block,bytes) :: listOfDiffs
	  in
	  MemoryBlockMap.fold (h b2b1) b2b2 []
	in
	let blocksIn2ButNot1 = cmpUnsharedBlocks blkToByt1 blkToByt2 in
	sharedBlockDiffs,sharedBlockSames,blocksIn1ButNot2,blocksIn2ButNot1

(** Find the common suffix of 2 lists l1 and l2, and return a triple
		(a,b,c) such that l1 = a @ c and l2 = b @ c. *)
let splitOutCommonSuffix l1 l2 =
	let rec helper a b acc =
		match a,b with
				[],_ -> [], List.rev b, acc
			| _,[] -> List.rev a, [], acc
			| h1::t1,h2::t2 ->
					if h1 == h2
					then helper t1 t2 (h1 :: acc)
					else List.rev a, List.rev b, acc
	in helper (List.rev l1) (List.rev l2) []
;;

(* Essentially, union for maps. However, anything bound in both x and
	 y in a call to [memoryMerge x y] will end up being mapped according
	 to x. *)
(*let memoryMerge : bytes MemoryBlockMap.t = MemoryBlockMap.fold MemoryBlockMap.add*)

(* Transform an association list into a memory block map, preserving
	 only the *last* binding for each key.
	 Preserving the *first* instead (which is more natural for
	 association lists) could be accomplished by reversing [lst] in the
	 first call to [helper].
	 I currently don't reverse [lst] because I currently only call this
	 function on values of [lst] which have only one binding per key. *)
let assocListToMemoryBlockMap lst =
	let rec helper acc = function
			[] -> acc
		| (a,b)::t ->
				helper (MemoryBlockMap.add a b acc) t
	in helper MemoryBlockMap.empty lst

let pcToBytes = function
		[] -> failwith "pcToAND"
	| [h] -> h
	| pc -> make_Bytes_Op(OP_LAND, List.map (fun b -> (b,Cil.intType)) pc)


let atSameProgramPoint job1 job2 =
	let state1 = job1.state
	and state2 = job2.state in
	(* I'm trying to be conservative here by using physical equality
		 because I haven't quite figured out what 'being at the same
		 program point' means. But I'm pretty sure that if these 3 things
		 are physically equal, then they really are at the same program
		 point, whatever that's supposed to mean. *)
	(* Actually, if the instrLists are equal, then the stmts have to be.
		 Right? So the stmt equality can be moved into the assertion. *)
	if job1.stmt == job2.stmt && job1.instrList == job2.instrList &&
		state1.callContexts == state2.callContexts
	then (
		assert
			(job1.exHist.bytesToVars == job2.exHist.bytesToVars &&
				 state1.global         == state2.global &&
			 	 state1.formals        == state2.formals &&
			 	 state1.locals         == state2.locals &&
				 state1.callstack      == state2.callstack &&
				 state1.va_arg         == state2.va_arg &&
				 state1.va_arg_map     == state2.va_arg_map &&
				 state1.loc_map        == state2.loc_map);
			true
		) else (
			false
		)


(* pick a job from the job_pool *)
let pick_job (job_queue, merge_set) =
	(* pick either from the job_queue, or from the merge_set *)
	match job_queue with
		| job::job_queue ->
			Some (job, (job_queue, merge_set))
		| [] ->
			try let job = JobSet.choose merge_set in
				let merge_set = JobSet.remove job merge_set in
				Some (job, ([], merge_set))
			with Not_found ->
				None

(* queue a job into the job_pool *)
let queue_job job (job_queue, merge_set) =
	(job::job_queue,  merge_set)


(* MergeDone includes the old job (which is swallowed by the current
	 job) and the state for the merged job. *)
exception MergeDone of job * state
exception TooDifferent


(* Merge job states. If we successfully merge job with a job j from merge_set,
	 return Complete(Truncated(job,j)). If job is at a merge point but we don't
	 merge, put job into the job_pool, pick a job j from job_pool, and return
	 Active(j). (This effectively suspends job. It will eventually be reactivated
	 when we run out of active jobs to run, or it will be merged with some other
	 job that reaches the same program point.) Otherwise, return Active(job). In
	 all cases, also return the updated job_pool. *)
let mergeJobs job ((job_queue, merge_set) as job_pool) =
	try
		(* First test to see if the job should pause and/or merge *)
		if run_args.arg_merge_paths &&
			StmtInfoSet.mem { siFuncName = (List.hd job.state.callstack).svar.vname;
											  siStmt = job.stmt; }
			job.mergePoints then begin
		JobSet.iter
			(fun j ->
				if atSameProgramPoint job j then begin
				 try
				 let jPC,jobPC,commonPC =
					 splitOutCommonSuffix j.state.path_condition job.state.path_condition
				 in
				 let jobPCBytes = pcToBytes jobPC and
						 jPCBytes = pcToBytes jPC in
				 let mergedPC =
					 match jPC,jobPC with
							 (* Optimize the common case of P \/ ~P *)
						 | [byts],[Bytes_Op(OP_LNOT,[(byts',_)])] when byts == byts' ->
								 commonPC
						 | [Bytes_Op(OP_LNOT,[(byts',_)])],[byts] when byts == byts' ->
								 commonPC
						 | _ ->
								 (* General case: either one condition or the other is
										true. We might sometimes be able to simplify the
										pc, but right now we don't try to do so. *)
								 make_Bytes_Op(OP_LOR, [(jPCBytes,Cil.intType);(jobPCBytes,Cil.intType)])
								 :: commonPC
				 in
				 (* I'm assuming [j.state.locals == job.state.locals] for now
						(due to the implementation of atSameProgramPoint),
						so the varinfos always map to the same blocks. This means I
						only need to mess with the [block_to_bytes]---I can leave
						[locals] alone. *)
				 let diffShared,sameShared,inJOnly,inJobOnly =
					 cmp_memory j.state.block_to_bytes job.state.block_to_bytes in
				 if diffShared = [] then (
					 (* The memory is identical in [j] and [job] *)
                     if not (inJOnly = [] && inJobOnly = []) then
                          failwith "there might be a memory leak"
                     else ();
					 Output.printf "Merging jobs %d and %d (identical memory)\n" j.jid job.jid;
					 raise (MergeDone (j, { job.state with path_condition = mergedPC; }))
				 ) else (
					 (* We have to fiddle with memory *)
					 (* TODO: do we still want to have a limit on number of blocks merged? Should benchmark the burden
                      *       on STP using the MayBytes/Morris encoding. *)
					 let indicator = MemOp.indicator__next () in
					 let numSymbolsCreated = ref 0 in
					 let newSharedBlocks =
						 (* Make a new symbolic bytes for each differing block *)
						 List.map
							 (fun (block,jBytes,jobBytes) ->
									let size = MemOp.bytes__length jBytes in (* or should I just check block.memory_block_size? *)
									if size <> MemOp.bytes__length jobBytes then (
										Output.printf "Unimplemented: merging bytes with different lengths\n";
										raise TooDifferent
									) else (
										numSymbolsCreated := !numSymbolsCreated + size;
										if !numSymbolsCreated > 100 then raise TooDifferent;
										let symbBytes = make_Bytes_MayBytes (indicator, jobBytes, jBytes) in
										(block, symbBytes)
									)
							 )
							 diffShared
					 in
					 let finalMergedPC = (make_Bytes_MayBytes (indicator, jobPCBytes, jPCBytes))::mergedPC
					 and mergedMemory =
						 (* I think it's safe (if not optimal) to keep both
								[inJOnly] and [inJobOnly] because [j]'s memory will be
								unreachable from [job]'s path and vice versa. *)
						 assocListToMemoryBlockMap
							 (List.concat [newSharedBlocks; sameShared; inJOnly; inJobOnly])
					 in
					 Output.printf "Merging jobs %d and %d (differing memory)\n" j.jid job.jid;
					 raise (MergeDone (j, { job.state with
																		block_to_bytes = mergedMemory;
																		path_condition = finalMergedPC;
																}))
				 )
				 with TooDifferent -> Output.print_endline "Memory too different to merge"; ()
				end
			) (* End iteration function *)
			merge_set;
			(* Reaching here means we've iterated through all jobs, never
			   being able to merge. So we just add the job to merge_set
			   and return another job, if available *)
			match pick_job job_pool with
				| Some (job', (job_queue, merge_set)) ->
					let merge_set = JobSet.add job merge_set in
					(Active job', (job_queue, merge_set))
				| None ->
					(Active job, job_pool)
		end else begin
			(* not at merge point *)
			(Active job, job_pool)
		end;
	with MergeDone (oldJob,mergedState) ->
		(* This is a pretty stupid way of getting complete coverage information with
			 merging, but it's something. It only credits a merged execution
			 with the parts that *both* jobs covered. Things covered by only
			 one side are attributed to truncated executions that end at the
			 join point. *)
		let jobOnlyLines = LineSet.diff job.exHist.coveredLines oldJob.exHist.coveredLines
		and oldJobOnlyLines = LineSet.diff oldJob.exHist.coveredLines job.exHist.coveredLines
		and jobOnlyBlocks = StmtInfoSet.diff job.exHist.coveredBlocks oldJob.exHist.coveredBlocks
		and oldJobOnlyBlocks = StmtInfoSet.diff oldJob.exHist.coveredBlocks job.exHist.coveredBlocks
		and jobOnlyEdges = EdgeSet.diff job.exHist.coveredEdges oldJob.exHist.coveredEdges
		and oldJobOnlyEdges = EdgeSet.diff oldJob.exHist.coveredEdges job.exHist.coveredEdges
		and jobOnlyConds = CondSet.diff job.exHist.coveredConds oldJob.exHist.coveredConds
		and oldJobOnlyConds = CondSet.diff oldJob.exHist.coveredConds job.exHist.coveredConds
		in
		let completed = Complete (Types.Truncated
			({ result_state = job.state;
			   result_history = {job.exHist with
														 coveredLines = jobOnlyLines;
														 coveredBlocks = jobOnlyBlocks;
														 coveredEdges = jobOnlyEdges;
														 coveredConds = jobOnlyConds; } },
			 { result_state = oldJob.state;
			   result_history = {oldJob.exHist with
														 coveredLines = oldJobOnlyLines;
														 coveredBlocks = oldJobOnlyBlocks;
														 coveredEdges = oldJobOnlyEdges;
														 coveredConds = oldJobOnlyConds; } }))
		in
		(* Remove the old job and add the merged job *)
		let merged_jobs = JobSet.add
			{ job with
					state = mergedState;
					exHist =
					{ job.exHist with
							coveredLines = LineSet.inter job.exHist.coveredLines oldJob.exHist.coveredLines;
							coveredBlocks = StmtInfoSet.inter job.exHist.coveredBlocks oldJob.exHist.coveredBlocks;
							coveredEdges = EdgeSet.inter job.exHist.coveredEdges oldJob.exHist.coveredEdges;
							coveredConds = CondSet.inter job.exHist.coveredConds oldJob.exHist.coveredConds;
					};
					mergePoints = StmtInfoSet.union job.mergePoints oldJob.mergePoints;
					jid = min job.jid oldJob.jid; (* Keep the lower jid, just because *)
			}
			(JobSet.remove oldJob merge_set)
		in
		(completed, (job_queue, merged_jobs))

let setRunningJob job =
	if !Output.runningJobId <> job.jid then (
		Output.set_mode Output.MSG_REG;
		Output.print_endline "***** Changing running job *****";
		Output.runningJobId := job.jid;
	);
	Output.runningJobDepth := (List.length job.state.path_condition)

(* Try to merge job with one that is waiting; then advance the resulting job by
	 one step. (Note that the job that *actually* gets advances might not be
	 step_job's argument; if the argument is at a merge point, mergeJobs places it
	 into the job_pool and returns a different job.) *)
let step_job job job_pool =
	let job_state,job_pool = mergeJobs job job_pool in
		match job_state with
			| Active job -> (
					setRunningJob job;
					try let result = match job.instrList with
						| [] -> exec_stmt job
						| _ -> exec_instr job
					in result,job_pool
					with Failure msg ->
						let result = { result_state = job.state; result_history = job.exHist } in
						let completed = Complete (Types.Abandoned (msg, !Output.cur_loc, result)) in
							(completed, job_pool)
				)
			| Complete _ -> (job_state, job_pool)
			| Fork _ -> failwith "mergeJobs can't return a Fork"


let main_loop job =
	Random.init 226; (* TODO: move random into some local state *)
	let rec main_loop completed job job_pool =
		match !signalStringOpt with
			| Some s ->
				(* if we got a signal, stop and return the completed results *)
				Output.set_mode Output.MSG_MUSTPRINT;
				Output.print_endline s;
				completed
			| None ->
				let result, job_pool = step_job job job_pool in
				begin match result with
					| Active job ->
						main_loop completed job job_pool
					| Fork (j1, j2) ->
						let job_pool = queue_job j1 job_pool in (* queue the true branch *)
						main_loop completed j2 job_pool (* continue the false branch *)
					| Complete completion ->
						(* log some interesting errors *)
						begin match completion with
							| Types.Abandoned (msg, loc, { result_state=state; result_history=hist }) ->
								Output.set_mode Output.MSG_MUSTPRINT;
								Output.printf "Error \"%s\" occurs at %s\n%sAbandoning path\n"
									msg (To_string.location loc)
                                    (if Executeargs.print_args.arg_print_callstack then
                                        "Call stack:\n"^
								       (To_string.callstack state.callContexts)
                                    else ""
                                    )
                                    ;
(*
								if run_args.arg_line_coverage then (
									Report.printPath state hist;
									Report.printLines hist.coveredLines
								)
								Output.set_mode Output.MSG_REG;
								Output.printf "Path condition: %s\n"
									(To_string.humanReadablePc state.path_condition hist.bytesToVars)
*)
							| _ ->
								()
						end;
						(* store the result and pick another job to continue, if available *)
						let completed = completion::completed in
						match pick_job job_pool with
							| Some (job, job_pool) ->
								main_loop completed job job_pool
							| None ->
								Output.set_mode Output.MSG_MUSTPRINT;
								Output.print_endline "Done executing";
								completed
				end
	in
	main_loop [] job ([], JobSet.empty)

