open Cil
open Types
open Executeargs

(** Remove a NOT from a bytes, or add one. The type of the bytes may
		be lost. *)
let logicalNegateBytes = function
		Bytes_Op(OP_LNOT,[bytes,_]) -> bytes
	| bytes -> Bytes_Op(OP_LNOT,[(bytes, Cil.intType)])

let dumpEdges (eS:EdgeSet.t) : unit =
	if EdgeSet.is_empty eS
	then ()
	else
		let currentSrc = ref (fst (EdgeSet.min_elt eS)) in
		if (!currentSrc = dummyStmt)
		then Output.printf "-1 [label:\"-1:Program start\"]\n"
		else Output.printf "%d [label:\"%d:%s\"]\n"
			!currentSrc.sid !currentSrc.sid
			(Pretty.sprint 500 (Cil.d_loc () (get_stmtLoc !currentSrc.skind)));
		EdgeSet.iter
			(fun (src,dst) ->
				if src != !currentSrc (* Print the label when the source statement changes. *)
				then(
					currentSrc := src;
					try
						Output.printf "%d [label:\"%d:%s\"]\n"
							!currentSrc.sid !currentSrc.sid
							(Pretty.sprint 500 (Cil.d_loc () (get_stmtLoc !currentSrc.skind)))
					with Errormsg.Error ->
						Output.printf "%d [label:\"%d:<error>\"]\n"
							!currentSrc.sid !currentSrc.sid
					);
				Output.printf "\t%d -> %d\n" src.sid dst.sid)
			eS
;;

let exec_instr_call job instr blkOffSizeOpt fexp exps loc =
	let state,exHist,stmt = job.state,job.exHist,job.nextStmt in

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
				let theSuccessor =
					(* [stmt] is an [Instr], so it can't have two successors. If
						 [func] returns, then [stmt] has exactly one successor. If
						 [func] is [exit] or has the [noreturn] attribute, [stmt]
						 has no successor. *)
					match stmt.succs with
							[] -> None
						| [h] -> Some h
						| _ -> assert false
				in
				let callContext = blkOffSizeOpt,instr,theSuccessor in
				let state = MemOp.state__start_fcall state (Source callContext) fundec argvs in
				Active (updateJob job state exHist (List.hd fundec.sallstmts))
		| _ ->
				try (
					let nextExHist = ref exHist in
				let state_end = begin match func with
					| Function.Builtin (builtin) ->
						let (state2,bytes) = builtin state exps in
						begin
							match blkOffSizeOpt with
								| None -> state2
								| Some dest -> MemOp.state__assign state2 dest bytes
						end

                    | Function.DataStructureOp (dsop) -> 
                        dsop state blkOffSizeOpt exps

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
                              let state3 = MemOp.state__assign state2 (target_block, target_offset,length_int_val) cloned_bytes in
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
				                  let truth = Stp.eval (given::state.path_condition) rv in
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
				                  let truth = Stp.eval state.path_condition rv in
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
										let (block,offset) = Eval.lval state lval in
										let symbBytes = (MemOp.bytes__symbolic size ) in
										Output.set_mode Output.MSG_MUSTPRINT;
										Output.print_endline (varinf.vname ^ " = " ^ (To_string.bytes symbBytes));
										nextExHist := { exHist with bytesToVars = (symbBytes,varinf) :: exHist.bytesToVars; };
										MemOp.state__assign state (block,offset,size) symbBytes
								| _ ->
										(* Any symbolic value not directly given to a variable by a call to
											 __SYMBOLIC(&<var>) does not get tracked. *)
										begin match blkOffSizeOpt with
											| None ->
													state
											| Some (block,offset,size) ->
													let ssize = match exps with
														| [] -> size
														| [CastE (_, h)] | [h] ->
																let newsize = Convert.bytes_to_int_auto (Eval.rval state h) in
																if newsize <= 0 then size else newsize
														| _ -> failwith "__SYMBOLIC takes at most one argument"
													in
													MemOp.state__assign state (block,offset,size (*ssize?*))
														(MemOp.bytes__symbolic ssize )
										end
						)

					| Function.SymbolicState ->
                        {state with
                             block_to_bytes = MemoryBlockMap.map 
                                (fun b -> match b with
                                     Bytes_FunPtr(_) -> b
                                   | _ -> MemOp.bytes__symbolic (MemOp.bytes__length b)
                                ) 
                                state.block_to_bytes;
                        }
					| Function.SymbolicStatic ->
							begin match blkOffSizeOpt with
								| None -> 
									state
								| Some (block,offset,size as dest) ->
									let key = 
										if List.length exps == 0 then 0 else
										let size_bytes = Eval.rval state (List.hd exps) in
												Convert.bytes_to_int_auto size_bytes 
									in
									let ssize =	size in
									let state2 = if  MemOp.loc_table__has state (loc,key) then state
										else MemOp.loc_table__add state (loc,key) (MemOp.bytes__symbolic ssize )
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
								| Some (block,offset,size as dest) ->
										MemOp.state__assign state dest (MemOp.bytes__symbolic size )
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
									let size = Convert.bytes_to_int_auto (Eval.rval state sizeexp) in
									let bytes = MemOp.state__get_bytes_from_lval state (block,offset,size) in
									begin match bytes with
										| Bytes_ByteArray(bytearray) -> To_string.bytestring bytearray
										| Bytes_Constant(CInt64(i,_,_)) -> Int64.to_string i
										| _ -> "(complicate)"
									end
								| _ -> "(nil)"
							in
							Output.set_mode Output.MSG_MUSTPRINT;
							Output.print_endline ("Evaluate to string: "^str);
							state
												
					| Function.Assume ->
						let pc = op_exps exps Cil.LAnd in
							MemOp.state__add_path_condition state pc
					
					| Function.PathCondition ->
						let pc_str = (Utility.print_list To_string.bytes state.path_condition " AND ") in
						Output.set_mode Output.MSG_MUSTPRINT;
						Output.print_endline (if String.length pc_str = 0 then "(nil)" else pc_str);
							state
															
					| Function.Assert -> 
						let post = op_exps exps Cil.LAnd in
							let truth = Stp.eval state.path_condition post in
							begin
								if truth == Stp.True then
									begin
									Output.set_mode Output.MSG_REG;
									Output.print_endline "Assertion satisfied.\n"
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
				(*			
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
					*)
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

				let theSuccessor =
					(* [stmt] is an [Instr] which doesn't end with a call to a
						 [noreturn] function, so it has exactly one successor. *)
					match stmt.succs with
							[h] -> h
						| _ -> assert false
				in
				Active (updateJob job state_end !nextExHist theSuccessor)

				) with Function.Notification_Exit exit_code ->
					(* If it was [Exit], there is no job to return *)
					Output.set_mode Output.MSG_MUSTPRINT;
					Output.print_endline "exit() called";
					Output.set_mode Output.MSG_REG;
					Output.print_endline
						("Path condition:\n" ^
							 (To_string.humanReadablePc state.path_condition exHist.bytesToVars));
					Complete (Types.Exit (exit_code, { result_state = state; result_history = exHist; }))

	end (* outer [match func] *)
;;

let exec_stmt job =
	let state,exHist,stmt = job.state,job.exHist,job.nextStmt in

	let nextExHist = { exHist with
		edgesTaken = EdgeSet.add (job.prevStmt,stmt) exHist.edgesTaken;
	} in

	if !Output.runningJobId <> job.jid then (
		Output.set_mode Output.MSG_REG;
		Output.print_endline "***** Changing running job *****";
		Output.runningJobId := job.jid;
	);
	Output.runningJobDepth := (List.length job.state.path_condition);

	Output.set_mode Output.MSG_STMT;
	Output.set_cur_loc (Cil.get_stmtLoc stmt.skind);
	Output.print_endline (To_string.stmt stmt);
	match stmt.skind with
		| Instr (instrs) ->
				(* Since we've used --domakeCFG, an [Instr] is a series of
					 [Set]s and [Asm]s, possibly terminated with a [Call]. Here,
					 we iterate through [instrs], performing each [Set]. If the
					 final [instr] is a [Call], we perform the call. Otherwise,
					 we just return the singleton JobSet resulting from the
					 sequence of assignments. *)
				let printInstr instr =
					Output.set_mode Output.MSG_STMT;
					Output.set_cur_loc (Cil.get_instrLoc instr);
					Output.print_endline (To_string.instr instr)
				in
				let rec doInstrs instrList latestState =
					match instrList with
							[] ->
								(* We know [stmt] is an [Instr], and if we've gotten
									 here, it doesn't have a [Call] in it. Thus, it has
									 exactly one successor. (A [stmt] ending with a
									 [Call] can have no successors if the call doesn't
									 return.) *)
								let theSuccessor =
									match stmt.succs with
											[h] -> h
										| _ -> assert false
								in
								Active (updateJob job latestState nextExHist theSuccessor)
						| (Set(lval,exp,loc) as instr)::tail ->
								printInstr instr;
								let (block,offset) = Eval.lval latestState lval in
								let size = (Cil.bitsSizeOf (Cil.typeOfLval lval))/8 in
								let rv = Eval.rval latestState exp in
								let state2 = MemOp.state__assign latestState (block,offset,size) rv in
								doInstrs tail state2
						| [Call(lvalopt, fexp, exps, loc) as instr] ->
								printInstr instr;
								let destOpt =
									match lvalopt with
										| None -> None
										| Some lval ->
												let (block,offset) = Eval.lval latestState lval in
												let size = (Cil.bitsSizeOf (Cil.typeOfLval lval))/8 in
												Some (block,offset,size)
								in
								exec_instr_call
									(updateJob job latestState nextExHist stmt)
									instr destOpt fexp exps loc
						| (Asm _ as instr)::tail ->
								Output.set_mode Output.MSG_MUSTPRINT;
								Output.print_endline "Warning: ASM unsupported";
								printInstr instr;
								doInstrs tail latestState
						| _ -> assert false (* [Call]s only occur at the end of [Instr]s *)
				in
				doInstrs instrs state

		| Cil.Return (expopt, loc) ->
				begin
					match state.callContexts with
						| Runtime::_ -> (* completed symbolic execution (e.g., return from main) *)
								Output.set_mode Output.MSG_MUSTPRINT;
								Output.print_endline "Program execution finished";
								Output.set_mode Output.MSG_REG;
								Output.print_endline
									("Path condition:\n" ^
										 (To_string.humanReadablePc state.path_condition exHist.bytesToVars));
								begin match expopt with
									| None ->
										Complete (Types.Return
											(None, { result_state = state; result_history = nextExHist; })) 
									| Some exp ->
										Complete (Types.Return
											(Some (Eval.rval state exp),
											 { result_state = state; result_history = nextExHist; }))
								end
						| (Source (destOpt,_,Some nextStmt))::_ ->
								let state2 =
									match expopt, destOpt with
										| None, _ (* If we are not returning a value *)
										| _, None -> (* or if we ignore the result of the call *)
												MemOp.state__end_fcall state (* Just end the call *)
										| Some exp, Some dest ->
												let rv = Eval.rval state exp in
												let state = MemOp.state__end_fcall state in
												MemOp.state__assign state dest rv
								in
								(* varinfo_to_block: memory_block VarinfoMap.t; *)
								let pop_frame = List.hd state.locals in
								let state2' = VarinfoMap.fold (fun varinfo block s -> MemOp.state__remove_block s block) pop_frame.varinfo_to_block state2 in 
								Active (updateJob job state2' nextExHist nextStmt)
						| _ ->
								(* [(_,_,None)] is impossible, because we wouldn't
									 be returning in that case. *)
								assert false
				end
		| Goto (stmtref, loc) ->
				Active (updateJob job state nextExHist !stmtref)
		| If (exp, block1, block2, loc) ->
				begin
				(* try a branch *)
					let try_branch pcopt block =
						let nextState = match pcopt with
							| Some(pc) -> MemOp.state__add_path_condition state pc
							| None -> state
						in
						if run_args.arg_branch_coverage then
							begin
								let which = (if block == block1 then fst else snd) in
								try
									let pcSet_ref = which (Hashtbl.find branches_taken (exp,loc)) in
									(* Add the path condition to the list of ways we take this branch *)
									pcSet_ref := PcHistSet.add (state.path_condition,nextExHist) !pcSet_ref
								with Not_found ->
									(* We haven't hit this conditional before. Initialize its entry in
										 the branch coverage table with the current path condition (and
										 an empty set for the other direction of the branch). *)
									Hashtbl.add
										branches_taken
										(exp,loc)
										(let res = (ref PcHistSet.empty, ref PcHistSet.empty) in
										which res := PcHistSet.singleton (state.path_condition,nextExHist);
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
							Output.print_endline ("Under the path condition:")
						end;
					let pc_str = (Utility.print_list To_string.bytes state.path_condition " AND ") in
					Output.print_endline (if String.length pc_str = 0 then "(nil)" else pc_str);
 
					let truth = Stp.eval state.path_condition rv in
 
					Output.set_mode Output.MSG_REG;
					if truth == Stp.True then
						begin
							Output.print_endline "True";
							let nextState,nextStmt = try_branch None block1 in
							Active (updateJob job nextState nextExHist nextStmt)
						end
					else if truth == Stp.False then
						begin
							Output.print_endline "False";
							let nextState,nextStmt = try_branch None block2 in
							Active (updateJob job nextState nextExHist nextStmt)
						end
					else
						begin
							Output.print_endline "Unknown";
							let nextStateT,nextStmtT = try_branch (Some rv) block1 in
							let nextStateF,nextStmtF = try_branch (Some (logicalNegateBytes rv)) block2 in

							let nextMergePts = (* Add in the new merge points *)
								if run_args.arg_merge_branches then (
									List.fold_left (fun set n -> IntSet.add n set) job.mergePoints
										begin match Inthash.find_all ifToJoinPointsHash stmt.sid with
											| [] ->
												begin match state.callContexts with
													| (Source (_,_,Some nextStmt))::_ ->
														print_endline "Will merge upon function return";
														[nextStmt.sid]
													| _ ->
														[]
													end
											| x -> x
										end
								) else (
									IntSet.empty
								)
							in
							let (j1,j2) =
								forkJob job nextStateT nextStateF nextStmtT nextStmtF nextExHist nextMergePts
							in
							Output.set_mode Output.MSG_MUSTPRINT;
							Output.printf "Branching on %s at %s.
Job %d is the true branch and job %d is the false branch.\n\n"
								 (To_string.exp exp)
								 (To_string.location loc)
								 j1.jid j2.jid;
							Fork (j1, j2)
						end
				end
		| Block(block)
		| Loop (block, _, _, _) ->
				(* A [Loop]'s block always has a non-empty bstmts. (See
					 Cil.succpred_stmt.)
					 This is not true for [Block]s, but it *does* seem to be
					 true for [Block]s which are not under [If]s, so we're okay. *)
				Active (updateJob job state nextExHist (List.hd block.bstmts))
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
	| pc -> Bytes_Op(OP_LAND, List.map (fun b -> (b,Cil.intType)) pc)

(* MergeDone includes the old job (which is swallowed by the current
	 job) and the state for the merged job. *)
exception MergeDone of job * state
exception TooDifferent


let atSameProgramPoint job1 job2 =
	let state1 = job1.state
	and state2 = job2.state in
	(* I'm trying to be conservative here by using physical equality
		 because I haven't quite figured out what 'being at the same
		 program point' means. But I'm pretty sure that if these are
		 physically equal, then they really are at the same program point,
		 whatever that means. *)
	if job1.nextStmt == job2.nextStmt &&
		state1.callContexts == state2.callContexts
	then (
		assert
			(job1.exHist.bytesToVars == job2.exHist.bytesToVars &&
				 state1.global         == state2.global &&
			 	 state1.locals         == state2.locals &&
				 state1.callstack      == state2.callstack &&
				 state1.va_arg         == state2.va_arg &&
				 state1.va_arg_map     == state2.va_arg_map &&
				 state1.loc_map        == state2.loc_map);
			true
		) else (
			false
		)

(* Merge job states *)
let mergeJobs job jobSet =
	try
		(* First test to see if the job should pause and/or merge *)
		if run_args.arg_merge_branches
		   && IntSet.mem job.nextStmt.sid job.mergePoints then begin
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
								 Bytes_Op(OP_LOR, [(jPCBytes,Cil.intType);(jobPCBytes,Cil.intType)])
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
					 assert (inJOnly = [] && inJobOnly = []);
					 Output.printf "Merging jobs %d and %d (identical memory)\n" j.jid job.jid;
					 raise (MergeDone (j, { job.state with path_condition = mergedPC; }))
				 ) else (
					 (* We have to fiddle with memory *)
					 let numSymbolsCreated = ref 0 in
					 let newSharedBlocks =
						 (* Make a new symbolic bytes for each differing block *)
						 List.map
							 (fun (block,jBytes,jobBytes) ->
									let size = MemOp.bytes__length jBytes in (* or should I just check block.memory_block_size? *)
									if size <> MemOp.bytes__length jobBytes then (
										failwith "Unimplemented: merging bytes with different lengths"
									) else (
										numSymbolsCreated := !numSymbolsCreated + size;
										if !numSymbolsCreated > 100 then raise TooDifferent;
										let symbBytes = MemOp.bytes__symbolic size in
										let jobImplication =
											(* jobPCBytes => symbBytes == jobBytes, i.e. ~jobPCBytes \/ symbBytes == jobBytes *)
											Bytes_Op(OP_LOR,
															 [(logicalNegateBytes jobPCBytes, Cil.intType);
																(Bytes_Op(OP_EQ,[(symbBytes,Cil.intType);
																								 (jobBytes,Cil.intType)]),Cil.intType)])
										and jImplication =
											(* Similarly for j *)
											Bytes_Op(OP_LOR,
															 [(logicalNegateBytes jPCBytes, Cil.intType);
																(Bytes_Op(OP_EQ,[(symbBytes,Cil.intType);
																								 (jBytes,Cil.intType)]),Cil.intType)])
										in
										((block, symbBytes), [jobImplication;jImplication])
									)
							 )
							 diffShared
					 in
					 let finalMergedPC = (* Attach all of the extra constraints to the path condition *)
						 List.fold_left
							 (fun pc (_,implications) -> List.rev_append implications pc)
							 mergedPC
							 newSharedBlocks
					 and mergedMemory =
						 (* I think it's safe (if not optimal) to keep both
								[inJOnly] and [inJobOnly] because [j]'s memory will be
								unreachable from [job]'s path and vice versa. *)
						 assocListToMemoryBlockMap
							 (List.concat [List.map fst newSharedBlocks; sameShared; inJOnly; inJobOnly])
					 in
					 Output.printf "Merging jobs %d and %d (differing memory)\n" j.jid job.jid;
					 raise (MergeDone (j, { job.state with
																		block_to_bytes = mergedMemory;
																		path_condition = finalMergedPC;
																}))
				 )
				 with TooDifferent -> print_endline "Memory too different to merge"; ()
				end
			) (* End iteration function *)
			jobSet;
			(* Reaching here means we've iterated through all jobs, never
				 being able to merge. So we just add the job to jobSet. *)
			(None, JobSet.add job jobSet)
		end else begin
			(* not at merge point *)
			(Some (Active job), jobSet)
		end;
	with MergeDone (oldJob,mergedState) ->
		(* This is a pretty stupid way of getting complete coverage with
			 merging, but it's something. It only credits a merged execution
			 with the edges that *both* sides covered. Edges covered by only
			 one side are attributed to truncated executions that end at the
			 join point. *)
		let jobOnlyEdges = EdgeSet.diff
			(EdgeSet.add (job.prevStmt,job.nextStmt) job.exHist.edgesTaken)
			oldJob.exHist.edgesTaken
		and oldJobOnlyEdges = EdgeSet.diff
			(* job.nextStmt == oldJob.nextStmt because we are at the same program point *)
			(EdgeSet.add (oldJob.prevStmt,job.nextStmt) oldJob.exHist.edgesTaken)
			job.exHist.edgesTaken
		in
		let completed = Complete (Types.Truncated
			({ result_state = job.state;
			   result_history = {job.exHist with edgesTaken = jobOnlyEdges } },
			 { result_state = oldJob.state;
			   result_history = {oldJob.exHist with edgesTaken = oldJobOnlyEdges } }))
		in
		(* Remove the old job and add the merged job *)
		let merged_jobs = JobSet.add
			{ job with
					state = mergedState;
					exHist =
					{ job.exHist with
							edgesTaken = EdgeSet.inter job.exHist.edgesTaken oldJob.exHist.edgesTaken;
					};
					mergePoints = IntSet.union job.mergePoints oldJob.mergePoints;
					jid = min job.jid oldJob.jid; (* Keep the lower jid, just because *)
			}
			(JobSet.remove oldJob jobSet)
		in
		(Some completed, merged_jobs)


let main_loop job =
	Random.init 226; (* TODO: move random into some local state *)
	let pausedJobs = ref JobSet.empty in
	let rec main_loop results jobs =
		(* Check to see if we got a signal; if so, stop execution, and return the results *)
		match !signalStringOpt with
			| Some s ->
				print_endline s;
				results
			| None ->
		match jobs with
		[] ->
			(* There are no unpaused jobs. *)
			if JobSet.is_empty !pausedJobs
			then (
				(* There are no paused jobs, either. We're done. *)
				Output.print_endline "Done executing";
				results
			) else (
				(* Pick a paused job, remove it from pausedJobs, and run it. *)
				let someJob = JobSet.choose !pausedJobs in
				pausedJobs := JobSet.remove someJob !pausedJobs;
				match exec_stmt someJob with
					| Active job -> main_loop results [ job ]
					| Fork (j1, j2) -> main_loop results [j1; j2]
					| Complete result -> main_loop (result::results) []
			)
		| job::jobs ->
			try
				let rec runUntilBranch results job =
					let maybe_result, merged = mergeJobs job !pausedJobs in
					(* Place the results of the merge back into pausedJobs. We
					   pause theJob and re-pause all results of the merge
					   because there could be other jobs that will also merge
					   at this statement. *)
					pausedJobs := merged;
					match maybe_result with
						| None ->
							(* job was at merge point, but wasn't merged *)
							main_loop results jobs
						| Some result ->
							(* job was either at merge point and merged, or was not at merge point *)
							let result = match result with Active job -> exec_stmt job | _ -> result in 
							begin match result with
								| Active j ->
									runUntilBranch results j
								| Fork (j1, j2) ->
									main_loop results (j1::j2::jobs)
								| Complete result ->
									main_loop (result::results) jobs
							end
				in
				runUntilBranch results job
			with Failure fail ->
				Output.set_mode Output.MSG_MUSTPRINT;
				Output.printf "Error \"%s\" occurs at %s\nAbandoning branch\n"
					 fail
					 (To_string.location !Output.cur_loc);
				Output.set_mode Output.MSG_REG;
				Output.print_endline
					("Path condition:\n" ^
						 (To_string.humanReadablePc job.state.path_condition job.exHist.bytesToVars));
				let result = Types.Abandoned { result_state = job.state; result_history = job.exHist; } in
				main_loop (result::results) jobs
	in
	main_loop [] [job]
