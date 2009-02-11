open Cil
open Types
open Executeargs

(** List of (path condition,executionHistory) pairs denoting coverage
		on different executions. *)
let coverage = ref [];;

(** List of path conditions that led to errors in the symbolic executor *)
let abandonedPaths = ref [];;

let dumpEdges (eS:EdgeSet.t) : unit =
	if EdgeSet.is_empty eS
	then ()
	else
		let currentSrc = ref (fst (EdgeSet.min_elt eS)) in
		if (!currentSrc = dummyStmt)
		then Printf.printf "-1 [label:\"-1:Program start\"]\n"
		else Printf.printf "%d [label:\"%d:%s\"]\n"
			!currentSrc.sid !currentSrc.sid
			(Pretty.sprint 500 (Cil.d_loc () (get_stmtLoc !currentSrc.skind)));
		EdgeSet.iter
			(fun (src,dst) ->
				if src != !currentSrc (* Print the label when the source statement changes. *)
				then(
					currentSrc := src;
					try
						Printf.printf "%d [label:\"%d:%s\"]\n"
							!currentSrc.sid !currentSrc.sid
							(Pretty.sprint 500 (Cil.d_loc () (get_stmtLoc !currentSrc.skind)))
					with Errormsg.Error ->
						Printf.printf "%d [label:\"%d:<error>\"]\n"
							!currentSrc.sid !currentSrc.sid
					);
				Printf.printf "\t%d -> %d\n" src.sid dst.sid)
			eS
;;

(* Note:
 * stmt = Cil.stmt
 * statement is union type of Cil.stmt and Cil.instr
 *)
(* TODO: need so modification for handling exit() *)
let init_argvs state func argvs = 
	let rec impl state pars argvs =
		match (pars,argvs) with
			| (h1::t1,h2::t2) ->
				let (block,offset) = Eval.lval state h1 in
				let size = (Cil.bitsSizeOf (Cil.typeOfLval h1))/8 in
				let state2 = MemOp.state__assign state (block,offset) size h2
				in 
					impl state2 t1 t2
			| ([],varg) -> 	
				Output.set_mode Output.MSG_FUNC;
				Output.print_endline ("Rest of args: "^(Utility.print_list To_string.bytes varg " , "));
				{state with va_arg = varg::state.va_arg; }
			| (_,[]) -> failwith "Unreachable init_argvs" 
	in
		let pars = List.map (fun x -> (Var(x),NoOffset)) func.sformals in
			impl state pars argvs
 ;;

let rec

exec_function state exHist func argvs caller_stmt =
  if func.sallstmts <> [] then
    Output.set_cur_loc (Cil.get_stmtLoc (List.hd func.sallstmts).skind)
  else
		Errormsg.s (Errormsg.error "No statements in function %s" func.svar.vname);
	let state1 = MemOp.state__start_fcall state func caller_stmt in
	let state2 = init_argvs state1 func argvs in
	let entry = List.hd func.sallstmts in
	exec_stmt state2 exHist entry 
and

exec_statement state exHist (statement: statement) =
	match statement with
		| MainEntry -> failwith "MainEntry is never executed!"
		| Instruction (instr, stmt) -> exec_instr state exHist instr stmt
		| Statement (stmt) -> exec_stmt state exHist stmt
			
and
			
exec_stmt state exHist (stmt: Cil.stmt) =
	(* Check to see if we got a signal; if so, stop execution *)
	(match !signalStringOpt with
			Some s -> print_endline s; raise SignalException
		 | _ -> ());

	let nextExHist () = { exHist with
		edgesTaken = EdgeSet.add (exHist.prevStmt,stmt) exHist.edgesTaken;
		prevStmt = stmt;
	} in
	Output.set_mode Output.MSG_STMT;
    Output.set_cur_loc (Cil.get_stmtLoc stmt.skind);
	Output.print_endline (To_string.stmt stmt);
	match stmt.skind with
		| Instr (instrs) ->
				(* Should the next line use exHist instead of (nextExHist ())?
					 It seems that [instrs] is empty iff [stmt] is a label. *)
				if instrs = [] then exec_stmt state (nextExHist ()) (next_stmt stmt) else
					let instr =
						List.hd instrs
					in
					exec_statement state (nextExHist ()) (Instruction(instr, stmt)) (* at least one instr? *)

		| Return (expopt, loc) ->
				begin
					let instruction = MemOp.state__get_caller_stmt state in
					let state2 =
						match expopt with
							| None ->
									let state3 = MemOp.state__end_fcall state in
									state3
							| Some(exp) ->
									begin match instruction with
										| Instruction(Call(lvalopt, fexp, exps, loc), stmt) ->
												let state3 = begin match lvalopt with
													| None ->
															let state3 = MemOp.state__end_fcall state in
															state3
													| Some(lval) -> (* assign exp to lval *)
															let rv = Eval.rval state exp in
															let state = MemOp.state__end_fcall state in
															let (block, offset) = Eval.lval state lval in
															let size = (Cil.bitsSizeOf (Cil.typeOfLval lval)) / 8 in
															MemOp.state__assign state (block, offset) size rv
												end
												in
												state3
										| MainEntry ->
												state
										| _ -> failwith "Impossible"
									end
					in
					(* varinfo_to_block: memory_block VarinfoMap.t; *)
					let pop_frame = List.hd state.locals in
					let state2' = VarinfoMap.fold (fun varinfo block s -> MemOp.state__remove_block s block) pop_frame.varinfo_to_block state2 in 
					if instruction = MainEntry then(
						Output.set_mode Output.MSG_MUSTPRINT;
						Output.print_endline "Program execution finished\nPath Condition:";
						Output.print_endline
							(To_string.annotated_bytes_list state.human_readable_path_condition);
						coverage := (state.human_readable_path_condition, nextExHist ()) :: !coverage;
						state2')
						else
							exec_statement state2' (nextExHist ()) (next_statement instruction)
				end
		| Goto (stmtref, loc) ->
				exec_stmt state (nextExHist ()) (!stmtref)
		| Loop (block, loc, breakopt, continueopt) ->
				(* We might want to use exHist here, as in the [Block] case
					 below because [stmt]s with skind [Loop] are uninteresting. *)
				exec_block state (nextExHist ()) block
		| If (exp, block1, block2, loc) ->
				begin
				(* try a branch *)
					let try_branch pcopt block =
						let state_t = match pcopt with
							| Some(pc) -> MemOp.state__add_path_condition state pc
							| None -> state
						in
						if run_args.arg_branch_coverage then
							begin
								let which = (if block == block1 then fst else snd) in
								try
									let pcSet_ref = which (Hashtbl.find branches_taken (exp,loc)) in
									(* Add the path condition to the list of ways we take this branch *)
									pcSet_ref := PcSet.add state.human_readable_path_condition !pcSet_ref
								with Not_found ->
									(* We haven't hit this conditional before. Initialize its entry in
										 the branch coverage table with the current path condition (and
										 an empty set for the other direction of the branch). *)
									Hashtbl.add
										branches_taken
										(exp,loc)
										(let res = (ref PcSet.empty, ref PcSet.empty) in
										which res := PcSet.singleton state.human_readable_path_condition;
										res)
							end;
						if block_is_empty block then
							exec_stmt state_t (nextExHist ()) (next_stmt stmt)
						else
							exec_block state_t (nextExHist ()) block
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
							try_branch None block1
						end
					else if truth == Stp.False then
						begin
							Output.print_endline "False";
							try_branch None block2
						end
					else
						begin
							Output.print_endline "Unknown";
							Output.increase ();
							let explore assumption block =
								try try_branch (Some assumption) block with
									| Function.Notification_Exit(state_exit, _) -> state_exit
									| Failure fail ->
											Output.set_mode Output.MSG_MUSTPRINT;
											Output.print_endline
												(Printf.sprintf "Error \"%s\" occurs at %s" fail
													 (To_string.location !Output.cur_loc));
											Output.print_endline "Abandoning branch";
											abandonedPaths :=
												state.human_readable_path_condition :: !abandonedPaths;
											state
							in
							Output.print_endline
								(String.concat ""
									 ["Try True branch of the conditional at ";
										To_string.location loc;
										" by assuming (";
										Pretty.sprint 100 (Cil.d_exp () exp);
										")"]);
							let state_t = explore rv block1 in
							Output.set_mode Output.MSG_REG;
							Output.print_endline "<TRUE BRANCH ENDED>";
 
							Output.print_endline
								(String.concat ""
									 ["Try False branch of the conditional at ";
										To_string.location loc;
										" by assuming !(";
										Pretty.sprint 100 (Cil.d_exp () exp);
										")"]);
							let state_f = explore (Bytes_Op(OP_LNOT,[(rv, Cil.typeOf exp)])) block2 in
							Output.set_mode Output.MSG_REG;
							Output.print_endline "<FALSE BRANCH ENDED>";

							Output.decrease ();
							combine_states [state_t; state_f]
						end
				end
		| Block(block) ->
				(* Use exHist here (rather than nextExHist) because we don't
					 need [Block]s in the CFG *)
				exec_block state exHist block
		| _ -> failwith "Not implemented yet"
and
block_is_empty block = (List.length block.bstmts = 0)
	
and

next_statement statement =
	match statement with
		| MainEntry -> failwith "Not implemented"
		| Instruction (instr, stmt) ->
				begin match stmt.skind with
					| Instr(instrs) ->
							let rec search_next instrs =
								begin match instrs with
									| [] -> failwith "instrs must contain instr"
									| h1::[] -> (* assert h1==instr *) Statement(next_stmt stmt)
									| h1:: h2:: t -> if h1 == instr then Instruction(h2, stmt) else search_next (h2:: t)
								end
							in
							search_next instrs
					| _ -> failwith "Instruction (instr, stmt): stmt must be Instr(instrs)"
				end
		| Statement (stmt) -> Statement (next_stmt stmt)
	
and

next_stmt stmt =
	match stmt.skind with
		| If (exp, block1, block2, loc) ->
				begin
					let add block lst = if List.length block.bstmts = 0 then lst else (List.hd block.bstmts):: lst in
					let heads = add block1 (add block2 []) in
					let rec find succs = match succs with
						| [] -> failwith "no succ stmt?!"
						| h:: t -> if List.memq h heads then find t else h
					in
					find stmt.succs
				end
		| Return (_) -> failwith "Don't call next_stmt with Return!"
		| _ -> List.hd stmt.succs
	
and

exec_block state exHist block =
	let stmts = block.bstmts in
	exec_stmt state exHist (List.hd stmts)

and

exec_instr state exHist instr stmt =
	(* MemOp.function_stat_increment (List.hd state.callstack); (*used to count the workload of each function*)*)
	Output.set_mode Output.MSG_STMT;
    Output.set_cur_loc (Cil.get_instrLoc instr);
	Output.print_endline (To_string.instr instr);
	match instr with
		| Set(lval,exp,loc) -> 
				let (block,offset) = Eval.lval state lval in
				let size = (Cil.bitsSizeOf (Cil.typeOfLval lval))/8 in
				let rv = Eval.rval state exp in
				let state2 = MemOp.state__assign state (block,offset) size rv in
				exec_statement state2 exHist (next_statement (Instruction(instr,stmt)))
		| Call(lvalopt, fexp, exps, loc) ->
				exec_instr_call state exHist instr stmt lvalopt fexp exps loc
		| Asm (attributes, s1_list, l1, l2, l3, loc) ->
				Output.set_mode Output.MSG_REG;
				Output.print_endline "Warning: ASM unsupported";
				exec_statement state exHist (next_statement (Instruction(instr, stmt)))
and

exec_instr_call state exHist instr stmt lvalopt fexp exps loc =
				let rec op_exps exps binop =
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
							exec_function state exHist fundec argvs (Instruction(instr,stmt))
					| _ ->
						let nextExHist = ref exHist in
				let state_end = begin match func with
					| Function.Builtin (builtin) ->
						let (state2,bytes) = builtin state exps in
							begin match lvalopt with
								| None -> state2
								| Some(lval) ->
									let (block,offset) = Eval.lval state lval in
									let size = (Cil.bitsSizeOf (Cil.typeOfLval lval))/8 in
										MemOp.state__assign state2 (block,offset) size bytes
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

										let isWritable = (*if List.length exps > 1 then false else*) true in
										let size = (Cil.bitsSizeOf (Cil.typeOfLval lval))/8 in
										let (block,offset) = Eval.lval state lval in
										let symbBytes = (MemOp.bytes__symbolic size isWritable) in
										nextExHist := { exHist with bytesToVars = (symbBytes,varinf) :: exHist.bytesToVars; };
										MemOp.state__assign state (block,offset) size symbBytes
								| _ ->
										(* Any symbolic value not directly given to a variable by a call to
											 __SYMBOLIC(&<var>) does not get tracked. *)
										begin match lvalopt with
											| None ->
													state
											| Some(lval) ->
													let isWritable = (*if List.length exps > 1 then false else*) true in
													let size = (Cil.bitsSizeOf (Cil.typeOfLval lval))/8 in
													let ssize = match exps with
														| [] -> size
														| [CastE (_, h)] | [h] ->
																let newsize = Convert.bytes_to_int_auto (Eval.rval state h) in
																if newsize <= 0 then size else newsize
														| _ -> failwith "__SYMBOLIC takes at most one argument"
													in
													let (block,offset) = Eval.lval state lval in
													MemOp.state__assign state (block,offset) size (*ssize?*)
														(MemOp.bytes__symbolic ssize isWritable)
										end
						)

					| Function.SymbolicStatic ->
							begin match lvalopt with
								| None -> 
									state
								| Some(lval) ->
									let key = 
										if List.length exps == 0 then 0 else
										let size_bytes = Eval.rval state (List.hd exps) in
												Convert.bytes_to_int_auto size_bytes 
									in
									let isWritable = (*if List.length exps > 1 then false else*) true in
									let size = (Cil.bitsSizeOf (Cil.typeOfLval lval))/8 in
									let ssize =	size in
									let (block,offset) = Eval.lval state lval in
									let state2 = if  MemOp.loc_table__has state (loc,key) then state
										else MemOp.loc_table__add state (loc,key) (MemOp.bytes__symbolic ssize isWritable)
									in
									let newbytes = MemOp.loc_table__get state2 (loc,key) in
										MemOp.state__assign state2 (block,offset) size newbytes
							end												
(*
					| Function.Fresh ->
							begin match lvalopt with
								| None -> 
									state
								| Some(lval) ->
									let id = Convert.bytes_to_int_auto (Eval.rval state (List.hd exps)) in
									let (block,offset) = Eval.lval state lval in
									let size = 1 in
										MemOp.state__assign state (block,offset) size (MemOp.bytes__of_list [(MemOp.byte__symbolic_with_id id true)])
							end				
*)												
					| Function.NotFound ->
							begin
								match lvalopt with
								| None -> 
										state
								| Some(lval) ->
										let (block,offset) = Eval.lval state lval in
										let size = (Cil.bitsSizeOf (Cil.typeOfLval lval))/8 in
										MemOp.state__assign state (block,offset) size (MemOp.bytes__symbolic size true)
							end
						
					| Function.Exit -> 
						
						Output.set_mode Output.MSG_MUSTPRINT;
						Output.print_endline "exit() called.\nPath Condition:";
						Output.print_endline
							(To_string.annotated_bytes_list state.human_readable_path_condition);
						coverage := (state.human_readable_path_condition, exHist) :: !coverage;
						raise (Function.Notification_Exit (state,Eval.rval state (List.hd exps)))
					
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
									Executedebug.log "\n(****************************";
									Executedebug.log "Assertion not-satisfied:";
									Executedebug.log "Assertion:";
									Executedebug.log (To_string.bytes post);
									Executedebug.log "Is Unsatisfiable with the path condition:";
									let pc_str = (Utility.print_list To_string.bytes state.path_condition " AND ") in
									Executedebug.log (if String.length pc_str = 0 then "(nil)" else pc_str);
									Executedebug.log "****************************)";
									()
									end									
									
							end;
							state
												
					| Function.BooleanOp (binop) ->
							begin match lvalopt with
								| None -> failwith "Unreachable BooleanOp"
								| Some(lval) ->
									let (block,offset) = Eval.lval state lval in
									let size = (Cil.bitsSizeOf (Cil.typeOfLval lval))/8 in

									let rv= op_exps exps binop in 
									MemOp.state__assign state (block,offset) size rv
							end

					| Function.BooleanNot ->
							begin match lvalopt with
								| None -> failwith "Unreachable BooleanNot"
								| Some(lval) ->
									let (block,offset) = Eval.lval state lval in
									let size = (Cil.bitsSizeOf (Cil.typeOfLval lval))/8 in
									let rv = Eval.rval state (UnOp(Cil.LNot, List.nth exps 0, Cil.voidType)) in
										MemOp.state__assign state (block,offset) size rv
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
								Printf.printf "sth\n";	
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
							Output.print_endline (Printf.sprintf "Record state %d" key);
							MemOp.index_to_state__add key state;
							state
													
					| Function.CompareState ->
						let bytes0 = Eval.rval state (List.nth exps 0) in
						let bytes1 = Eval.rval state (List.nth exps 1) in
						let key0 = Convert.bytes_to_int_auto bytes0 in
						let key1 = Convert.bytes_to_int_auto bytes1 in
						Output.set_mode Output.MSG_MUSTPRINT;
						Output.print_endline (Printf.sprintf "Compare states %d and %d" key0 key1);
						begin try
						  let s0 = try MemOp.index_to_state__get key0 
						  with Not_found -> (
						   	Output.set_mode Output.MSG_MUSTPRINT;
						   	Output.print_endline (Printf.sprintf "Warning: snapshot %d is absent" key0);
						   	    raise Not_found )
                          in
						  let s1 = try MemOp.index_to_state__get key1
						  with Not_found -> (
						   	Output.set_mode Output.MSG_MUSTPRINT;
						   	Output.print_endline (Printf.sprintf "Warning: snapshot %d is absent" key1);
						   	    raise Not_found )
                          in
						    let output = MemOp.cmp_states s0 s1 in
						    	Output.set_mode Output.MSG_MUSTPRINT;
						    	Output.print_endline output;			
						    	state
                        with Not_found -> 
						   	Output.set_mode Output.MSG_MUSTPRINT;
						   	Output.print_endline (Printf.sprintf "Compare states fail");
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
						
					| _ -> failwith "unreachable"
						
						end in
							exec_statement state_end !nextExHist (next_statement (Instruction(instr,stmt)))

						
					end (* match func *)
	
					
	
and

combine_states states =
	List.hd states
;; 
