open Cil
open Ternary
open Bytes
open BytesUtility
open Types
open PathMerging
open Executeargs
open Cilutility
open Utility

let eval_with_cache state pc bytes =
    MemOp.state__eval state pc bytes
  (*
  match MemOp.state__get_bytes_eval_cache state bytes with
    | Some (boolval) -> ((if boolval then True else False), state)
    | None ->
        let truth = Stp.eval pc bytes in
          if truth = True then (truth,MemOp.state__add_bytes_eval_cache state bytes true)
          else if truth = False then (truth,MemOp.state__add_bytes_eval_cache state bytes false)
          else (truth,state)
   *)

let stmtInfo_of_job job =
	{ siFuncName = (List.hd (get_callstack job.state)).svar.vname;
		siStmt = Cilutility.stmtAtEndOfBlock (get_proc_info job).stmt; }

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
				then match (get_proc_info job).stmt.skind with
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
							Some nextStmt when (get_proc_info job).stmt
								 == Cilutility.stmtAtEndOfBlock (get_proc_info job).stmt ->
									let funcName = (List.hd (get_callstack job.state)).svar.vname in
									EdgeSet.add
										({ siFuncName = funcName; siStmt = (get_proc_info job).stmt; },
										 { siFuncName = funcName;
											 siStmt = Cilutility.stmtAtEndOfBlock nextStmt; })
										job.exHist.coveredEdges
						| _ -> job.exHist.coveredEdges
				) else EdgeSet.empty;
			coveredConds =
                                if run_args.arg_cond_coverage
                                then (
                                        match (get_proc_info job).stmt.skind with
                                                If _ -> CondSet.add (stmtInfo_of_job job,whichBranch) job.exHist.coveredConds
                                        | _ -> job.exHist.coveredConds
                                ) else CondSet.empty;
			executionPath =
				if run_args.arg_path_coverage && (get_proc_info job).stmt == Cilutility.stmtAtEndOfBlock (get_proc_info job).stmt
				then (
					{ siFuncName = (List.hd (get_callstack job.state)).svar.vname; 
						siStmt = (get_proc_info job).stmt; } :: job.exHist.executionPath
				) else (
					job.exHist.executionPath
				)
	}

let addInstrCoverage job instr =
	let instrLoc = get_instrLoc instr in
	{ job.exHist with coveredLines =
			LineSet.add (instrLoc.file,instrLoc.line) job.exHist.coveredLines; }

let exec_instr_call job instr lvalopt fexp exps loc =
	let state,exHist,stmt = job.state,job.exHist,(get_proc_info job).stmt in

	let op_exps state exps binop =
		let rec impl exps =
			match exps with
				| [] -> failwith "AND/OR must take at least 1 argument"
				| h::[] -> h
				| h:: tail -> let t = impl tail in BinOp(binop, h, t, Cil.intType)
		in
		Eval.rval state (impl exps)
	in

       (*let state, func funList = Function.from_exp state fexp exps in*)

       let process_func state func = 
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
            InvInput.examine state fundec
          else ()
        end;

				(* Update the state, the next stmt to execute, and whether or
					 not we're in a tracked function. *)
				Active { job with
							state = state;
							proc_info = set_proc_info job {(get_proc_info job) with
									stmt = List.hd fundec.sallstmts; 
									inTrackedFn = StringSet.mem fundec.svar.vname run_args.arg_fns; 
							};
					};
		| _ ->
				try (
					let nextExHist = ref exHist in
				let state_end = begin match func with
					| Function.Builtin (builtin) ->
						let (state,bytes) = builtin state exps in
                                               begin match lvalopt with
                                                       | None ->
                                                               state
                                                       | Some cil_lval ->
                                                               let state, lval = Eval.lval state cil_lval in
                                                               MemOp.state__assign state lval bytes
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

                          
(* 
 * This function was introduced to implement abstract set. Maybe we don't need
 * it anymore?
 * *)
                          (*
                    | Function.Clone ->
                        if List.length exps <> 3 then failwith "Clone takes 3 arguments" else
						let state, argvs = List.fold_right begin fun exp (state, argvs) ->
							let state, bytes = Eval.rval state exp in
							(state, bytes::argvs)
						end exps (state, []) in
                        let target_ptr_bytes = List.nth argvs 0 in
                        let source_ptr_bytes = List.nth argvs 1 in
                        let length_int_bytes = List.nth argvs 2 in
                        begin
                        match target_ptr_bytes,source_ptr_bytes,length_int_bytes with
                          | Bytes_Address(target_block, target_offset),
                            Bytes_Address(source_block, source_offset),
                            Bytes_Constant(_)
                            ->
                              let length_int_val = bytes_to_int_auto length_int_bytes in
                              let state, source_bytes = MemOp.state__get_bytes_from_lval state (source_block,source_offset,length_int_val) in
                              let state, cloned_bytes = MemOp.state__clone_bytes state source_bytes in
                              let state = MemOp.state__assign state (Lval_Block (target_block, target_offset),length_int_val) cloned_bytes in
                              state
                          | _ -> failwith "Clone error"
                       end
                           *)

                    | Function.Given -> 
						begin match lvalopt with
							| None ->
								state
							| Some cil_lval ->
								let state, lval = Eval.lval state cil_lval in
                                let truthvalue = 
                                  begin
                                  if List.length exps <> 2 then 
                                    failwith "__GIVEN takes 2 arguments"
                                  else
                                  let state, given = Eval.rval state (List.nth exps 0) in
				                  let state, rv = Eval.rval state (List.nth exps 1 ) in
				                  let state, truth = eval_with_cache state (given::state.path_condition) rv in
				                  if truth == True then lazy_int_to_bytes 1 
				                  else if truth == False then lazy_int_to_bytes 0
				                  else bytes__symbolic (bitsSizeOf intType / 8)
                                  end
                                in
								MemOp.state__assign state lval truthvalue
						end
                    | Function.TruthValue -> 
						begin match lvalopt with
							| None -> state 
							| Some cil_lval ->
								let state, lval = Eval.lval state cil_lval in
                                let truthvalue = 
                                  lazy_int_to_bytes
                                  begin
                                  if List.length exps = 0 then 0 else
				                  let state, rv = Eval.rval state (List.hd exps) in
				                  let state, truth = eval_with_cache state state.path_condition rv in
				                  if truth == True then 1
				                  else if truth == False then -1
				                  else 0
                                  end
                                in
								MemOp.state__assign state lval truthvalue
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
											| None ->
													failwith "Incorrect usage of __SYMBOLIC(): symbolic value generated and ignored"
											| Some lval ->
													let state, (_, size as lval) = Eval.lval state lval in
													let state, ssize = match exps with
														| [] ->
															(state, size)
														| [CastE (_, h)] | [h] ->
															let state, bytes = Eval.rval state h in
															let newsize = bytes_to_int_auto bytes in
															(state, if newsize <= 0 then size else newsize)
														| _ ->
															failwith "__SYMBOLIC takes at most one argument"
													in
													MemOp.state__assign state lval (bytes__symbolic ssize)
										end
						)

					| Function.SymbolicState ->
						MemoryBlockMap.fold begin fun block _ state ->
							(* TODO: what about deferred bytes? *)
							(* TODO: handle pointers with an alias analysis *)
							let state, bytes = MemOp.state__get_bytes_from_block state block in 
							match bytes with
								| Bytes_FunPtr(_) ->
									state
								| _ ->
									MemOp.state__add_block state block (bytes__symbolic (bytes__length bytes))
						end state.block_to_bytes state

					| Function.SymbolicStatic ->
							begin match lvalopt with
								| None -> 
									state
								| Some cil_lval ->
									let state, (_, size as lval) = Eval.lval state cil_lval in
									let state, key =
										if List.length exps == 0 then
											(state, 0)
										else
											let state, size_bytes = Eval.rval state (List.hd exps) in
											(state, bytes_to_int_auto size_bytes) 
									in
									let state =
										if MemOp.loc_table__has state (loc,key)
										then state
										else MemOp.loc_table__add state (loc,key) (bytes__symbolic size)
									in
									let newbytes = MemOp.loc_table__get state (loc,key) in
									MemOp.state__assign state lval newbytes
							end												
(*
					| Function.Fresh ->
							begin match blkOffSizeOpt with
								| None -> 
									state
								| Some (block,offset,_) ->
									let id = bytes_to_int_auto (Eval.rval state (List.hd exps)) in
									let size = 1 in
										MemOp.state__assign state (block,offset,size) (bytes__of_list [(MemOp.byte__symbolic_with_id id true)])
							end				
*)												
					| Function.NotFound ->
							begin match lvalopt with
								| None -> 
									state
								| Some cil_lval ->
									let state, (_, size as lval) = Eval.lval state cil_lval in
									MemOp.state__assign state lval (bytes__symbolic size)
							end
						
					| Function.Exit ->
						let exit_code = match exps with
							| exp1::_ -> Some (snd (Eval.rval state exp1))
							| [] -> None
						in
						raise (Function.Notification_Exit (exit_code))
					
					| Function.Evaluate ->
						let state, pc = op_exps state exps Cil.LAnd in
							Output.set_mode Output.MSG_MUSTPRINT;
							Output.print_endline ("    Evaluates to "^(To_string.bytes pc));
							state
							
					| Function.EvaluateString ->
							let exp = List.hd exps in
							let sizeexp = List.nth exps 1 in
							let state, addr_bytes = Eval.rval state exp in
							let state, str = match addr_bytes with
								| Bytes_Address(block, offset) ->
									let state, size_bytes = Eval.rval state sizeexp in
									let size =
                                      try bytes_to_int_auto size_bytes with
                                          Failure(s) -> Output.print_endline s; 32
                                    in
									let state, bytes = MemOp.state__deref state (conditional__lval_block (block, offset), size) in
									let str = match bytes with
										| Bytes_ByteArray(bytearray) -> To_string.bytestring bytearray
										| Bytes_Constant(CInt64(i,_,_)) -> Int64.to_string i
										| _ -> "(complicate)"
									in
									(state, str)
								| _ ->
									(state, "(nil)")
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
						let state, pc = op_exps state exps Cil.LAnd in
							MemOp.state__add_path_condition state pc false
					
					| Function.PathCondition ->
						Output.set_mode Output.MSG_MUSTPRINT;
						let pc_str = (Utility.print_list To_string.bytes state.path_condition "\n AND \n") in
						Output.print_endline (if String.length pc_str = 0 then "(nil)" else pc_str);
							state
															
					| Function.Assert -> 
						let state, assertion = op_exps state exps Cil.LAnd in
						Eval.check state assertion exps
												
					| Function.IfThenElse ->
							begin match lvalopt with
								| None -> state
								| Some cil_lval ->
									let state, lval = Eval.lval state cil_lval in
									let state, bytes0 = Eval.rval state (List.nth exps 0) in
									let state, bytes1 = Eval.rval state (List.nth exps 1) in
									let state, bytes2 = Eval.rval state (List.nth exps 2) in
									let c = IfThenElse (
										guard__bytes bytes0, conditional__bytes bytes1, conditional__bytes bytes2
									) in
									let rv = make_Bytes_Conditional c in
									MemOp.state__assign state lval rv
							end
												
					| Function.BooleanOp (binop) ->
							begin match lvalopt with
								| None -> failwith "Unreachable BooleanOp"
								| Some cil_lval ->
									let state, lval = Eval.lval state cil_lval in
									let state, rv = op_exps state exps binop in
									MemOp.state__assign state lval rv
							end

					| Function.BooleanNot ->
							begin match lvalopt with
								| None -> failwith "Unreachable BooleanNot"
								| Some cil_lval ->
									let state, lval = Eval.lval state cil_lval in
									let state, rv = Eval.rval state (UnOp(Cil.LNot, List.hd exps, Cil.voidType)) in
									MemOp.state__assign state lval rv
							end

					| Function.Aspect(pointcut, advice) ->
						let state, argvs = List.fold_right begin fun exp (state, argvs) ->
							let state, bytes = Eval.rval state exp in
							(state, bytes::argvs)
						end exps (state, []) in
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
						let state, bytes = Eval.rval state (List.hd exps) in
						let key = bytes_to_int_auto bytes in
							Output.set_mode Output.MSG_MUSTPRINT;
							Output.printf "Record state %d\n" key;
							MemOp.index_to_state__add key state;
							state

         (*
					| Function.PrintState ->
						Output.set_mode Output.MSG_MUSTPRINT;
						let module MemBlockSet = Set.Make(struct
							type t = memory_block
							let compare a b = Pervasives.compare a.memory_block_id b.memory_block_id
							end)
						in
						let blocksAlreadyPrinted = ref MemBlockSet.empty in
                        let printStringString s1 s2 =
                            Output.print_endline (s1 ^ " = " ^ s2)
                        in
                        let rec printVarFieldsBytes varname typ bytes off =
                            (* break down each local by its fields *)
                            (* canonicalize concrete values by their array rep*)
                          match typ with
                            | TComp (compinfo,_) -> 
                                List.iter 
                                  (fun fieldinfo -> printVarFieldsBytes (varname^"."^fieldinfo.fname) fieldinfo.ftype bytes (off+fst(Cil.bitsOffset typ (Field(fieldinfo,NoOffset)))/8))
                                  compinfo.cfields
                            | _ -> 
                                let rec p b= match b with
                                | Bytes_Constant const ->  p (constant_to_bytes const)
                                | Bytes_ByteArray ba -> To_string.bytes (Bytes_ByteArray(ImmutableArray.sub ba off (Cil.bitsSizeOf typ/8)))
                                | _ -> "("^(To_string.bytes b)^","^(string_of_int off)^","^(string_of_int (Cil.bitsSizeOf typ/8))^")"
                                in 
                                let rhs = p bytes
                                in printStringString varname rhs
                        in
                        let printVarBytes var bytes =
                            printVarFieldsBytes var.vname var.vtype bytes 0 
                        in
						let printVar var block =
							blocksAlreadyPrinted := MemBlockSet.add block !blocksAlreadyPrinted;
                            match (MemoryBlockMap.find block state.block_to_bytes) with
                              | Immediate bytes -> printVarBytes var bytes
                              | Deferred _ -> printStringString var.vname "(deferred)"
						in
						Output.print_endline "#BEGIN PRINTSTATE";
						Output.print_endline "#Globals:";
						VarinfoMap.iter printVar state.global.varinfo_to_block;
						Output.print_endline "#Locals:";
						VarinfoMap.iter printVar (List.hd state.locals).varinfo_to_block;
						Output.print_endline "#Formals:";
						VarinfoMap.iter printVar (List.hd state.formals).varinfo_to_block;
						Output.print_endline "#Memory:";
						MemoryBlockMap.iter (fun block deferred ->
							if not (MemBlockSet.mem block !blocksAlreadyPrinted) then (
								Output.print_endline (To_string.memory_block block ^ " -> " ^ (To_string.deferred deferred))
							))
							state.block_to_bytes;
						Output.print_endline "#END PRINTSTATE";
						state
          *)

					| Function.PrintState ->
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
                                  (fun fieldinfo -> printVarFieldsBytes (varname^"."^fieldinfo.fname) fieldinfo.ftype bytes (off+fst(Cil.bitsOffset typ (Field(fieldinfo,NoOffset)))/8))
                                  compinfo.cfields
                            | _ -> 
                                let size = (Cil.bitsSizeOf typ/8) in
                                let rec p b= match b with
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
                                let rhs = p bytes
                                in printStringString varname rhs
                        in
                        let printVarBytes var bytes =
                            printVarFieldsBytes var.vname var.vtype bytes 0 
                        in
						let printVar var lval_block =
                          if Cilutility.isConstType var.vtype then () else
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

					| Function.CompareState ->
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
						   	    raise Not_found )
                          in
						  let s1 = try MemOp.index_to_state__get key1
						  with Not_found -> (
						   	Output.set_mode Output.MSG_MUSTPRINT;
						   	Output.printf "Warning: snapshot %d is absent\n" key1;
						   	    raise Not_found )
                          in
						    	Output.set_mode Output.MSG_MUSTPRINT;
						    	ignore (MemOp.cmp_states s0 s1);
						    	state
                        with Not_found -> 
						   	Output.set_mode Output.MSG_MUSTPRINT;
						   	Output.printf "Compare states fail\n";
                            state
                        end
					| Function.AssertEqualState ->
						let state, bytes0 = Eval.rval state (List.nth exps 0) in
						let key0 = bytes_to_int_auto bytes0 in
						begin try 
							let s0 = MemOp.index_to_state__get key0 in
							Output.set_mode Output.MSG_MUSTPRINT;
						    	if MemOp.cmp_states s0 state then Output.print_endline "AssertEqualState satisfied";
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
				if (get_proc_info job).inTrackedFn && run_args.arg_edge_coverage then
					nextExHist := { !nextExHist with coveredEdges =
							EdgeSet.add (stmtInfo_of_job job,
									{ siFuncName = (List.hd (get_callstack job.state)).svar.vname;
									  siStmt = Cilutility.stmtAtEndOfBlock nextStmt; })
							!nextExHist.coveredEdges; };

				(* Update state, the stmt to execute, and exHist (which may
					 have gotten an extra bytesToVar mapping added to it). *)
				Active { job with 
					state = state_end; 
					proc_info = set_proc_info job {(get_proc_info job) with 
						stmt = nextStmt; };
					exHist = !nextExHist; }
	

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
					Complete (
					{
						job = job;
						reason = Types.Exit (exit_code, { result_state = state; result_history = exHist; });
					})

	end (* outer [match func] *)
       in
       let rec process_func_list func_list =
	      match func_list with
	      | [] -> []
	      | (state, func)::t -> 
			let job_state =
				try
					(process_func state func)
				with Failure msg ->
					if run_args.arg_failfast then failwith msg;
					let result = { result_state = job.state; result_history = job.exHist } in
					let completed = Complete (
						{
							job = job;
							reason = Types.Abandoned (msg, !Output.cur_loc, result);
						}
					) in
					completed
			in
			job_state::(process_func_list t)
       in
	let f = (process_func_list (Function.from_exp state fexp exps)) in
	match f with
		| _::_::_ -> Big_Fork(f)
		| [a] -> a
		| [] -> failwith "No valid function found!"


let exec_instr job =
	assert ((get_proc_info job).instrList <> []);
	let printInstr instr =
		Output.set_mode Output.MSG_STMT;
		Output.set_cur_loc (Cil.get_instrLoc instr);
		Output.print_endline (To_string.instr instr)
	in

	let instr,tail = match ((get_proc_info job).instrList) with 
		| i::tl -> i,tl 
		| _ -> assert false 
	in
	let job = { job with 
			proc_info = set_proc_info job {(get_proc_info job) with 
				instrList = tail; };
		    }; in

	(* Within instructions, we have to update line coverage (but not
		 statement or edge coverage). *)
	let job =
		if (get_proc_info job).inTrackedFn && run_args.arg_line_coverage
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
         let nextStmt = if tail = [] then List.hd (get_proc_info job).stmt.succs else (get_proc_info job).stmt in
           Active { job with 
			state = state; 
			proc_info = set_proc_info job {(get_proc_info job) with
				stmt = nextStmt;
			};
		    }
		| Call(lvalopt, fexp, exps, loc) ->
			assert (tail = []);
			printInstr instr;
			exec_instr_call job instr lvalopt fexp exps loc
		| Asm _ ->
			Output.set_mode Output.MSG_MUSTPRINT;
			Output.print_endline "Warning: ASM unsupported";
			printInstr instr;
			Active (if tail = [] (* Update job.stmt if this instr ends this stmt *)
							then { job with 
								proc_info = set_proc_info job {(get_proc_info job) with
									stmt = List.hd (get_proc_info job).stmt.succs; 
								};
							      }
							else job)

let exec_stmt job =
	assert ((get_proc_info job).instrList = []);
	let state,stmt = job.state,(get_proc_info job).stmt in

	let nextExHist ?(whichBranch=false) nextStmtOpt =
		if (get_proc_info job).inTrackedFn
		then addStmtCoverage job whichBranch nextStmtOpt
		else job.exHist
	in

	Output.set_mode Output.MSG_STMT;
	Output.set_cur_loc (Cil.get_stmtLoc stmt.skind);
	Output.print_endline (To_string.stmt stmt);
	match stmt.skind with
		| Instr [] ->
				let nextStmt = match stmt.succs with [x] -> x | _ -> assert false in
				Active { job with 
						proc_info = set_proc_info job {(get_proc_info job) with
							stmt = nextStmt; };
						exHist = nextExHist (Some nextStmt); }
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
			Active { job with 
					proc_info = set_proc_info job {(get_proc_info job) with
						instrList = instrs; };
					exHist = nextExHist nextStmtOpt; }
		| Cil.Return (expopt, loc) ->
				begin
					match (get_callContexts state) with
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
								Complete (
								{
									job = job;
									reason = Types.Return
										(retval, { result_state = state; result_history = nextExHist None; })
								}) 
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
								let callingFuncName = (List.hd (get_callstack state2)).svar.vname in
								(* Update the state, stmt, exHist, and whether or not
									 we're in a tracked function *)
								let job' = { job with
										state = state2;
										proc_info = set_proc_info job {(get_proc_info job) with
											stmt = nextStmt;
											inTrackedFn =
											StringSet.mem callingFuncName run_args.arg_fns; };
									      }
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
										if (get_proc_info job').inTrackedFn && run_args.arg_edge_coverage
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
				Active { job with 
					proc_info = set_proc_info job {(get_proc_info job) with
						stmt = !stmtref;
					};
					exHist = nextExHist (Some !stmtref); }
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
 
					let state, truth = eval_with_cache state state.path_condition rv in
 
					Output.set_mode Output.MSG_REG;
					if truth == True then
						begin
							Output.print_endline "True";
							let nextState,nextStmt = try_branch state None block1 in
							let job' = { job with 
								state = nextState; 
								proc_info = set_proc_info job {(get_proc_info job) with
									stmt = nextStmt; 
								};
							} in
							Active { job' with exHist = nextExHist (Some nextStmt) ~whichBranch:true; }
						end
					else if truth == False then
						begin
							Output.print_endline "False";
							let nextState,nextStmt = try_branch state None block2 in
							let job' = { job with 
								state = nextState; 
								proc_info = set_proc_info job {(get_proc_info job) with
									stmt = nextStmt; 
								};
							} in
							Active { job' with exHist = nextExHist (Some nextStmt) ~whichBranch:false; }
						end
					else
						begin
							Output.print_endline "Unknown\n";
							
							let nextStateT,nextStmtT = try_branch state (Some rv) block1 in
							let nextStateF,nextStmtF = try_branch state (Some (logicalNot rv)) block2 in

							let job' = 
								if run_args.arg_merge_paths then (
									(* Add in the new merge points *)
									{ job with mergePoints =
											List.fold_left (fun set s -> StmtInfoSet.add s set) job.mergePoints
												begin match Hashtbl.find_all ifToJoinPointsHash (stmtInfo_of_job job) with
													| [] ->
															begin match (get_callContexts state) with
																| (Source (_,_,_,nextStmt))::_ ->
																		Output.print_endline "Will merge upon function return";
																		(* nextStmt is in the calling function, not the current function,
																			 so we call (nth _ 1). *)
																		[{ siFuncName = (List.nth (get_callstack state) 1).svar.vname ; siStmt = nextStmt }]
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
										proc_info = set_proc_info job {(get_proc_info job) with
											stmt = nextStmtT;
										};
										exHist = nextExHist (Some nextStmtT) ~whichBranch:true;
										jid = Utility.next_id Output.jidCounter; 
							} in
							let falseJob = { job' with
										state = nextStateF;
										proc_info = set_proc_info job {(get_proc_info job) with
											stmt = nextStmtF;
										};
										exHist =  nextExHist (Some nextStmtF) ~whichBranch:false; 
							} in
							Output.set_mode Output.MSG_MUSTPRINT;
							Output.printf "Branching on %s at %s. %s\nJob %d is the true branch and job %d is the false branch.\n\n"
								 (To_string.exp exp)
								 (To_string.location loc)
                                 (if Executeargs.print_args.arg_print_callstack then
                                     "Call stack:\n"^
								    (To_string.callstack (get_callContexts state))
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
				Active { job with 
						proc_info = set_proc_info job {(get_proc_info job) with
							stmt = nextStmt; 
						};
						exHist = nextExHist (Some nextStmt); 
					}
		| _ -> failwith "Not implemented yet"


(*
let printJob (state,stmt) =
	Output.printf "path condition = %s\n" (To_string.bytes_list state.path_condition);
	Output.printf "statement:\n%s\n"
		(try Pretty.sprint 100 (Cil.d_stmt () stmt)
		 with Errormsg.Error -> "<error printing statement>")
*)


let setRunningJob job =
	if !Output.runningJobId <> job.jid then (
      if not Executeargs.run_args.arg_cfg_pruning then
        (
		Output.set_mode Output.MSG_REG;
		Output.print_endline "***** Changing running job *****"
        );
		Output.runningJobId := job.jid;
	);
	Output.runningJobDepth := (List.length job.state.path_condition)


(* Try to merge job with one that is waiting; then advance the resulting job by
	 one step. (Note that the job that *actually* gets advances might not be
	 step_job's argument; if the argument is at a merge point, mergeJobs places it
	 into the job_pool and returns a different job.) *)

let pass_targets targets job fexp exps =
  (* convert fexp to fundec *) 
  let state = job.state in
  let hist = job.exHist in
  let fundecs = 
    List.fold_left 
      ( fun lst (_,ft) -> match ft with Function.Ordinary (f) -> f::lst | _ -> lst
      ) [] (Function.from_exp state fexp exps) in
  (* convert args to bytes. argvs are from left to right *)
  let _, argvs = 
    List.fold_right 
      ( fun exp (state, argvs) ->
          let state, bytes = Eval.rval state exp in
            (state, bytes::argvs)
      ) exps (state, []) 
  in
  (* check if all fundecs pass target *)
  let pass_target target : bool*bytes =
    List.fold_left 
      ( fun (b,fc) fundec -> 
          if fundec != target.func then true,Bytes.bytes__zero
          else
            (
              (* caller's input values to callee: argvs
               * callee's input values from caller: target.state 
               * at the end, use caller's state as the state of quirying 
               *)
              Output.banner_printf 0  "Check if the failing condition is hit\n";
              (* TODO: add globals *)
              let connecting_bytes = 
                List.fold_left2
                  ( fun b argv formal ->
                      let _,fargv = Eval.rval target.entry_state (Lval(Var(formal),NoOffset)) in
                      let equation = Operation.eq [(fargv,formal.vtype);(argv,formal.vtype)] in
                        Operation.bytes__land equation b 
                  ) bytes__one argvs fundec.sformals
              in
                (*
                 (Output.banner_printf 0  "Failing condition: %s\n" (To_string.bytes target.failing_condition));
                 (Output.banner_printf 0  "Path condition: %s\n" (String.concat "&&" (List.map To_string.bytes state.path_condition)));
                 (Output.banner_printf 0  "Connection : %s\n" (To_string.bytes connecting_bytes));
                 *)
              let _, truth = eval_with_cache state (connecting_bytes::state.path_condition)  (Operation.bytes__not target.failing_condition) in
              let total_failing_condition = Operation.bytes__land target.failing_condition connecting_bytes in
              let total_failing_condition = Operation.bytes__lor total_failing_condition fc in

              let print_failed_assertion isUnknown =
                let _ = Output.set_mode Output.MSG_MUSTPRINT in
                let caller = List.hd (get_callstack state) in
                let mustmay = (if isUnknown then "may" else "must") in
                let log = Executedebug.log in
                let _ = Output.banner_printf 1  "Failing condition %s be hit (see error log).\n%!" mustmay in
                let _ = log "(****************************\n" in
	        let _ = log (Printf.sprintf "The following failure %s happen in function %s: \n" mustmay caller.svar.vname) in
                let _ = log (Printf.sprintf "Failing condition: %s\n" (To_string.bytes target.failing_condition)) in
                let _ = log (Printf.sprintf "Path condition: %s\n" (String.concat "&&" (List.map To_string.bytes state.path_condition))) in
                let _ = log (Printf.sprintf "Connection : %s\n" (To_string.bytes connecting_bytes)) in
                let _ = log (Printf.sprintf "Consult STP for an example...\n") in
                let valuesForSymbols = Stp.getAllValues (target.failing_condition::connecting_bytes::state.path_condition) in
                let getVal = function
                  | Bytes_ByteArray bytArr ->
                      let byteOptArray =
                        ImmutableArray.map
                          (function
                             | Byte_Symbolic s ->
                                 (try
                                    let valueForS = List.assq s valuesForSymbols in
                                      Some (make_Byte_Concrete valueForS)
                                  with Not_found -> None
                                 )
                             | _ -> failwith "Impossible: tracked symbolic value must be fully symbolic"
                          )
                          bytArr
                      in
                        if ImmutableArray.exists (* Check if any byte is constrained *)
                             (function Some _ -> true | _ -> false)
                             byteOptArray
                        then (Some (make_Bytes_ByteArray
                                  (ImmutableArray.map
                                     (function Some b -> b | None -> byte__zero)
                                     byteOptArray))
                        ) 
                        else None
                  | _ -> failwith "Impossible: symbolic bytes must be a ByteArray"
                in
                let _ = List.iter 
                          ( fun (bytes,varinf) -> 
                              match getVal bytes with 
                                | None -> () 
                                | Some concreteByteArray -> 
                                    (
                                      match bytes_to_constant concreteByteArray varinf.vtype with
                                        | CInt64 (n,_,_) ->
                                            log (Printf.sprintf "%s=%Ld\n" varinf.vname n)
                                        | _ -> failwith "Unimplemented: non-integer symbolic"
                                    )
                          )
                          hist.bytesToVars
                in
                let _ = log "(****************************\n" in
                  ()
              in
                match truth with 
                  | Ternary.True -> true,Bytes.bytes__zero
                  | Ternary.Unknown -> 
                      print_failed_assertion true; false,total_failing_condition
                  | Ternary.False -> 
                      print_failed_assertion false; false,total_failing_condition
            )
      ) (true,Bytes.bytes__zero) fundecs
  in
  let rec pass_targets targets =
    match targets with 
      | [] -> true,Bytes.bytes__zero
      | t::ts -> 
          let truth,failing_condition = pass_target t in
          if truth then pass_targets ts 
          else false,failing_condition (* TODO: can proceed, to find more failing targets *)
  in
    pass_targets targets


exception Failure_wc of string * bytes
let failwith_wc str cond = raise (Failure_wc (str,cond))

let step_job_with_targets targets job =
  (* if job meets one of the targets, do checking *)
  (* if fails, return Complete Abandoned
   * else step_job job *)
	setRunningJob job;
	try
        (* let _ = Output.printf "Step into Job %d\n" job.jid in *)
		let result = match (get_proc_info job).instrList with
			| [] -> exec_stmt job
			| Call(_,fexp,exps,_)::_-> 
                if Executeargs.run_args.arg_callchain_backward then
                  begin
                    let truth,failing_condition = pass_targets targets job fexp exps in
                      if truth then exec_instr job 
                      else failwith_wc (Printf.sprintf "Job %d hits the failing condition" job.jid ) failing_condition
                  end
                else exec_instr job
			| _ -> exec_instr job
		in
          result
	with 
      | Failure msg ->
		if run_args.arg_failfast then failwith msg;
		let result = { result_state = job.state; result_history = job.exHist } in
		let completed = Complete (
			{
				job = job;
				reason = Types.Abandoned (msg, !Output.cur_loc, result);
			}) 
		in
		completed
      | Failure_wc (msg,failing_condition) ->
		if run_args.arg_failfast then failwith msg;
        let state = {job.state with path_condition = failing_condition::job.state.path_condition} in
		let result = { result_state = state; result_history = job.exHist } in
		let completed = Complete (
			{
				job = job;
				reason = Types.Abandoned (msg, !Output.cur_loc, result); 
			}) 
		in
		completed




let at_merge_point job =
	StmtInfoSet.mem
		{ siFuncName=(List.hd (get_callstack job.state)).Cil.svar.Cil.vname;
		  siStmt=(get_proc_info job).stmt; }
		job.mergePoints



(** The main loop
  *)
let main_loop ?targets:(targets=[]) job : job_completion_reason list =
  let step_job = step_job_with_targets targets in
  let rec main_loop (completed: job_completion_reason list) (jobs: Jobs.t) : job_completion_reason list =
    match !signalStringOpt with
      | Some s ->
          (* if we got a signal, stop and return the completed results *)
          Output.set_mode Output.MSG_MUSTPRINT;
          Output.print_endline s;
          completed
      | None ->
          begin
            if Jobs.has_next_runnable jobs then
              begin
                let job = Jobs.take_next_runnable jobs in
                  if run_args.arg_merge_paths && at_merge_point job then
                    (* job is at a merge point and merging is enabled: try to merge it *)
                    begin match Jobs.merge jobs job with
                      | Some (truncated) ->
                          (* merge was successful: process the result and continue *)
                          process_result completed jobs truncated
                      | None ->
                          (* merge was unsuccessful: keep the job at the merge point in the merge set in case
                           * later jobs can merge; this leads to the invariant that no jobs in the merge set
                           * can merge with each other *)
                          main_loop completed jobs
                    end
                  else
                      (* job is not at a merge point or merging is disabled: step the job *)
                      (* process_result completed (job_queue, merge_set) (step_job job) *)
                      let _ = Jobs.running jobs job in (* set current job *)
                      process_result completed jobs (step_job job )
              end
            else if Jobs.has_next_mergable jobs then
              begin
                (* job queue is empty: take a job out of the merge set and step it, since it cannot merge
                 * with any other jobs in the merge set (the merge set invariant) *)
                let job = Jobs.take_next_mergable jobs in
                let _ = Jobs.running jobs job in (* set current job *)
                  process_result completed jobs (step_job job)
              end
            else
              begin
                Output.set_mode Output.MSG_MUSTPRINT;
                Output.print_endline "Done executing";
                completed
              end
          end

   and output_completion_info completion =
     (* log some interesting errors *)
     begin match completion with
       | Types.Abandoned (msg, loc, { result_state=state; result_history=hist }) ->
           Output.set_mode Output.MSG_MUSTPRINT;
           Output.printf "Error \"%s\" occurs at %s\n%sAbandoning path\n"
             msg (To_string.location loc)
             (if Executeargs.print_args.arg_print_callstack then
                "Call stack:\n"^(To_string.callstack (get_callContexts state))
              else
                "");
       | _ ->
           ()
     end

	and process_result completed jobs job = 
      match job with
     | Active job ->
         Jobs.add_runnable jobs (next_proc job);
         main_loop completed jobs 

     | Fork (j1, j2) ->
         (* queue the true branch and continue the false branch *)  (* CCBSE *)
         Jobs.add_runnables jobs [(next_proc j1);(next_proc j2)];
         main_loop completed jobs

     | Big_Fork job_list -> 
		let rec process_job_list job_list =
			match job_list with
				| [] -> []
				| (job_state::t) ->
					match job_state with
						| Active job -> (next_proc job)::(process_job_list t)
						| Fork (j1, j2) -> (next_proc j1)::(next_proc j2)::(process_job_list t)
						| Big_Fork l -> failwith "Unexpected nested Big_Fork."(*(process_job_list l)@(process_job_list t)*)
						| Complete completion -> 
							(if (completion.job.num_procs > 1) then
								(kill_proc completion.job)::(process_job_list t) (*attempt to continue executing other processes*)
							else
								(process_job_list t))
		in
		let rec process_completion_list job_list =
			match job_list with
				| [] -> []
				| (job_state::t) ->
					match job_state with
						| Active job -> (process_completion_list t)
						| Fork (j1, j2) -> (process_completion_list t)
						| Big_Fork l -> failwith "Unexpected nested Big_Fork."(*(process_completion_list l)@(process_completion_list t)*)
						| Complete completion ->
							(output_completion_info completion.reason);
							(if (completion.job.num_procs > 1) then
								(process_completion_list t)
							else
								completion.reason::(process_completion_list t))
		in
            	let _ = Jobs.add_runnables jobs (process_job_list job_list) in
		main_loop ((process_completion_list job_list)@completed) jobs

     | Complete completion ->
			(output_completion_info completion.reason);
			(if (completion.job.num_procs > 1) then
				let _ = Jobs.add_runnable jobs (kill_proc completion.job) in
				main_loop completed jobs (*attempt to continue executing other processes*)
			else
				main_loop (completion.reason::completed) jobs)
	in
  let jobs = Jobs.create targets in
  let _ = Jobs.add_runnable jobs job in
	main_loop [] jobs


let callchain_bacward_se callergraph entryfn assertfn job_init : job_completion_reason list list =
  let job_init fn ts =
    let _ = Output.banner_printf 1 "Start forward SE on function %s with target(s)\n%s\n%!"
            (fn.svar.vname) (let s=(String.concat "," (List.map (fun t -> t.func.svar.vname) ts)) in if s="" then "(none)" else s)
    in
    job_init fn
  in
  let get_failing_condition result = 
    List.fold_left 
      ( fun b job_completion ->
          match job_completion with
            | Abandoned (_,_,job_result) ->
                let this_fc = List.fold_left Operation.bytes__land Bytes.bytes__one job_result.result_state.path_condition in
                  Operation.bytes__lor b this_fc 
            | _ -> b
      )
      Bytes.bytes__zero result
  in

  (* The implementation of main loop *)
  let rec callchain_bacward_main_loop job targets =
    (* Assume we start at f *)
    let f = List.hd (get_callstack job.state) in
    (* Run forward SE based on the targets *)
    let result = main_loop ~targets:targets job in 
    (* result is a (may not be completed) list of finished jobs.
     * A job is either successful, if no assertion failure, or unsuccessful.
     *)
    (* Get a failing condition *)
    let failing_condition = get_failing_condition result in
      if f == entryfn then 
        (* If f is main(), we are done *)
        result 
      else
        let new_target = {
          func = f;
          entry_state = job.state;
          failing_condition = failing_condition;
        } in
        let callers = Cilutility.get_callers callergraph f in
          Output.banner_printf 1 "Function %s's caller(s): " f.svar.vname;
          List.iter (fun caller -> Output.banner_printf 1 " %s\n" caller.svar.vname) callers;
          Output.banner_printf 1 "%!";
          List.fold_left 
          (
            fun lst caller -> 
              let targets = (new_target::targets) in
              let newjob = job_init caller targets in
              let newlst = callchain_bacward_main_loop newjob targets  in
                List.rev_append newlst lst
          ) 
          [] callers
  in
  let callers = Cilutility.get_callers callergraph assertfn in
    List.fold_left 
      (fun results caller ->
         Output.banner_printf 2 "Call-chain backward Symbolic Execution of target function %s\n%!" caller.svar.vname;
         let job = job_init caller [] in 
         let new_result = callchain_bacward_main_loop job [] in
           new_result::results
      ) [] callers



