open Cil
open Types
open Executeargs

let rec init_globalvars state globals =
	match globals with
		| [] -> state
		| GVar(varinfo, initinfo, loc):: tail ->
				Output.set_mode Output.MSG_MUSTPRINT;
					begin
					let lhost_typ = varinfo.vtype in
					let size = (Cil.bitsSizeOf (varinfo.vtype)) / 8 in
					let zeros = MemOp.bytes__make size in
					let init_bytes = match initinfo.init with
						| None -> zeros
						| Some(init) ->
								(* make the offset argument "accumulative",
								   i.e. not following the spec of Cil's offset in init type
								 *)
								let rec accumulate_offset offset off' =
									match offset with
										| NoOffset -> off'
										| Field(f,offset2) -> Field(f,accumulate_offset offset2 off')
										| Index(exp,offset2) -> Index(exp,accumulate_offset offset2 off')
								in
								let rec myInit  (offset:Cil.offset) (i:Cil.init) acc =
									match i with 
										| SingleInit(exp) -> 
											let (off,typ) = Eval.flatten_offset state lhost_typ offset in
											MemOp.bytes__write acc off (Cilutility.bitsSizeOfExp exp) (Eval.rval state exp)
										| CompoundInit(typ, list) ->          
											foldLeftCompound 
												~implicit: false
												~doinit: (fun off' i' t' acc ->	myInit  (accumulate_offset offset off') i' acc)
												~ct: typ
												~initl: list
												~acc: acc
								in
									myInit NoOffset init zeros
					in
					Output.set_mode Output.MSG_REG;
					Output.print_endline ("Initialize "^varinfo.vname^" to "
						^(if init_bytes == zeros then "zeros" else To_string.bytes init_bytes)
					);					
					let state2 = MemOp.state__add_global state varinfo init_bytes in
					init_globalvars state2 tail
				end
		| GVarDecl(varinfo, loc):: tail when (not(Cil.isFunctionType varinfo.vtype)) ->
				(* I think the list of globals is always in the same order as in the source
					 code. In particular, I think there will never be a declaration of a
					 variable after that variable has been defined, since CIL gets rid of
					 such extra declarations. If this is true, then this should work fine. If
					 not, a declaration occuring *after* a definition will wipe out the
					 definition, replacing the value with zeros. *)
				Output.set_mode Output.MSG_REG;
				Output.print_endline ("Initialize "^varinfo.vname^" without initial value"
					(*^(To_string.bytes init)*)
				);
				let size = (Cil.bitsSizeOf (varinfo.vtype)) / 8 in
				let size = if size <= 0 then 1 else size in
				let init_bytes = MemOp.bytes__make size (* zeros *)
				in
				let state2 = MemOp.state__add_global state varinfo init_bytes in
				init_globalvars state2 tail
		| _:: tail -> init_globalvars state tail
;;

(* To initialize the arguments, we need to create a bytes which represents argc and
	 one which represents argv. The one for argc is simple: count how many arguments
	 and make that value into a bytes. For argv, we need a bytes which is the address
	 of a byteArray of byte-s, each of which is the address of some offset into one
	 big memory block which holds all of the arguments (as strings). This last memory
	 block is itself a byteArray of concrete byte-s (the characters in the arguments).
	 Because of the way things point to each other, we construct these three layers
	 bottom-up. *)
let init_cmdline_argvs state argstr =
	(* How many arguments were there? *)
	let num_args = List.length argstr in

	(* Convert the number of arguments into a 'bytes' *)
	let argc = Convert.lazy_int_to_bytes num_args in

	(* C's standard is to have the arguments be consecutive strings. For example, if the
		 executed code were "./run abc de fgh", this would lead to the following chunk of
		 memory:
		 ['.','/','r','u','n','\000','a','b','c','\000','d','e','\000','f','g','h','\000']
		 Let's create this bytes. *)
	(* First we make the concatenated string *)
	let argv_strings = String.concat "\000" argstr in

	(* Then we make a bytes (specifically, a Bytes_ByteArray) out of this string *)
	let argv_strings_bytes = Convert.string_to_bytes argv_strings in

	(* Make a block to point to this bytes. *)
	(* The block's size will be one more than the length of argv_strings (because of the
		 terminating null byte). *)
	let argv_strings_block = MemOp.block__make "argv_strings" ((String.length argv_strings) + 1) Block_type_Local in

	(* Map the block we just made to the bytes we just made *)
	let state' = MemOp.state__add_block state argv_strings_block argv_strings_bytes in

(* TODO: argv[argc] is supposed to be a null pointer. [Standard 5.1.2.2.1] *)
	(* Now, make a block for the array of pointers, with room for a pointer for each argument *)
	let argv_ptrs_block = MemOp.block__make "argv_pointers" (num_args * word__size) Block_type_Local in

	(* Make the byteArray of pointers by making each individual pointer and putting them
		 into the array using MemOp's bytes__write function. *)
	let rec impl argstr ptrsSoFar charsSoFar bytes : bytes =
		match argstr with
			[] -> bytes
		| h::t ->
				(* Print out each argument *)
				Output.set_mode Output.MSG_DEBUG;
				Output.print_endline ("With arguments: \""^h^"\"");
				let h_bytes =
					Bytes_Address (Some argv_strings_block, Convert.lazy_int_to_bytes charsSoFar) in
				impl t (ptrsSoFar + 1)
					(charsSoFar + String.length h + 1 (* '+ 1' for the null character *))
					(MemOp.bytes__write bytes (Convert.lazy_int_to_bytes (ptrsSoFar * word__size)) word__size h_bytes)
	in
	let argv_ptrs_bytes =
		impl argstr 0 0 (Bytes_ByteArray (ImmutableArray.make (num_args * word__size) MemOp.byte__zero)) in

	(* Map the pointers block to its bytes *)
	let state'' = MemOp.state__add_block state' argv_ptrs_block argv_ptrs_bytes in

	(* Make the top-level address that is the actual argv. It is the address of
		 argv_ptrs_bytes. We do not have to map this to anything; we just pass it as the
		 argument to main. *)
	let argv = Bytes_Address (Some argv_ptrs_block, MemOp.bytes__zero) in

	(* Finally, return the updated state and the list of arguments *)
	(state'', [argc; argv])
;;

let rec allSymbols = function
	| Bytes_Constant const -> SymbolSet.empty
	| Bytes_ByteArray bytearray ->
			ImmutableArray.fold_left
				(fun symbSet byte -> match byte with
					 | Byte_Concrete _ -> symbSet
					 | Byte_Symbolic symb -> SymbolSet.add symb symbSet
					 | Byte_Bytes (bytes,_) -> SymbolSet.union symbSet (allSymbols bytes))
				SymbolSet.empty
				bytearray
	| Bytes_Address (memBlockOpt,bytes) -> (
			let partialAnswer = allSymbols bytes in
			match memBlockOpt with
					None -> partialAnswer
				| Some memBlock ->
						SymbolSet.union partialAnswer (allSymbols memBlock.memory_block_addr)
		)
	| Bytes_Op (_,bytes_typ_list) ->
			List.fold_left
				(fun symbSet (b,_) -> SymbolSet.union symbSet (allSymbols b))
				SymbolSet.empty
				bytes_typ_list
	| Bytes_Read (bytes1,bytes2,_) ->
			SymbolSet.union (allSymbols bytes1) (allSymbols bytes2)
	| Bytes_Write (bytes1,bytes2,_,bytes3) ->
			SymbolSet.union
				(allSymbols bytes3)
				(SymbolSet.union (allSymbols bytes1) (allSymbols bytes2))
	| Bytes_FunPtr (_,bytes) -> allSymbols bytes

let allSymbolsInList byteslist =
	List.fold_left
		(fun symbSet b -> SymbolSet.union symbSet (allSymbols b))
		SymbolSet.empty
		byteslist

let finish_up results =
	if run_args.arg_branch_coverage then
		begin
			print_endline "Branch coverage:";
			let hashtblAsList =
				Hashtbl.fold
					(fun a b acc -> (a,b) :: acc)
					branches_taken
					[]
			and cmpByLoc ((_,loc1),_) ((_,loc2),_) = Cilutility.compareLoc loc1 loc2
			in
			let sortedList = List.sort cmpByLoc hashtblAsList and
					printPcHistSet pcHistSet =
						let counter = ref 0 in
						PcHistSet.iter
							(fun (pc,hist) ->
								let str = To_string.humanReadablePc pc hist.bytesToVars in
								counter := !counter + 1;
								print_endline ("Condition " ^ (string_of_int !counter) ^ ":");
								print_endline (if str = "" then "true" else str);
								print_newline ())
							pcHistSet
			in
			List.iter
				(fun ((exp,loc), (true_pcHistSet_ref,false_pcHistSet_ref)) ->
					print_endline ((To_string.location loc) ^ ", " ^ (To_string.exp exp));
					if not (PcHistSet.is_empty !true_pcHistSet_ref) then
						(print_endline "True branch taken under the following conditions:";
						 printPcHistSet !true_pcHistSet_ref);
					if not (PcHistSet.is_empty !false_pcHistSet_ref) then
						(print_endline "False branch taken under the following conditions:";
						 printPcHistSet !false_pcHistSet_ref);
					print_newline ())
				sortedList
		end;

	let coverage, completed, truncated, abandoned =
		List.fold_left begin fun (coverage, completed, truncated, abandoned) result ->
			match result with
				| Types.Return (_, c)
				| Types.Exit (_, c)      -> (c::coverage, completed + 1, truncated, abandoned)
				| Types.Truncated (c, d) -> (c::d::coverage, completed, truncated + 2, abandoned)
				| Types.Abandoned _      -> (coverage, completed, truncated, abandoned + 1)
	end ([], 0, 0, 0) results in
	if completed = 0 then (
		Output.printf "All %d paths had errors.\n" abandoned
	) else (
		Output.printf "%d paths ran to completion; %d had errors.\n" completed abandoned;
		Output.printf "There are %d truncated paths.\n" truncated;

	let (alwaysExecuted,everExecuted) =
		match coverage with
			|  hd::tl ->
				let first_edges = hd.result_history.edgesTaken in
				List.fold_left
					(fun (interAcc,unionAcc) { result_history=hist } ->
						 EdgeSet.inter interAcc hist.edgesTaken,
						 EdgeSet.union unionAcc hist.edgesTaken)
					(first_edges, first_edges)
					tl
			| [] -> assert false (* there has to be at least one execution *)
	in

	print_string "\nIn all, ";
	print_int (EdgeSet.cardinal everExecuted);
	print_string " edges were executed, of which ";
	print_int (EdgeSet.cardinal alwaysExecuted);
	print_endline " were always executed.\n";
    begin if Executeargs.print_args.arg_print_reg then begin
	let greedySetCover coverageList =
		let rec helper acc cvrgList remaining =
			if EdgeSet.is_empty remaining then acc
			else (
				match cvrgList with
					| [] -> failwith "Impossible to cover universe."
					| h::t ->
						let nextPick = ref h in
						let score x = EdgeSet.cardinal (EdgeSet.inter x.result_history.edgesTaken remaining) in
						let nextPickScore = ref (score h) in
						List.iter
							(fun s -> let sScore = score s in
							 if sScore > !nextPickScore
							 then (nextPickScore := sScore; nextPick := s))
							cvrgList;
						Output.printf "Covering %d new edges\n" !nextPickScore;
						helper (!nextPick::acc)
							(List.filter ((!=) !nextPick) cvrgList)
							(EdgeSet.diff remaining (!nextPick.result_history).edgesTaken)
			)
		in helper [] coverageList (EdgeSet.diff everExecuted alwaysExecuted)
	in
	let coveringSet = greedySetCover coverage in
	if coveringSet = [] then print_endline "No constraints: any run covers all edges"
	else (
		Output.printf "Here is a set of %d configurations which covers all the edges:\n"
			(List.length coveringSet);
		Output.set_mode Output.MSG_MUSTPRINT;
		List.iter
			(fun { result_state={ path_condition=pc }; result_history=hist } ->
				 print_newline ();
				 print_endline (To_string.humanReadablePc pc hist.bytesToVars ^ "\n");

				 let mentionedSymbols = allSymbolsInList pc in
				 let valuesForSymbols = Stp.getValues pc (SymbolSet.elements mentionedSymbols) in

				 (* Keep track of which symbols we haven't given values to.
						This would happen if there are untracked symbolic values
						in the path condition. *)
				 let unboundSymbols = ref mentionedSymbols in

				 (* Get the value of a symbolic ByteArray *)
				 let getVal = function
					 | Bytes_ByteArray bytArr ->
							 let byteOptArray =
								 ImmutableArray.map
									 (function
											| Byte_Symbolic s ->
													(try
														 let valueForS = List.assq s valuesForSymbols in
														 (* Now s is bound *)
														 unboundSymbols := SymbolSet.remove s !unboundSymbols;
														 Some (Byte_Concrete valueForS)
													 with Not_found -> None)
											| _ -> failwith "Impossible: tracked symbolic value must be fully symbolic")
									 bytArr
							 in
							 if ImmutableArray.exists (* Check if any byte is constrained *)
								 (function Some _ -> true | _ -> false)
								 byteOptArray
							 then (
								 (* Return a Some with the bytearray, filling in
										unconstrained bytes with 0. *)
								 Some (Bytes_ByteArray
												 (ImmutableArray.map
														(function Some b -> b | None -> MemOp.byte__zero)
														byteOptArray))
							 ) else (
								 (* Return None for a totally unconstrained value *)
								 None
							 )
					 | _ -> failwith "Impossible: symbolic bytes must be a ByteArray"
				 in

				 print_endline "For example:";
				 List.iter
					 (fun (bytes,varinf) ->
							match getVal bytes with
								| None -> () (* Don't print anything for an unconstrained value *)
								| Some concreteByteArray ->
										match Convert.bytes_to_constant concreteByteArray varinf.vtype with
											| CInt64 (n,_,_) ->
													(* Is it okay to ignore the type? Or might we have to truncate? *)
													Output.printf "%s=%Ld\n" varinf.vname n
											| _ -> failwith "Unimplemented: non-integer symbolic")
					 hist.bytesToVars;

				 (* Check to see if we've bound all of the symbols in the path
						condition *)
				 if not (SymbolSet.is_empty !unboundSymbols)
				 then (
					 print_endline "but these symbolic values are unaccounted for by tracked variables:";
					 SymbolSet.iter
						 (fun s -> print_int s.symbol_id; print_string " ")
						 !unboundSymbols
				 );
				 print_newline ())
			coveringSet
	);
    ()end else () end;	
	);
	print_endline "Finished.";
	()
;;

let computeJoinPointsForIfs fundec =
	(* Compute the dominators for the function *)
	let idomHashtbl = Dominators.computeIDom ~doCFG:false fundec in
	(* Iterate through all statements in the function *)
	List.iter
		(fun s ->
			 (* For each statement, see if it is a join-point (i.e., has > 1 pred) *)
			 match s.preds with
					 [] | [_] -> () (* Ignore non-join points *)
				 | _ ->
						 let rec getClosestDominatingIf stmt =
							 match Dominators.getIdom idomHashtbl stmt with
									 None -> None (* s is not dominated by any [If]s *)
								 | Some dom ->
										 (match dom.skind with
												| If _ -> Some dom (* Aha! [dom] is the closest dominating [If] *)
												| _ -> getClosestDominatingIf dom) (* Not an [If]; get its dominator *)
						 in
						 match getClosestDominatingIf s with
								 None -> () (* No dominating [If]; do nothing *)
							 | Some ifDom ->
									 (* Add this join point to the dominating [If]'s list *)
									 Inthash.add ifToJoinPointsHash ifDom.sid s.sid)
		fundec.sallstmts
;;

(* set up the file for symbolic execution *)
let prepare_file file =
	Cilly.makeCFGFeature.fd_doit file;

	(* TODO: move this out of global variable *)
	(* Hash all of the fundecs by their varinfos. Also, compute the join points which each [If] dominates.*)
	List.iter begin function
		| GFun(fundec,_) ->
			Hashtbl.add Cilutility.fundecHashtbl fundec.svar fundec;
			computeJoinPointsForIfs fundec
		| _ -> ()
	end file.globals


(* create a job that begins at a function, given an initial state *)
let job_for_function state fn argvs =
	let state = MemOp.state__start_fcall state Runtime fn argvs in
	(* create a new job *)
	{ state = state;
	  exHist = emptyHistory;
	  prevStmt = Cil.dummyStmt;
	  nextStmt = List.hd fn.sallstmts;
	  mergePoints = IntSet.empty;
	  jid = Utility.next_id Output.jidCounter }


(* create a job that begins at the main function of a file, with the initial state set up for the file *)
let job_for_file file cmdline =
	let main_func =
		try Function.from_name_in_file "main" file
		with Not_found -> failwith "No main function found!"
	in

	(* Initialize the state *)
	let state = MemOp.state__empty in
	let state = init_globalvars state file.globals in

	(* prepare the command line arguments *)
	let state, main_args = init_cmdline_argvs state cmdline in

	(* create a job starting at main *)
	job_for_function state main_func main_args


let doExecute (f: file) =
	Output.set_mode Output.MSG_REG;
	Output.print_endline "\nA (symbolic) executor for C\n";

	(* Keep track of how long we run *)
	let startTime = Unix.gettimeofday () in

	(* Set signal handlers to catch timeouts and interrupts *)
	let old_ALRM_handler =
		Sys.signal Sys.sigalrm
			(Sys.Signal_handle (fun _ -> signalStringOpt := Some "Timed out!"))
	and old_INT_handler =
		Sys.signal Sys.sigint
			(Sys.Signal_handle (fun _ -> signalStringOpt := Some "User interrupt!"))
	in
	(* Set a timer *)
	ignore (Unix.alarm Executeargs.run_args.arg_timeout);

	(* prepare the file for symbolic execution *)
	prepare_file f;

	(* create a job for the file, with the commandline arguments set to the file name and the arguments from the
	   '--arg' option *)
	let file_job = job_for_file f (f.fileName::Executeargs.run_args.arg_cmdline_argvs) in

	(* run the job *)
	let results = Driver.main_loop file_job in

	(* Turn off the alarm and reset the signal handlers *)
	ignore (Unix.alarm 0);
	Sys.set_signal Sys.sigalrm old_ALRM_handler;
	Sys.set_signal Sys.sigint old_INT_handler;

	(* Stop if arg_print_nothing is true *)
	if not print_args.arg_print_nothing then (

		print_endline (Executedebug.get_log ());
		(* function stat 
		Output.print_endline "\nFunction call stat:";
		Cilutility.FundecMap.iter (fun f c -> Output.print_endline ((To_string.fundec f)^" : "^(string_of_int c))) (!MemOp.function_stat);	
		*)
		Output.printf "\nSTP was invoked %d times.\n" !Types.stp_count;

		let executionTime = (Unix.gettimeofday ()) -. startTime
		and stpTime = Stats.lookupTime "STP" in
		Output.printf "It ran for %.2f s, which is %.2f%% of the total %.2f s execution.\n"
			stpTime (100. *. stpTime /. executionTime) executionTime;
		Output.printf "  It took %.2f s to construct the formulas for the expressions inside 'if(...)'s,
  %.2f s to construct and assert the path conditions,
  and %.2f s to solve the resulting formulas.\n\n"
			(Stats.lookupTime "convert conditional")
			(Stats.lookupTime "STP assert")
			(Stats.lookupTime "STP query");

	(*	let rep = Report.format state3 in
		Output.print_endline rep;*)
		finish_up results
	)
;;

let feature : featureDescr = 
  { fd_name = "execute";              
    fd_enabled = ref false;
    fd_description = "(symbolic) executor for C";
		fd_extraopt = [
			
			(**
					Running options
			 *)
			(* Regression test output *)
			("--regression",
			Arg.Unit (fun () -> Executeargs.run_args.arg_run_regression <- true),
			" Output for regression test \n");

			(**
					Printing options
			 *)
			(* TODO: for each msg type, a --print and --noprint option*)
			(* STP *)
			("--printSTP",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_stp <- true),
			" Print STP programs \n");
			(* Assignment in the form lval = rval *)
			("--printAssign",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_assign <- true),
			" Print assignments (from rval to lval) \n");

			("--printFunctionCall",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_func <- true),
			" Print function calls \n");
			(* Print the guard of an if statement *)
			("--printIf",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_guard <- true),
			" Print the guard of an if statement \n");
			(* Sparse printing *)
			("--printLittle",
			Arg.Unit (fun () -> 
				Executeargs.print_args.arg_print_reg <- false;
				Executeargs.print_args.arg_print_ifstmt <- false;
				Executeargs.print_args.arg_print_misc <- false;
				Executeargs.print_args.arg_print_stmt <- false;
				Executeargs.print_args.arg_print_func <- false;
				Executeargs.print_args.arg_print_assign <- false;
				()
			),
			" Suppress most output \n");

			("--printNothing",
			 Arg.Unit (fun () -> print_args.arg_print_nothing <- true),
			" Suppress (pretty much) all output. This trumps all other --print* options\n");

			("--printCharAsInt",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_char_as_int <- true),
			" Print char as int \n");
			(** 
					Argvs
			 *)
			("--arg",
			Arg.String (fun argv -> Executeargs.run_args.arg_cmdline_argvs <- Executeargs.run_args.arg_cmdline_argvs @ [argv]),
			"<argv> Run with command line argument <argv>
\t\t\t(This option can be repeated to give multiple arguments.)\n");

(*		  ("--symbolicExternFns",
			 Arg.Unit (fun () -> Executeargs.run_args.arg_symbolic_extern_fns <- true),
			 " Return a fresh symbolic value (instead of dying) upon encountering an undefined function. This effectively assumes that undefined functions are nondeterministic and side-effect-free, which is usually not true. (Note that ending program execution is a side-effect that will be ignored.)\n");
*)

			("--branchCoverage",
			 Arg.Unit (fun () -> run_args.arg_branch_coverage <- true),
			 " Track branch coverage\n");

			("--mergePaths",
			 Arg.Unit (fun () -> Executeargs.run_args.arg_merge_branches <- true),
			 " Merge similar execution paths\n");

			("--timeout",
			 Arg.Int (fun n -> Executeargs.run_args.arg_timeout <- n),
			 "<numSeconds> Set a timeout for the executor\n")
		];
		fd_post_check = true;
    fd_doit = doExecute
  } 
	;;
