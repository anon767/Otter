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

	(* Then we make a bytes (specifically, a make_Bytes_ByteArray) out of this string *)
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
					make_Bytes_Address (Some argv_strings_block, Convert.lazy_int_to_bytes charsSoFar) in
				impl t (ptrsSoFar + 1)
					(charsSoFar + String.length h + 1 (* '+ 1' for the null character *))
					(MemOp.bytes__write bytes (Convert.lazy_int_to_bytes (ptrsSoFar * word__size)) word__size h_bytes)
	in
	let argv_ptrs_bytes =
		impl argstr 0 0 (make_Bytes_ByteArray (ImmutableArray.make (num_args * word__size) MemOp.byte__zero)) in

	(* Map the pointers block to its bytes *)
	let state'' = MemOp.state__add_block state' argv_ptrs_block argv_ptrs_bytes in

	(* Make the top-level address that is the actual argv. It is the address of
		 argv_ptrs_bytes. We do not have to map this to anything; we just pass it as the
		 argument to main. *)
	let argv = make_Bytes_Address (Some argv_ptrs_block, MemOp.bytes__zero) in

	(* Finally, return the updated state and the list of arguments *)
	(state'', [argc; argv])
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
	end file.globals;

	(* Find all executable lines. For now, we don't care about the rest *)
	let (setOfLines,_,setOfEdges,setOfConds) = GetProgInfo.getProgInfo file run_args.arg_fns in
	run_args.arg_total_lines <- LineSet.cardinal setOfLines;
	run_args.arg_total_edges <- EdgeSet.cardinal setOfEdges;
	run_args.arg_total_conds <- CondSet.cardinal setOfConds;
	Output.printf "This program contains %d executable lines within the functions specified\n"
		run_args.arg_total_lines;
	Output.printf "This program contains %d executable edges within the functions specified\n"
		run_args.arg_total_edges;
	Output.printf "This program contains %d executable conditions within the functions specified\n"
		run_args.arg_total_conds;
	if run_args.arg_list_executable_lines then (
		LineSet.iter
			(fun (file,lineNum) -> Output.printf "%s:%d\n" file lineNum)
			setOfLines	
	)
	else
	if run_args.arg_list_executable_edges then (
		EdgeSet.iter
			(fun (b_stmt,e_stmt) -> Output.printf "%d-%d\n" b_stmt.sid e_stmt.sid)
		setOfEdges
	)
	else
	if run_args.arg_list_executable_lines then (
		CondSet.iter
	  	(fun (condition, location, truth) -> Output.printf "%s (%s) %s\n" (To_string.location location) (To_string.exp condition) truth)
		setOfConds
	)

(* create a job that begins at a function, given an initial state *)
let job_for_function state fn argvs =
	let state = MemOp.state__start_fcall state Runtime fn argvs in
	(* create a new job *)
	{ state = state;
	  exHist = emptyHistory;
	  instrList = [];
	  stmt = List.hd fn.sallstmts;
	  inTrackedFn = StringSet.mem fn.svar.vname run_args.arg_fns;
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

	Output.print_endline (Executedebug.get_log ());
		(* function stat 
		Output.print_endline "\nFunction call stat:";
		Cilutility.FundecMap.iter (fun f c -> Output.print_endline ((To_string.fundec f)^" : "^(string_of_int c))) (!MemOp.function_stat);	
		*)
	Output.printf "\nSTP was invoked %d times. (%d cache hits; %d misses)\n" !Types.stp_count !Stp.cacheHits !Stp.cacheMisses;

	let executionTime = (Unix.gettimeofday ()) -. startTime
	and stpTime = Stats.lookupTime "STP" in
	Output.printf "It ran for %.2f s, which is %.2f%% of the total %.2f s execution.\n"
		stpTime (100. *. stpTime /. executionTime) executionTime;
	Output.printf "  It took %.2f s to construct the formulas for the expressions inside 'if(...)'s,
  %.2f s to construct and %.2f s to assert the path conditions,
  and %.2f s to solve the resulting formulas.\n\n"
		(Stats.lookupTime "convert conditional")
		(Stats.lookupTime "STP construct")
		(Stats.lookupTime "STP doassert")
		(Stats.lookupTime "STP query");
    Output.printf "Hash-consing: hits=%d misses=%d\n" (!Types.hash_consing_bytes_hits) (!Types.hash_consing_bytes_misses);
    Output.printf "Bytes eval caching: hits=%d misses=%d\n\n" (!MemOp.bytes_eval_cache_hits) (!MemOp.bytes_eval_cache_misses);
    (*
    Hashtbl.iter (fun a b -> print_endline (To_string.bytes b)) Types.hash_consing_bytes_tbl; 
     *)

	Report.print_report results
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
			("--printNoEscapedString",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_no_escaped_string <- true),
			" Print strings without escaped characters \n");
			("--printCallStack",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_callstack <- true),
			" Print call stack (when branching)\n");
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

			("--condCov",
			 Arg.Unit (fun () -> run_args.arg_cond_coverage <- true),
			 " Track condition coverage\n");
			("--edgeCov",
			 Arg.Unit (fun () -> run_args.arg_edge_coverage <- true),
			 " Track edge coverage\n");
			("--stmtCov",
			 Arg.Unit (fun () -> run_args.arg_stmt_coverage <- true),
			 " Track statement coverage\n");
			("--lineCov",
			 Arg.Unit (fun () -> run_args.arg_line_coverage <- true),
			 " Track line coverage\n");
			("--covStats",
			 Arg.String readCovStatsFromFile,
			 "<filename> File containing coverage statistics\n");
			("--listAllExecutableLines",
			 Arg.Unit (fun () -> run_args.arg_list_executable_lines <- true),
			 " Before execution, print out all of the executable lines in the program.\n");

			("--mergePaths",
			 Arg.Unit (fun () -> run_args.arg_merge_paths <- true),
			 " Merge similar execution paths\n");

			("--timeout",
			 Arg.Int (fun n -> run_args.arg_timeout <- n),
			 "<numSeconds> Set a timeout for the executor\n");

			("--marshalCoverageTo",
			 Arg.String (fun str -> run_args.arg_marshal_file <- str),
			 "<file> Marshal coverage information to <file>.\n");

			("--calculateDepsDuringExecution",
			 Arg.Unit (fun () -> run_args.arg_calculate_dependencies <- true),
			 " Calculate (at the end of symbolic execution) what lines depend on what symbolic variables\n");

			("--marshalFrom",
			 Arg.String
				 (fun filename ->
						let inChan = open_in_bin filename in
						let coverage = (Marshal.from_channel inChan : job_result list) in
						ignore coverage (* Do something with the coverage information *)
				 ),
			 "<filename> Read coverage information from an output file.");
		];
		fd_post_check = true;
    fd_doit = doExecute
  } 
	;;
