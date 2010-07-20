open Cil
open Bytes
open BytesUtility
open Types
open Executeargs
open MemOp
(*open InvInput*)

let unreachable_global varinfo = not (Cilutility.VarinfoSet.mem varinfo (!GetProgInfo.reachable_globals))

let init_symbolic_pointer state varinfo size =
  (* TODO: what's the right size?
   * For now, assume that each pointer points to an array of size 1.
   *)
  let name = Printf.sprintf "Two-fold Sym Ptr (%s)" varinfo.vname in
  let block =  block__make name size Block_type_Aliased in 
  let addrof_block = make_Bytes_Address (block, bytes__zero) in
  let state = MemOp.state__add_block state block (bytes__symbolic size) in
    state,make_Bytes_Conditional (conditional__from_list [Unconditional bytes__zero; Unconditional addrof_block])


let init_symbolic_varinfo state varinfo =
  let typ = varinfo.vtype in
  let size = (Cil.bitsSizeOf (typ)) / 8 in
  let size = if size <= 0 then 1 else size in
    match typ with
      | TPtr (basetyp,_) -> 
          assert (size==4);
          Output.set_mode Output.MSG_REG;
          Output.printf "Initialize %s to ITE(?,null,non-null)\n" varinfo.vname;
          init_symbolic_pointer state varinfo size
      | _ ->
          Output.set_mode Output.MSG_REG;
          Output.printf "Initialize %s to symbolic\n" varinfo.vname;
          state,bytes__symbolic size 


let init_symbolic_globalvars state globals =
	List.fold_left begin fun state g -> match g with
		| GVar (varinfo, _, _)
		| GVarDecl (varinfo, _)
				when not (Cil.isFunctionType varinfo.vtype)
					 && not (Executeargs.run_args.arg_noinit_unreachable_globals && unreachable_global varinfo) ->
         let state,init_bytes = init_symbolic_varinfo state varinfo in
           MemOp.state__add_global state varinfo init_bytes
		| _ ->
			state
	end state globals



let init_globalvars state globals =
	List.fold_left begin fun state g -> match g with
		| GVar(varinfo, { init=Some init }, _)
				when not (Executeargs.run_args.arg_noinit_unreachable_globals && unreachable_global varinfo) ->
			Output.set_mode Output.MSG_MUSTPRINT;
			let lhost_typ = varinfo.vtype in
			let size = (Cil.bitsSizeOf (varinfo.vtype)) / 8 in
			let zeros = bytes__make size in
			let rec myInit (offset:Cil.offset) (i:Cil.init) (state, acc) =
				match i with
					| SingleInit(exp) ->
						let state, off, typ = Eval.flatten_offset state lhost_typ offset in
						let state, off_bytes = Eval.rval state exp in
						let size = (Cil.bitsSizeOf typ) / 8 in
						let init_bytes = bytes__write acc off size off_bytes in
						(state, init_bytes)
					| CompoundInit(typ, list) ->
						foldLeftCompound
							~implicit: false
							~doinit: (fun off' i' t' (state, acc) -> myInit (addOffset off' offset) i' (state, acc))
							~ct: typ
							~initl: list
							~acc: (state, acc)
			in
            let state, init_bytes = myInit NoOffset init (state, zeros) in

			Output.set_mode Output.MSG_REG;
			Output.printf "Initialize %s to %s\n" varinfo.vname
				(if init_bytes == zeros then "zeros" else To_string.bytes init_bytes);

            MemOp.state__add_global state varinfo init_bytes

		| GVar(varinfo, _, _)
		| GVarDecl(varinfo, _)
				when not (Cil.isFunctionType varinfo.vtype)
					 && not (Executeargs.run_args.arg_noinit_unreachable_globals && unreachable_global varinfo) ->
				(* I think the list of globals is always in the same order as in the source
					 code. In particular, I think there will never be a declaration of a
					 variable after that variable has been defined, since CIL gets rid of
					 such extra declarations. If this is true, then this should work fine. If
					 not, a declaration occuring *after* a definition will wipe out the
					 definition, replacing the value with zeros. *)
			let size = (Cil.bitsSizeOf (varinfo.vtype)) / 8 in
			let size = if size <= 0 then 1 else size in
			let init_bytes = bytes__make size (* zeros *) in

			Output.set_mode Output.MSG_REG;
			Output.printf "Initialize %s to zeros\n" varinfo.vname;

			MemOp.state__add_global state varinfo init_bytes

		| _ ->
			state
	end state globals



(* Initialize arguments for an entry function to purely symbolic *)
let init_symbolic_argvs state (entryfn:fundec) : (state*bytes list*executionHistory) =
  let state = state__add_frame state in
  let state,args,bytesToVars = 
    List.fold_right  (* TODO: Does ordering matter? *)
      (fun varinfo (state,args,bytesToVars) ->
        let state,init = init_symbolic_varinfo state varinfo in
        (state__add_formal state varinfo init,init::args,(init,varinfo)::bytesToVars)
      ) 
      entryfn.sformals
      (state,[],[])
  in
  let exHist = 
    {emptyHistory with
         bytesToVars = bytesToVars
    }
  in
  (state,args,exHist)


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
	let argc = lazy_int_to_bytes num_args in

	(* C's standard is to have the arguments be consecutive strings. For example, if the
		 executed code were "./run abc de fgh", this would lead to the following chunk of
		 memory:
		 ['.','/','r','u','n','\000','a','b','c','\000','d','e','\000','f','g','h','\000']
		 Let's create this bytes. *)
	(* First we make the concatenated string *)
	let argv_strings = String.concat "\000" argstr in

	(* Then we make a bytes (specifically, a make_Bytes_ByteArray) out of this string *)
	let argv_strings_bytes = string_to_bytes argv_strings in

	(* Make a block to point to this bytes. *)
	(* The block's size will be one more than the length of argv_strings (because of the
		 terminating null byte). *)
	let argv_strings_block = block__make "argv_strings" ((String.length argv_strings) + 1) Block_type_Local in

	(* Map the block we just made to the bytes we just made *)
	let state' = MemOp.state__add_block state argv_strings_block argv_strings_bytes in

    let charPtrSize = bitsSizeOf charPtrType / 8 in

(* TODO: argv[argc] is supposed to be a null pointer. [Standard 5.1.2.2.1] *)
	(* Now, make a block for the array of pointers, with room for a pointer for each argument *)
	let argv_ptrs_block = block__make "argv_pointers" (num_args * charPtrSize) Block_type_Local in

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
					make_Bytes_Address (argv_strings_block, lazy_int_to_bytes charsSoFar) in
				impl t (ptrsSoFar + 1)
					(charsSoFar + String.length h + 1 (* '+ 1' for the null character *))
					(bytes__write bytes (lazy_int_to_bytes (ptrsSoFar * charPtrSize)) charPtrSize h_bytes)
	in
	let argv_ptrs_bytes =
		impl argstr 0 0 (make_Bytes_ByteArray (ImmutableArray.make (num_args * charPtrSize) byte__zero)) in

	(* Map the pointers block to its bytes *)
	let state'' = MemOp.state__add_block state' argv_ptrs_block argv_ptrs_bytes in

	(* Make the top-level address that is the actual argv. It is the address of
		 argv_ptrs_bytes. We do not have to map this to anything; we just pass it as the
		 argument to main. *)
	let argv = make_Bytes_Address (argv_ptrs_block, bytes__zero) in

	(* Finally, return the updated state and the list of arguments *)
	(state'', [argc; argv])


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
									 Hashtbl.add ifToJoinPointsHash
										 { siFuncName = fundec.svar.vname ; siStmt = ifDom }
										 { siFuncName = fundec.svar.vname ; siStmt = s })
		fundec.sallstmts


(* set up the file for symbolic execution *)
let prepare_file file =
	(* makeCFGFeature must precede the call to getProgInfo. *)
	Cilly.makeCFGFeature.fd_doit file;

	Cil.iterGlobals file begin function
		| GFun(fundec,_) ->
			(* Reset sids to be unique only within a function, rather than globally, so that they will be given
			   consistent sids across different analysis runs even if different files are merged
			 *)
			ignore (List.fold_left (fun n stmt -> stmt.sid <- n; succ n) 0 fundec.sallstmts);

			(* Compute the join points which each [If] dominates *)
			computeJoinPointsForIfs fundec

		| _ ->
			()
	end;

    if Executeargs.run_args.arg_noinit_unreachable_globals then (
        GetProgInfo.computeReachableCode file 
    ) else ();
    (* Output.print_endline "(* computed reachable globals from main *)"; *)

	(* Find all lines, blocks, edges, and conditions. *)
	(* TODO: wrap the listings of Lines,Edges,etc... *)
	let (setOfLines,setOfBlocks,setOfEdges,setOfConds) = GetProgInfo.getProgInfo file run_args.arg_fns in
	run_args.arg_num_lines <- LineSet.cardinal setOfLines;
	run_args.arg_num_blocks <- StmtInfoSet.cardinal setOfBlocks;
	run_args.arg_num_edges <- EdgeSet.cardinal setOfEdges;
	run_args.arg_num_conds <- CondSet.cardinal setOfConds;
	if run_args.arg_list_lines then (
		Output.printf "Total number of %s: %d\n" "Lines" run_args.arg_num_lines;
		LineSet.iter
			(fun (file,lineNum) -> Output.printf "%s:%d\n" file lineNum)
			setOfLines
		;Output.printf "\n"
	);
	if run_args.arg_list_blocks then (
		Output.printf "Total number of %s: %d\n" "Blocks" run_args.arg_num_blocks;
		StmtInfoSet.iter
			(fun stmtInfo ->
				 Output.printf "%s\n" (To_string.stmtInfo stmtInfo))
		setOfBlocks
		;Output.printf "\n"
	);
	if run_args.arg_list_edges then (
		Output.printf "Total number of %s: %d\n" "Edges" run_args.arg_num_edges;
		EdgeSet.iter
			(fun (srcStmtInfo,destStmtInfo) ->
				 Output.printf "%s -> %s\n"
					 (To_string.stmtInfo srcStmtInfo)
					 (To_string.stmtInfo destStmtInfo))
		setOfEdges
		;Output.printf "\n"
	);
	if run_args.arg_list_conds then (
		Output.printf "Total number of %s: %d\n" "Conditions" run_args.arg_num_conds;
		CondSet.iter
			(fun (stmtInfo, truth) -> Output.printf "%s %c\n" (To_string.stmtInfo stmtInfo) (if truth then 'T' else 'F'))
		setOfConds
		;Output.printf "\n"
	)

(* create a job that begins at a function, given an initial state *)
let job_for_function ?(exHist=emptyHistory) file state fn argvs =
	let state = MemOp.state__start_fcall state Runtime fn argvs in
	(* create a new job *)
	{
		file = file;
		state = state;
		exHist = exHist;
		instrList = [];
		stmt = List.hd fn.sallstmts;
		inTrackedFn = Utility.StringSet.mem fn.svar.vname run_args.arg_fns;
		mergePoints = StmtInfoSet.empty;
		jid = Utility.next_jid;
	}


(* create a job that begins in the middle of a file at some entry function with some optional constraints *)
let job_for_middle file entryfn yamlconstraints =

    (* Initialize the state with symbolic globals *)
    let state = MemOp.state__empty in
    let state = init_symbolic_globalvars state file.globals in

    let state, entryargs, exHist = init_symbolic_argvs state entryfn in

    (* create a job starting at entryfn *)
    let job = job_for_function ~exHist:exHist file state entryfn entryargs in

    (* apply constraints if provided *)
    if yamlconstraints = "" then
        job
    else begin
        (* Prepare the invariant input *)
      (*
      let objectMap = InvInput.parse yamlconstraints in
      let _,new_state = InvInput.constrain job.state entryfn objectMap in
        { job with state = new_state }
       *)
      failwith "YAML input not supported"
    end
    

(* create a job that begins at the main function of a file, with the initial state set up for the file *)
let job_for_file file cmdline =
	let main_func =
		try Cilutility.find_fundec_by_name file "main"
		with Not_found -> failwith "No main function found!"
	in

	(* Initialize the state with zeroed globals *)
	let state = MemOp.state__empty in
	let state = init_globalvars state file.globals in

	(* prepare the command line arguments, if needed *)
	let state, main_args =
		match main_func.svar.vtype with
			| TFun (_, Some [], _, _) -> state, [] (* main has no arguments *)
			| _ -> init_cmdline_argvs state cmdline
	in

	(* create a job starting at main *)
	job_for_function file state main_func main_args


let find_entryfn file =
	let fname = Executeargs.run_args.arg_entryfn in
	try
		Cilutility.find_fundec_by_name file fname
	with Not_found ->
		failwith (Printf.sprintf "Entry function %s not found" fname)


let doExecute (f: file) =

	Random.init 226; (* Random is used in Bytes *)

	Output.banner_printf 3 "Otter, a symbolic executor for C\n%!";
    let _ = if Executeargs.run_args.arg_callchain_backward then
      Output.printf "\n* Call-chain-backward Symbolic Execution mode is on.\n\n"
    in

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

    let entryfn = find_entryfn f in

    let results = 
      if Executeargs.run_args.arg_callchain_backward then
        (
          let assertfn =
            let fname = Executeargs.run_args.arg_assertfn in
            try Cilutility.find_fundec_by_name f fname
            with Not_found -> failwith (Printf.sprintf "Assserton function %s not found" fname )
          in
          let job_init = function entryfn -> job_for_middle f entryfn ""(* no yaml file *)
          in
            Callchain_backward.callchain_backward_se (Cilutility.make_callergraph f) entryfn assertfn job_init
        )
      else
        let job = 
          if Executeargs.run_args.arg_entryfn = "main" then
            (* create a job for the file, with the commandline arguments set to the file name
             * and the arguments from the '--arg' option *)
            job_for_file f (f.fileName::Executeargs.run_args.arg_cmdline_argvs)
          else
              (* create a job to start in the middle of entryfn *)
              job_for_middle f entryfn Executeargs.run_args.arg_yaml
        in
	    (* run the job *)
	if run_args.arg_merge_paths then
		let result = PathMerging.init job in
          	[result]
	else 
        	let result = Driver.init job in
          	[result]
    in

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
   (if Executeargs.run_args.arg_use_conditional_exceptions then
      Output.printf "It took %.2f s to remove conditonal exceptions in Bytes_Conditional's.\n"
         (Stats.lookupTime "Remove exceptions")
    else ());
   (if Executeargs.run_args.arg_simplify_path_condition then
      Output.printf "It took %.2f s to simplify path conditions.\n"
         (Stats.lookupTime "Simplify PC")
    else ());
   
    Output.printf "Hash-consing: hits=%d misses=%d\n" (!hash_consing_bytes_hits) (!hash_consing_bytes_misses);
    Output.printf "Bytes eval caching: hits=%d misses=%d\n\n" (!MemOp.bytes_eval_cache_hits) (!MemOp.bytes_eval_cache_misses);

    (*
  begin
    if Executeargs.run_args.arg_examfn = "" then () else
      let print_record r = Output.printf "#true:%d\n#false:%d\n#unknown:%d\n" r.numTrue r.numFalse r.numUnknown in
        Output.printf "pc -> ct:\n";
        print_record (!InvInput.pc2ct);
        Output.printf "ct -> pc:\n";
        print_record (!InvInput.ct2pc);
        ()
  end;
     *)
	List.iter (fun result -> Report.print_report result) results


let feature : featureDescr = 
  { fd_name = "execute";              
    fd_enabled = ref false;
    fd_description = "(symbolic) executor for C";
		fd_extraopt = [
			
			(**
					Running options
			 *)
			("--failfast",
			Arg.Unit (fun () -> Executeargs.run_args.arg_failfast <- true),
			" Abort execution if any path encounters an error\n");

			("--noboundsChecking",
			Arg.Unit (fun () -> run_args.arg_bounds_checking <- false),
			" Disable bounds checking on memory accesses\n");

			("--cfgPruning",
			Arg.Unit (fun () -> run_args.arg_cfg_pruning <- true),
			" Enable CFG pruning\n");

			("--callchainBackward",
			Arg.Unit (fun () -> run_args.arg_cfg_pruning <- true;run_args.arg_callchain_backward <- true),
			" Enable call-chain backward symbolic execution (will enable CFG pruning)\n");

			(**
					Printing options
			 *)
			(* TODO: for each msg type, a --print and --noprint option*)
			(* STP *)
			("--printSTP",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_stp <- true),
			" Print STP programs");
			(* Assignment in the form lval = rval *)
			("--printAssign",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_assign <- true),
			" Print assignments (from rval to lval)");

			("--printFunctionCall",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_func <- true),
			" Print function calls");
			(* Print the guard of an if statement *)
			("--printIf",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_guard <- true),
			" Print the guard of an if statement");
			("--printNoEscapedString",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_no_escaped_string <- true),
			" Print strings without escaped characters");
			("--printCallStack",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_callstack <- true),
			" Print call stack (when branching)");
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
			" Suppress most output");

			("--printNothing",
			 Arg.Unit (fun () -> print_args.arg_print_nothing <- true),
			" Suppress (pretty much) all output. This trumps all other --print* options");

			("--printCharAsInt",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_char_as_int <- true),
			" Print char as int");

			("--printStmtLocs",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_stmt_locs <- true),
			" Print file and line number for statements, in addition to function name an id number, for block and edge coverage\n");

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
			 " Track condition coverage");
			("--edgeCov",
			 Arg.Unit (fun () -> run_args.arg_edge_coverage <- true),
			 " Track edge coverage");
			("--blockCov",
			 Arg.Unit (fun () -> run_args.arg_block_coverage <- true),
			 " Track statement coverage");
			("--lineCov",
			 Arg.Unit (fun () -> run_args.arg_line_coverage <- true),
			 " Track line coverage");
			("--pathCov",
			 Arg.Unit (fun () -> run_args.arg_path_coverage <- true),
			 " Track path coverage");
			("--covStats",
			 Arg.String readCovStatsFromFile,
			 "<filename> File containing coverage statistics\n");

			("--listAllLines",
			 Arg.Unit (fun () -> run_args.arg_list_lines <- true),
			 " Before execution, print out all of the lines in the program.");
			("--listAllBlocks",
			 Arg.Unit (fun () -> run_args.arg_list_blocks <- true),
			 " Before execution, print out all of the basic blocks in the program.");
			("--listAllEdges",
			 Arg.Unit (fun () -> run_args.arg_list_edges <- true),
			 " Before execution, print out all of the intraprodecural edges in the program.");
			("--listAllConds",
			 Arg.Unit (fun () -> run_args.arg_list_conds <- true),
			 " Before execution, print out all of the conditions in the program.\n");

			("--mergePaths",
			 Arg.Unit (fun () -> run_args.arg_merge_paths <- true),
			 " Merge similar execution paths\n");

			("--initMallocZero",
			 Arg.Unit (fun () -> run_args.arg_init_malloc_zero <- true),
			 " Initialize memory allocated by malloc() to zero.
\t\t\t(By default, such memory contains undefined values which
\t\t\tcause an error if they ever get passed to the SMT solver.)\n");

			("--timeout",
			 Arg.Int (fun n -> run_args.arg_timeout <- n),
			 "<numSeconds> Set a timeout for the executor\n");

			("--marshalCoverageTo",
			 Arg.String (fun str -> run_args.arg_marshal_file <- str),
			 "<file> Marshal coverage information to <file>.\n");

			("--noinitUnreachableGlobals",
			 Arg.Unit (fun () -> run_args.arg_noinit_unreachable_globals <- true),
			 " Do NOT initialize unreachable globals\n");

			("--useConditionalExceptions",
			 Arg.Unit (fun () -> run_args.arg_use_conditional_exceptions <- true),
			 " Use conditional exceptions when processing 'a conditional\n");

			("--simplifyPathCondition",
			 Arg.Unit (fun () -> run_args.arg_simplify_path_condition <- true),
			 " Check if a newly added constraint implies any previous ones\n");

			("--yaml",
			 Arg.String readYamlFromFile,
			 "<filename> File containing Yaml output\n");

			("--entryfn",
			Arg.String (fun fname -> Executeargs.run_args.arg_entryfn <- fname),
			"<fname> Entry function (default: main) \n");

			("--assertfn",
			Arg.String (fun fname -> Executeargs.run_args.arg_assertfn <- fname),
			"<fname> Assertion function to look for in the call-chain-backward mode (default: __ASSERT) \n");

			("--examfn",
			Arg.String (fun fname -> Executeargs.run_args.arg_examfn <- fname),
			"<fname> Function to be examined (default: none) \n");


(*
			("--marshalFrom",
			 Arg.String
				 (fun filename ->
						let inChan = open_in_bin filename in
						let coverage = (Marshal.from_channel inChan : job_result list) in
						ignore coverage (* Do something with the coverage information *)
				 ),
			 "<filename> Read coverage information from an output file.");
*)
		];
		fd_post_check = true;
    fd_doit = doExecute
  } 
	
