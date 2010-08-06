open DataStructures
open OcamlUtilities
open Cil
open OtterBytes
open Bytes
open BytesUtility
open Types
open Executeargs
open MemOp
(*open InvInput*)

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
	Driver.prepare_file f;

    let entryfn = Driver.find_entryfn f in

    let results =
      if Executeargs.run_args.arg_callchain_backward then
        (
          let assertfn =
            let fname = Executeargs.run_args.arg_assertfn in
            try Cilutility.find_fundec_by_name f fname
            with Not_found -> failwith (Printf.sprintf "Assserton function %s not found" fname )
          in
          let job_init = function entryfn -> Driver.job_for_middle f entryfn
          in
            Callchain_backward.callchain_backward_se (Cilutility.make_callergraph f) entryfn assertfn job_init
        )
      else
        let job = 
          if Executeargs.run_args.arg_entryfn = "main" then
              (* create a job for the file, with the commandline arguments set to the file name
               * and the arguments from the '--arg' option *)
              Driver.job_for_file f (f.fileName::Executeargs.run_args.arg_cmdline_argvs)
          else
              (* create a job to start in the middle of entryfn *)
              Driver.job_for_middle f entryfn
        in
	    (* run the job *)
        	[ Driver.run job ]
    in

	(* Turn off the alarm and reset the signal handlers *)
	ignore (Unix.alarm 0);
	Sys.set_signal Sys.sigalrm old_ALRM_handler;
	Sys.set_signal Sys.sigint old_INT_handler;

	Output.formatter := new Output.plain;
	Output.printf "%s@\n" (Executedebug.get_log ());
		(* function stat 
		Output.print_endline "\nFunction call stat:";
		Cilutility.FundecMap.iter (fun f c -> Output.print_endline ((To_string.fundec f)^" : "^(string_of_int c))) (!MemOp.function_stat);	
		*)
	Output.printf "\nSTP was invoked %d times. (%d cache hits; %d misses)\n" !Stp.stp_count !Stp.cacheHits !Stp.cacheMisses;

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
			Arg.Set Output.arg_print_stp,
			" Print STP programs");
			(* Assignment in the form lval = rval *)
			("--printAssign",
			Arg.Set Output.arg_print_assign,
			" Print assignments (from rval to lval)");

			("--printFunctionCall",
			Arg.Set Output.arg_print_func,
			" Print function calls");
			(* Print the guard of an if statement *)
			("--printIf",
			Arg.Set Output.arg_print_guard,
			" Print the guard of an if statement");
			("--printCallStack",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_callstack <- true),
			" Print call stack (when branching)");
			(* Sparse printing *)
			("--printLittle",
			Arg.Unit (fun () -> 
				Output.arg_print_reg := false;
				Output.arg_print_stmt := false;
				Output.arg_print_func := false;
				Output.arg_print_assign := false;
			),
			" Suppress most output");

			("--printNothing",
			 Arg.Set Output.arg_print_nothing,
			" Suppress (pretty much) all output. This trumps all other --print* options");

			("--printCharAsInt",
			Arg.Set BytesPrinter.print_char_as_int,
			" Print char as int");

			("--printStmtLocs",
			Arg.Set TypesPrinter.print_stmtInfo_locs,
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
	
