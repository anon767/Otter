open OcamlUtilities
open Cil
open OtterBytes
open OtterCore
open OtterJob
open OtterQueue
open OtterReporter
open OtterDriver
open Types
(*open InvInput*)

let doExecute (f: file) =
	(* connect Cil's debug flag to Output *)
	Output.arg_print_debug := !Errormsg.debugFlag;

	Random.init (!Executeargs.arg_random_seed); (* Random is used in Bytes *)

	Output.printf "Otter, a symbolic executor for C@\n@\n";

	(* Keep track of how long we run *)
	let startTime = Unix.gettimeofday () in

	(* Set signal handlers to catch timeouts and interrupts *)
	let old_ALRM_handler =
		Sys.signal Sys.sigalrm
			(Sys.Signal_handle (fun _ -> raise (SignalException "Timed out!")))
	and old_INT_handler =
		Sys.signal Sys.sigint
			(Sys.Signal_handle (fun _ -> raise (SignalException "User interrupt!")))
	in
	(* Set a timer *)
	ignore (Unix.alarm !Executeargs.arg_timeout);

	(* prepare the file for symbolic execution *)
	Core.prepare_file f;

	let job = Job.get_default f in

	(* run the job *)
	let module Reporter = ErrorReporter.Make (OtterCore.Errors) in
	let completed = Driver.run_with_libc (new Reporter.t ()) job in

	(* Turn off the alarm and reset the signal handlers *)
	ignore (Unix.alarm 0);
	Sys.set_signal Sys.sigalrm old_ALRM_handler;
	Sys.set_signal Sys.sigint old_INT_handler;

	Output.set_formatter (new Output.plain);
	Output.printf "%s@\n" (Executedebug.get_log ());
		(* function stat
		Output.print_endline "\nFunction call stat:";
		Cilutility.FundecMap.iter (fun f c -> Output.print_endline ((To_string.fundec f)^" : "^(string_of_int c))) (!MemOp.function_stat);
		*)
	Output.printf "\nSTP was invoked %d times (%d cache hits).\n" !Stp.stp_count !Stp.cacheHits;

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
   (if !Executeargs.arg_simplify_path_condition then
      Output.printf "It took %.2f s to simplify path conditions.\n"
         (Stats.lookupTime "Simplify PC")
    else ());

    Output.printf "Hash-consing: hits=%d misses=%d\n" (!Bytes.hash_consing_bytes_hits) (!Bytes.hash_consing_bytes_misses);
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
    let nodes, _, _ = completed#get_stats in
    Output.printf "Number of nodes: %d@\n" nodes;
    Report.print_report completed#completed


let feature : featureDescr = {
	fd_name = "execute";
	fd_enabled = ref false;
	fd_description = "(symbolic) executor for C";
	fd_extraopt = Executeargs.options @ Queue.options @ Job.options @ BasicReporter.options @ Stp.options;
	fd_post_check = true;
	fd_doit = doExecute;
}

