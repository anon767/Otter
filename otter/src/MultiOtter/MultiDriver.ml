open DataStructures
open OcamlUtilities
open OtterBytes
open OtterCore
open OtterReporter
open OtterDriver
open State
open Job

open MultiTypes
open MultiJobUtilities

let (@@) = MultiInterceptor.(@@)
let (@@@) = MultiInterceptor.(@@@)

let multi_set_output_formatter_interceptor job multijob job_queue interceptor = 
	let loc = Job.get_loc job in
	let label =
		if loc = Cil.locUnknown then
			Format.sprintf "[jid: %d, pid: %d] : " multijob.active_job#path_id multijob.current_metadata.pid
		else
			Format.sprintf "[jid: %d, pid: %d] %s:%d : " multijob.active_job#path_id multijob.current_metadata.pid (Filename.basename loc.Cil.file) loc.Cil.line
	in
	Output.set_formatter (new Output.labeled label);
	interceptor job multijob job_queue

let rec get_job_multijob job_queue = 
	match job_queue#get with
		| None -> None (* no multijobs left; quit *)
		| Some (job_queue, multijob) ->
			match get_job multijob with
				| None -> get_job_multijob job_queue (* this multijob had no processes left; get another *)
				| Some (job, multijob) -> Some (job, (multijob, job_queue)) (* found a multijob with a job to run *)

(* process the results *)
let rec process_job_states result multijob multijob_queue reporter =
	let reporter = reporter#report result in  (* TODO: use the #should_continue flag *)
	match result with
		| Active job ->
			(* put the job back into the multijob and queue it *)
			let multijob = put_job job multijob multijob.current_metadata in
			((multijob_queue#put multijob), reporter)
		| Fork states ->
			(* process all forks *)
			List.fold_left begin fun (multijob_queue, reporter) state ->
				process_job_states state multijob multijob_queue reporter
			end (multijob_queue, reporter) states
		| Complete completion ->
			(* store the results *)
			let multijob = put_completion completion multijob in
			((multijob_queue#put multijob), reporter)

		| _ ->
			(multijob_queue, reporter)

let process_result result job_queue reporter =
	let multijob, multijob_queue = job_queue in
	process_job_states result multijob multijob_queue reporter

let rec flush_queue reporter job_queue =
	match get_job_multijob job_queue with
		| None -> reporter
		| Some (job, (multijob, job_queue)) ->
			let reporter = reporter#report (Complete (Abandoned (`Failure "Killed by signal", job))) in
			flush_queue reporter job_queue

let run reporter job =
	let multijob = {
		processes = [];
		shared =
		{
			shared_block_to_bytes = MemoryBlockMap.empty;
		};
		next_pid = 1;
		current_metadata = 
		{
			pid = 0;
			parent_pid = -2;
			priority = Running;
		};
		active_job = job;
	} in
	let multijob = put_job job multijob multijob.current_metadata in

	let queue = (new GenerationalQueue.t)#put multijob in

	(* start executing *)
	LegacyDriver.main_loop 
		get_job_multijob
		(
			MultiInterceptor.unpack_job_interceptor @@
			multi_set_output_formatter_interceptor @@@
			MultiInterceptor.abandon_io_block_deadlock_interceptor @@@
			MultiFunctions.interceptor @@@
			MultiInterceptor.repack_job_interceptor @@@
			BuiltinFunctions.interceptor @@ 
			BuiltinFunctions.libc_interceptor @@
			Statement.step
		)
		process_result
		queue
		reporter

let doit file =
	(* connect Cil's debug flag to Output *)
	Output.arg_print_debug := !Errormsg.debugFlag;
	
	Output.printf "MultiOtter, a multiprocess symbolic executor for C@\n@\n";

	(* Keep track of how long we run *)
	let startTime = Unix.gettimeofday () in

	(* Set signal handlers to catch timeouts and interrupts *)
	let old_ALRM_handler =
		Sys.signal Sys.sigalrm
			(Sys.Signal_handle (fun _ -> raise (SignalException "\nTimed out!")))
	and old_INT_handler =
		Sys.signal Sys.sigint
			(Sys.Signal_handle (fun _ -> raise (SignalException "\nUser interrupt!")))
	in
	(* Set a timer *)
	ignore (Unix.alarm !Executeargs.arg_timeout);

	Core.prepare_file file;
	let job = OtterJob.Job.get_default file in

	(* run the job *)
	let module Reporter = ErrorReporter.Make (OtterCore.Errors) in
	let job_queue, result = run (new Reporter.t ()) job in
	let result = flush_queue result job_queue in
	
	(* Turn off the alarm and reset the signal handlers *)
	ignore (Unix.alarm 0);
	Sys.set_signal Sys.sigalrm old_ALRM_handler;
	Sys.set_signal Sys.sigint old_INT_handler;

	(* print the results *)
	Output.set_formatter (new Output.plain);
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

	let nodes, _, _ = result#get_stats in
    Output.printf "Number of nodes: %d@\n" nodes;
	Report.print_report result#completed


(* Cil feature for multi-process Otter *)
let feature = {
	Cil.fd_name = "multiotter";
	Cil.fd_enabled = ref false;
	Cil.fd_description = "Multi-process symbolic executor for C";
	Cil.fd_extraopt = [];
	Cil.fd_post_check = true;
	Cil.fd_doit = doit
}

