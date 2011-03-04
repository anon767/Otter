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
			Interceptor.function_pointer_interceptor @@
			MultiInterceptor.unpack_job_interceptor @@
			multi_set_output_formatter_interceptor @@@
			MultiInterceptor.abandon_io_block_deadlock_interceptor @@@
			MultiFunctions.interceptor @@@
			MultiInterceptor.repack_job_interceptor @@@
			BuiltinFunctions.interceptor @@ 
			Statement.step
		)
		process_result
		queue
		reporter

let doit file =
	(* connect Cil's debug flag to Output *)
	Output.arg_print_debug := !Errormsg.debugFlag;
	
	Output.printf "MultiOtter, a multiprocess symbolic executor for C@\n@.";

	let result = UserSignal.using_signals begin fun () ->
		Core.prepare_file file;
		let job = OtterJob.Job.get_default file in

		(* run the job *)
		let module Reporter = ErrorReporter.Make (OtterCore.Errors) in
		let job_queue, result = run (new Reporter.t ()) job in
		flush_queue result job_queue
	end in

	(* print the results *)
	Output.set_formatter (new Output.plain);
	Output.printf "== Global profile ==@\n@[%t@]@." Profiler.global#printer;
	Output.printf "@[%t@]@." Memo.statistics_printer;
	let nodes, _, _ = result#get_stats in
	Output.printf "Number of nodes: %d@." nodes;
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

