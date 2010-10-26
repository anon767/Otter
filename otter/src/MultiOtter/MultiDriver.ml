open DataStructures
open OcamlUtilities
open OtterBytes
open OtterCore
open OtterReporter
open OtterDriver
open Types
open Job

open MultiTypes
open MultiJobUtilities

let (@@) = MultiInterceptor.(@@)
let (@@@) = MultiInterceptor.(@@@)

let multi_set_output_formatter_interceptor job multijob job_queue interceptor = 
	let loc = Job.get_loc job in
	let label =
		if loc = Cil.locUnknown then
			Format.sprintf "[jid: %d, pid: %d] : " multijob.jid multijob.current_pid
		else
			Format.sprintf "[jid: %d, pid: %d] %s:%d : " multijob.jid multijob.current_pid loc.Cil.file loc.Cil.line
	in
	Output.set_formatter (new Output.labeled label);
	interceptor job multijob job_queue

let rec get_job_multijob job_queue = 
	match job_queue with
		| [] -> None
		| multijob::t ->
			match get_job multijob with
				| None -> get_job_multijob t
				| Some (job, multijob) -> Some (job, (multijob, t))

(* process the results *)
let rec process_job_states result multijob reporter multijob_queue =
	let reporter, _more = reporter#report result in (* TODO: actually respond to the _more flag *)
	match result with
		| Active job ->
			(* put the job back into the multijob and queue it *)
			let multijob = put_job job multijob multijob.current_pid in
			(reporter, (multijob::multijob_queue))
		| Fork states ->
			(* process all forks *)
			List.fold_left begin fun (reporter, multijob_queue) state ->
				process_job_states state multijob reporter multijob_queue
			end (reporter, multijob_queue) states
		| Complete completion ->
			(* store the results *)
			let multijob = put_completion completion multijob in
			(reporter, (multijob::multijob_queue))

		| _ ->
			(reporter, multijob_queue)

let process_result result reporter job_queue =
	let multijob, multijob_queue = job_queue in
	process_job_states result multijob reporter multijob_queue

let run reporter job = 
	let multijob = {
		file = job.Job.file;
		processes = [];
		shared = {
			path_condition = [];
			shared_block_to_bytes = MemoryBlockMap.empty;
		};
		jid = job.Job.jid;
		next_pid = 1;
		current_pid = 0;
	} in
	let multijob = put_job job multijob 0 in

	(* start executing *)
	LegacyDriver.main_loop 
		get_job_multijob
		(
			MultiInterceptor.unpack_job_interceptor @@
			multi_set_output_formatter_interceptor @@@
			MultiFunctions.interceptor @@@
			MultiInterceptor.repack_job_interceptor @@@
			BuiltinFunctions.interceptor @@ 
			BuiltinFunctions.libc_interceptor @@
			Statement.step
		)
		process_result
		reporter
		[ multijob ]

let doit file =
	(* TODO: do something about signal handlers/run statistics from Executemain.doExecute *)

	Core.prepare_file file;
	let job = OtterJob.Job.get_default file in

	(* run the job *)
	let module Reporter = BasicReporter.Make (OtterCore.Errors) in
	let result = run (new Reporter.t ()) job in

	(* print the results *)
	Output.printf "%s@\n" (Executedebug.get_log ());
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

