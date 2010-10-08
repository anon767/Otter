open DataStructures
open OcamlUtilities
open OtterBytes
open OtterCore
open OtterJob
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
let rec process_job_states result multijob completed multijob_queue =
	match result with
		| Active job ->
			(* put the job back into the multijob and queue it *)
			let multijob = put_job job multijob multijob.current_pid in
			(completed, (multijob::multijob_queue))
		| Fork states ->
			(* process all forks *)
			List.fold_left begin fun (completed, multijob_queue) state ->
				process_job_states state multijob completed multijob_queue
			end (completed, multijob_queue) states
		| Complete completion ->
			(* store the results *)
			let multijob = put_completion completion multijob in
			begin match completion with
				| Abandoned (reason, loc, job_result) ->
					Output.set_mode Output.MSG_MUSTPRINT;
					Output.printf
						"Error \"%a\" occurs at %a.@\nAbandoning path.@\n"
						Report.abandoned_reason reason Printcil.loc loc
				| _ ->
					()
			end;
			((completion::completed), (multijob::multijob_queue))

		| _ ->
			(completed, multijob_queue)

let process_result result completed job_queue =
	let multijob, multijob_queue = job_queue in
	process_job_states result multijob completed multijob_queue

let run job = 
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
		[ multijob ]

let doit file =
	(* TODO: do something about signal handlers/run statistics from Executemain.doExecute *)

	Core.prepare_file file;
	let entryfn = Driver.find_entryfn file in
	let job =
		if !Executeargs.arg_entryfn = "main" then
			(* create a job for the file, with the commandline arguments set to the file name
			 * and the arguments from the '--arg' option *)
			FileJob.make file (file.Cil.fileName::!Executeargs.arg_cmdline_argvs)
		else
			(* create a job to start in the middle of entryfn *)
			FunctionJob.make file entryfn
	in

	(* run the job *)
	let result = run job in

	(* print the results *)
	Output.printf "%s@\n" (Executedebug.get_log ());
	Report.print_report result


(* Cil feature for multi-process Otter *)
let feature = {
	Cil.fd_name = "multiotter";
	Cil.fd_enabled = ref false;
	Cil.fd_description = "Multi-process symbolic executor for C";
	Cil.fd_extraopt = [];
	Cil.fd_post_check = true;
	Cil.fd_doit = doit
}

