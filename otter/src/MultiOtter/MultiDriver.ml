open DataStructures
open OcamlUtilities
open OtterBytes
open OtterCore
open OtterReporter
open OtterDriver
open State
open Job

open MultiJobUtilities

let (>>>) = Interceptor.(>>>)

let multi_set_output_formatter job =
	let loc = Job.get_loc job in
	let label =
		if loc = Cil.locUnknown then
			Format.sprintf "[jid: %d, pid: %d] : " job#path_id job#pid
		else
			Format.sprintf "[jid: %d, pid: %d] %s:%d : " job#path_id job#pid (Filename.basename loc.Cil.file) loc.Cil.line
	in
	Output.set_formatter (new Output.labeled label)


let rec process_results = function
    | Active job ->
        Active (process_active job)
    | Fork results ->
        Fork (List.map process_results results)
    | Complete completion ->
        match process_completed completion with
            | Some job -> Fork [ Active job; Complete completion ]
            | None -> Complete completion


let rec flush_queue reporter job_queue =
    match job_queue#get with
      | None -> reporter
      | Some (job_queue, job) ->
            multi_set_output_formatter job;
            let completion = Abandoned (`Failure "Killed by signal", job) in
            let reporter = reporter#report (Complete completion) in
            match process_completed completion with
                | Some job ->
                    flush_queue reporter (job_queue#put job) (* Put the other processes back into the queue so that they get reported, too. *)
                | None ->
                    flush_queue reporter job_queue


let run reporter file =
    let interceptor =
        Interceptor.function_pointer_interceptor
        >>> MultiInterceptor.abandon_io_block_deadlock_interceptor
        >>> MultiFunctions.interceptor
        >>> BuiltinFunctions.interceptor
    in
    let step job =
        multi_set_output_formatter job;
        let job = MultiJobUtilities.schedule_job job in
        let results = interceptor job Statement.step in
        process_results results
    in
    let job = new MultiJob.t file (file.Cil.fileName::!ProgramPoints.command_line) in
    let queue = (OtterQueue.Queue.get_default ())#put job in

    (* start executing *)
    Driver.main_loop step queue reporter


let doit file =
	(* connect Cil's debug flag to Output *)
	Output.arg_print_debug := !Errormsg.debugFlag;
	
	Output.printf "MultiOtter, a multiprocess symbolic executor for C@\n@.";

	let reporter = UserSignal.using_signals begin fun () ->
		Core.prepare_file file;

		(* run the job *)
		let module Reporter = ErrorReporter.Make (OtterCore.Errors) in
		let job_queue, reporter = run (new Reporter.t ()) file in
		flush_queue reporter job_queue
	end in

	(* print the results *)
	Output.set_formatter (new Output.plain);
	if !Profiler.do_profiling then Output.printf "== Global profile ==@\n@[%t@]@." Profiler.global#printer;
	Output.printf "@[%t@]@." Memo.statistics_printer;
	let steps, _, _ = reporter#get_stats in
	Output.printf "Number of steps: %d@." steps;
	Report.print_report reporter#completed


(* Cil feature for multi-process Otter *)
let feature = {
	Cil.fd_name = "multiotter";
	Cil.fd_enabled = ref false;
	Cil.fd_description = "Multi-process symbolic executor for C";
	Cil.fd_extraopt = [];
	Cil.fd_post_check = true;
	Cil.fd_doit = doit
}

