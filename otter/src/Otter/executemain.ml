open OcamlUtilities
open Cil
open OtterBytes
open OtterCore
open OtterJob
open OtterQueue
open OtterReporter
open OtterDriver
open State
(*open InvInput*)

let doExecute (f: file) =
	(* connect Cil's debug flag to Output *)
	Output.arg_print_debug := !Errormsg.debugFlag;

	Output.printf "Otter, a symbolic executor for C@\n@.";

	let reporter = UserSignal.using_signals begin fun () ->
		(* prepare the file for symbolic execution *)
		Core.prepare_file f;

		let job = Job.get_default f in

		(* run the job *)
		let module Reporter = ErrorReporter.Make (OtterCore.Errors) in
		let reporter = new Reporter.t () in
		snd (Driver.run_basic reporter job)
	end in

    Output.set_formatter (new Output.plain);
    Output.printf "== Global profile ==@\n@[%t@]@." Profiler.global#printer;
    Output.printf "@[%t@]@." Memo.statistics_printer;

    let steps, _, _ = reporter#get_stats in
    Output.printf "Number of steps: %d@." steps;
    Report.print_report reporter#completed

let options =
    BasicReporter.options @
    UserSignal.options @
    Executeargs.options @
    FunctionJob.options @
    ProgramPoints.options @
    Queue.options @
    Stp.options @
    SymbolicPointers.options @
    TrackingFunctions.options @
    Bytes.options

let feature : featureDescr = {
	fd_name = "execute";
	fd_enabled = ref false;
	fd_description = "(symbolic) executor for C";
	fd_extraopt = options;
	fd_post_check = true;
	fd_doit = doExecute;
}

