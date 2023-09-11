open OcamlUtilities
open Cil
open OtterBytes
open OtterCore
open OtterJob
open OtterQueue
open OtterReporter
open OtterDriver
open State

module OtterJobProfiler = Driver.OtterJobProfiler

let doit file =
	(* connect Cil's debug flag to Output *)
	Output.arg_print_debug := !Errormsg.debugFlag;

	Output.printf "Otter, a symbolic executor for C@\n@.";

	let reporter = UserSignal.using_signals ~usr1_handler:(fun _ -> OtterJobProfiler.flush ()) begin fun () ->
		(* prepare the file for symbolic execution *)
		Core.prepare_file file;

		(* run the job *)
		let module Reporter = ErrorReporter.Make (OtterCore.Errors) in
		let reporter = new Reporter.t () in
		let _, reporter = Driver.run_basic reporter file in
		reporter
	end in

    Output.set_formatter (new Output.plain);
    if !Profiler.do_profiling then Output.printf "== Global profile ==@\n@[%t@]@." Profiler.global#printer;
    Output.printf "@[%t@]@." Memo.statistics_printer;

    OtterJobProfiler.flush ();

    let steps, _, _ = reporter#get_stats in
    Output.printf "Number of steps: %d@." steps;
    Report.print_report reporter#completed

let options =
    BasicReporter.options @
    UserSignal.options @
    Profiler.options @
    Executeargs.options @
    FunctionJob.options @
    ProgramPoints.options @
    Queue.options @
    BytesSTP.options @
    SymbolicPointers.options @
    TrackingFunctions.options @
    Output.options @
    InitBytes.options @
    CilUtilities.CilPtranal.options @
    Operator.options @
    OtterExtensions.JobProfiler.options

let feature : featureDescr = {
	fd_name = "otter";
	fd_enabled = ref false;
	fd_description = "(symbolic) executor for C";
	fd_extraopt = options;
	fd_post_check = true;
	fd_doit = doit;
}

