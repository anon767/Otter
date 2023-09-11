open OcamlUtilities
open Cil
open OtterBytes
open OtterCore
open OtterJob
open OtterQueue
open OtterReporter
open OtterDriver
open State
open DebugOtterDriver
(*open InvInput*)

let doDebugExecute file =
	(* connect Cil's debug flag to Output *)
	Output.arg_print_debug := !Errormsg.debugFlag;
	Output.printf "A debugger for Otter, a symbolic executor for C@\n@.";

	let reporter = UserSignal.using_signals begin fun () ->
		(* prepare the file for symbolic execution *)
		Core.prepare_file file;

		let job = new OtterJob.FileJob.t file (file.Cil.fileName::!ProgramPoints.command_line) in

		(* run the job *)
		let module Reporter = ErrorReporter.Make (OtterCore.Errors) in
		let reporter = new Reporter.t () in
		snd (DebugOtterDriver.run_with_libc reporter job)
	end in

	Output.set_formatter (new Output.plain);
	Output.printf "@[%t@]@." Memo.statistics_printer;

    let steps, _, _ = reporter#get_stats in
    Output.printf "Number of steps: %d@." steps;
    Report.print_report reporter#completed


let feature : featureDescr = {
    fd_name = "debugotter";
    fd_enabled = ref false;
    fd_description = "debugger for the (symbolic) executor for C";
    fd_extraopt = [];
    fd_post_check = true;
    fd_doit = doDebugExecute;
}


