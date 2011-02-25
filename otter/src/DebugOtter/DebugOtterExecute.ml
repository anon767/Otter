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
open Printf
(*open InvInput*)

let doDebugExecute (f: file) =
	(* connect Cil's debug flag to Output *)
	Output.arg_print_debug := !Errormsg.debugFlag;
	printf "A debugger for Otter, a symbolic executor for C\n\n";

	let reporter = UserSignal.using_signals begin fun () ->
		(* prepare the file for symbolic execution *)
		Core.prepare_file f;

		let job = Job.get_default f in

		(* run the job *)
		let module Reporter = ErrorReporter.Make (OtterCore.Errors) in
		let reporter = new Reporter.t () in
		snd (DebugOtterDriver.run_with_libc reporter job)
	end in

	Output.set_formatter (new Output.plain);
		(* function stat
		Output.print_endline "\nFunction call stat:";
		Cilutility.FundecMap.iter (fun f c -> Output.print_endline ((To_string.fundec f)^" : "^(string_of_int c))) (!MemOp.function_stat);
		*)
	Output.printf "\nSTP was invoked %d times (%d cache hits).\n" !Stp.stp_count !Stp.cacheHits;

    Output.printf "Hash-consing: hits=%d misses=%d\n" (!Bytes.hash_consing_bytes_hits) (!Bytes.hash_consing_bytes_misses);
  (*  Output.printf "Bytes eval caching: hits=%d misses=%d\n\n" (!MemOp.bytes_eval_cache_hits) (!MemOp.bytes_eval_cache_misses);*)

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
    let nodes, _, _ = reporter#get_stats in
    Output.printf "Number of nodes: %d@\n" nodes;
    Report.print_report reporter#completed


let feature : featureDescr = {
    fd_name = "debugotter";
    fd_enabled = ref false;
    fd_description = "debugger for the (symbolic) executor for C";
    fd_extraopt = [];
    fd_post_check = true;
    fd_doit = doDebugExecute;
}


