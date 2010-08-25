open OcamlUtilities
open OtterCore
open Executeargs

let print_report results =
	let coverage, completed, truncated, abandoned =
		List.fold_left begin fun (coverage, completed, truncated, abandoned) result ->
			match result with
				| Types.Return (_, c)
				| Types.Exit (_, c)      -> (c::coverage, completed + 1, truncated, abandoned)
				| Types.Truncated (c, d) -> (c::d::coverage, completed, truncated + 2, abandoned)
				| Types.Abandoned _      -> (coverage, completed, truncated, abandoned + 1)
		end ([], 0, 0, 0) results in
		if completed = 0 then (
			Output.printf "All %d paths had errors.\n" abandoned
				(* Program execution ends. *)
	) else (
		(* If there were successful runs *)
		Output.printf "%d paths ran to completion; %d had errors.\n" completed abandoned;
		Output.printf "There are %d truncated paths.\n" truncated;

		if run_args.arg_line_coverage || run_args.arg_block_coverage || run_args.arg_edge_coverage || run_args.arg_cond_coverage || run_args.arg_path_coverage
		then begin
			(* Print coverage information, if it was gathered, regardless of anything else.*)
			Output.arg_print_nothing := false;
			Output.set_mode Output.MSG_MUSTPRINT;
			Coverage.printCoverageInfo coverage;

			(* Marshal out (coverage : Types.job_result list) so that we can
				 read it back in later. *)
			if run_args.arg_marshal_file <> ""
			then (
				Output.printf "Marshalling results to %s\n" run_args.arg_marshal_file;
				let outChan = open_out_bin run_args.arg_marshal_file in
					Marshal.to_channel outChan coverage [];
					close_out outChan
			);

			Output.printf "Finished.\n";

		end
	)

