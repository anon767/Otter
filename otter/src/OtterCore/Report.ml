open OcamlUtilities

let abandoned_reason ff = function
    | `Failure msg -> Format.fprintf ff "`Failure:%s" msg

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

		if !Executeargs.arg_line_coverage
				|| !Executeargs.arg_block_coverage
				|| !Executeargs.arg_edge_coverage
				|| !Executeargs.arg_cond_coverage
				|| !Executeargs.arg_path_coverage then begin
			(* Print coverage information, if it was gathered, regardless of anything else.*)
			Output.set_mode Output.MSG_MUSTPRINT;
			Coverage.printCoverageInfo coverage;

			(* Marshal out (coverage : Types.job_result list) so that we can
				 read it back in later. *)
			if !Executeargs.arg_marshal_file <> ""
			then (
				Output.printf "Marshalling results to %s\n" !Executeargs.arg_marshal_file;
				let outChan = open_out_bin !Executeargs.arg_marshal_file in
					Marshal.to_channel outChan coverage [];
					close_out outChan
			);

			Output.printf "Finished.\n";

		end
	)

