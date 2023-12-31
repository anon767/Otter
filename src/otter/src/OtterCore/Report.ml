open OcamlUtilities

let print_report results =
    let coverage, completed, abandoned =
        List.fold_left begin fun (coverage, completed, abandoned) result ->
            match result with
                | (Job.Return _ | Job.Exit _), c -> (c::coverage, completed + 1, abandoned)
                | Job.Abandoned _, c -> (c::coverage, completed, abandoned + 1)
                | Job.Truncated _, _ -> (coverage, completed, abandoned) (* ignored *)
        end ([], 0, 0) results in
        (if completed = 0 then
            Output.printf "All %d paths had errors.@." abandoned
            (* Program execution ends. *)
        else
            (* If there were successful runs *)
            Output.printf "%d paths ran to completion; %d had errors.@." completed abandoned
        );

        if coverage = [] then
            ()
        else begin
            if    !Executeargs.arg_line_coverage
               || !Executeargs.arg_block_coverage
               || !Executeargs.arg_edge_coverage
               || !Executeargs.arg_cond_coverage
               || !Executeargs.arg_path_coverage then
            begin
                (* Print coverage information, if it was gathered, regardless of anything else.*)
                Output.set_mode Output.MSG_REPORT;
                Coverage.printCoverageInfo coverage;

                (* Marshal out (coverage : Job.job_result list) so that we can
                     read it back in later. *)
                if !Executeargs.arg_marshal_file <> ""
                then (
                    Output.printf "Marshalling results to %s@." !Executeargs.arg_marshal_file;
                    let outChan = open_out_bin !Executeargs.arg_marshal_file in
                        Marshal.to_channel outChan coverage [];
                        close_out outChan
                );

                Output.printf "Finished.@.";
            end
        end

