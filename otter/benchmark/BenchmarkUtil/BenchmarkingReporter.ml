open OtterCore

class ['self] t = object (_ : 'self)
    val start_times = Unix.times ()
    val nodes = 0
    val paths = 0
    val records = []

    method summarize =
        Format.eprintf "@.";
        Format.eprintf "Abandoned jobs in order of discovery (by time, nodes and paths visited):@\n";
        Format.eprintf "  @[";
        ignore begin List.fold_right begin fun (completion, end_times, nodes, paths) n ->
            match completion with
                | Job.Abandoned _ ->
                    let elapsed_user = end_times.Unix.tms_utime -. start_times.Unix.tms_utime in
                    let elapsed_total = elapsed_user +. end_times.Unix.tms_stime -. start_times.Unix.tms_stime in
                    Format.eprintf "%4d. %8.3fs user  %8.3fs total  %6d node%c  %6d path%c@\n"
                        n elapsed_user elapsed_total nodes (if nodes = 1 then ' ' else 's') paths (if paths = 1 then ' ' else 's');
                    n + 1
                | _ ->
                    n
        end records 1 end;
        Format.eprintf "@]@.";

    method report = function
        | Job.Complete completion ->
            let nodes = nodes + 1 in
            let paths = paths + (match completion with Job.Truncated _ -> 0 | _ -> 1) in
            let records = (completion, Unix.times (), nodes, paths)::records in
            {< nodes = nodes; paths = paths; records = records >}

        | Job.Active _
        | Job.Fork _ ->
            {< nodes = nodes + 1 >}

        | Job.Paused _ ->
            invalid_arg "unexpected Job.Paused"

    method should_continue = true

    method completed = List.map (fun (c, _, _, _) -> c) records
end
