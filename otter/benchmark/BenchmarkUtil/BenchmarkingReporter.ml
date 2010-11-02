open OtterCore
open OtterReporter

class ['self] t ?max_nodes ?max_paths ?max_abandoned () = object (self : 'self)
    inherit ['self] BasicReporter.t ?max_nodes ?max_paths ?max_abandoned () as super
    val start_times = Unix.times ()
    val records = []

    method summarize =
        Format.eprintf "@.";
        Format.eprintf "Abandoned jobs in order of discovery (by time, nodes and paths visited):@\n";
        Format.eprintf "  @[";
        ignore begin List.fold_right2 begin fun completion (end_times, nodes, paths) n ->
            match completion with
                | Job.Abandoned _ ->
                    let elapsed_user = end_times.Unix.tms_utime -. start_times.Unix.tms_utime in
                    let elapsed_total = elapsed_user +. end_times.Unix.tms_stime -. start_times.Unix.tms_stime in
                    Format.eprintf "%4d. %8.3fs user  %8.3fs total  %6d node%c  %6d path%c@\n"
                        n elapsed_user elapsed_total nodes (if nodes = 1 then ' ' else 's') paths (if paths = 1 then ' ' else 's');
                    n + 1
                | _ ->
                    n
        end completed records 1 end;
        Format.eprintf "@]@.";

    method record = function
        | Job.Complete completion ->
            {< records = (Unix.times (), nodes, paths)::records >}

        | Job.Active _
        | Job.Fork _ ->
            self

        | Job.Paused _ ->
            invalid_arg "unexpected Job.Paused"

    method report job_result =
        (super#report job_result)#record job_result
end
