open OtterCore
open OtterReporter

class ['self] t ?(max_nodes=0) ?(max_paths=0) ?(max_abandoned=0) () = object (self : 'self)
    inherit ['self] BasicReporter.t ~max_nodes ~max_paths ~max_abandoned () as super
    val start_times = Unix.times ()
    val records = []

    method summarize =
        Format.eprintf "@.";

        Format.eprintf "Abandoned jobs in order of discovery (by time, nodes and paths visited):@.";
        Format.eprintf "    @[";
        if abandoned = 0 then
            Format.eprintf "None"
        else
            ignore begin List.fold_right2 begin fun completion (end_times, nodes, paths) (n, br) ->
                match completion with
                    | Job.Abandoned _ ->
                        let n = n + 1 in
                        let elapsed_user = end_times.Unix.tms_utime -. start_times.Unix.tms_utime in
                        let elapsed_total = elapsed_user +. end_times.Unix.tms_stime -. start_times.Unix.tms_stime in
                        Format.eprintf "%(%)%4d. %8.3fs user  %8.3fs total  %6d node%c  %6d path%c"
                            br n elapsed_user elapsed_total nodes (if nodes = 1 then ' ' else 's') paths (if paths = 1 then ' ' else 's');
                        (n, format_of_string "@\n")
                    | _ ->
                        (n, br)
            end completed records (0, format_of_string "") end;
        Format.eprintf "@]@.";

        let end_times = Unix.times () in
        let elapsed_user = end_times.Unix.tms_utime -. start_times.Unix.tms_utime in
        let elapsed_total = elapsed_user +. end_times.Unix.tms_stime -. start_times.Unix.tms_stime in
        Format.eprintf "All jobs: %8.3fs user  %8.3fs total  %6d node%c  %6d path%c@."
            elapsed_user elapsed_total nodes (if nodes = 1 then ' ' else 's') paths (if paths = 1 then ' ' else 's');

        if not (self#should_continue) then begin
            if max_nodes > 0 && nodes = max_nodes then Format.eprintf "Nodes bound of %d reached.@." nodes;
            if max_paths > 0 && paths = max_paths then Format.eprintf "Paths bound of %d reached.@." paths;
            if max_abandoned > 0 && abandoned = max_abandoned then Format.eprintf "Paths bound of %d reached.@." paths;
        end;

        Format.eprintf "@..@."

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
