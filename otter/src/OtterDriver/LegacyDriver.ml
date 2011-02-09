open OcamlUtilities
open OtterCore

let main_loop get_job interceptor process_result job_queue reporter =
    (* set up a checkpoint to rollback to upon SignalException *)
    let checkpoint = ref (job_queue, reporter) in
    try
        let rec run job_queue reporter =
            checkpoint := (job_queue, reporter);
            match get_job job_queue with
                | Some (job, job_queue2) ->
                    let result, job_queue = interceptor job job_queue2 in
                    let job_queue, reporter = process_result result job_queue reporter in
                    run job_queue reporter
                | None ->
                    (job_queue, reporter)
        in
        run job_queue reporter
    with State.SignalException s ->
        (* if we got a signal, stop and return the completed results *)
        Output.set_mode Output.MSG_MUSTPRINT;
        Output.printf "%s@\n" s;
        !checkpoint
