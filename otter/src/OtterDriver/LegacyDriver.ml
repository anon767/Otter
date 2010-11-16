open OcamlUtilities
open OtterCore

let main_loop get_job interceptor process_result job_queue reporter =
    let rec main_loop job_queue reporter =
        try
            match get_job job_queue with
                | Some (job, job_queue2) ->
                      let result_opt =
                          let result, job_queue = interceptor job job_queue2 in
                          let job_queue, reporter = process_result result job_queue reporter in
                          Some (job_queue, reporter)
                      in
                      begin match result_opt with
                          | Some (job_queue, reporter) -> main_loop job_queue reporter
                          | None -> (job_queue, reporter)
                      end
                | None ->
                      (job_queue, reporter)
        with Types.SignalException s ->
            (* if we got a signal, stop and return the completed results *)
            Output.set_mode Output.MSG_MUSTPRINT;
            Output.printf "%s@\n" s;
            (job_queue, reporter)
    in
    main_loop job_queue reporter
