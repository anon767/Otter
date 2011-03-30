open OcamlUtilities
open OtterCore

(** Main symbolic execution loop. Copied from OtterCore.Driver. *)
let main_loop entry_fn interceptor queue reporter =
    (* set up a checkpoint to rollback to upon SignalException *)
    let checkpoint = ref (queue, reporter) in
    try
        (* compose the interceptor with the core symbolic executor *)
        let step = fun job -> 
            DataStructures.NamedCounter.incr "step";
            fst (interceptor job () Statement.step) 
        in
        let rec run (queue, reporter) =
            checkpoint := (queue, reporter);
            match queue#get with
                | Some (queue, job) ->
                    let result =
                        (* The difference between timing here and timing in BidirectionalQueue is that
                         * here we only time the stepping of the job, whereas in BidirectionalQueue we
                         * also include the time of getting a job. *)
                        let fundec = BackOtterUtilities.get_origin_function job in
                        let tkind = if fundec == entry_fn then `TKindEntry else `TKindOther in
                        (* TODO: count the time somewhere else, so main_loop doesn't depend on entry_fn *)
                        BackOtterTimer.time tkind (fun () -> step job)
                    in
                    let rec process_result (queue, reporter) result =
                        let reporter = reporter#report result in
                        match result with
                            | Job.Active job ->
                                (queue#put job, reporter)
                            | Job.Fork results ->
                                List.fold_left process_result (queue, reporter) results
                            | Job.Complete completion ->
                                (queue, reporter)
                    in
                    let queue, reporter = process_result (queue, reporter) result in
                    if reporter#should_continue then
                        run (queue, reporter)
                    else
                        (queue, reporter)
                | None ->
                    (queue, reporter)
        in
        run (queue, reporter)
    with UserSignal.UserInterrupt | UserSignal.TimedOut as exn ->
        (* if we got a signal, stop and return the checkpoint results *)
        Output.set_mode Output.MSG_MUSTPRINT;
        Output.printf "%s@." (Printexc.to_string exn);
        !checkpoint

