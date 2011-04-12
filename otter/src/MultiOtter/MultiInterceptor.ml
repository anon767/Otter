open OtterCore
open MultiJob
open Job

let abandon_io_block_deadlock_interceptor job interceptor =
    match job#priority with
        | IOBlock _ -> (* The best job availiable is blocking. This is a deadlock. Each job will be abandoned one at a time. *)
            Complete (Abandoned (`Failure "Deadlock", job))
        | TimeWait _ -> (* If we choose a TimeWait, convert it to a Running rather than just letting its time tick down *)
            interceptor (job#with_priority Running)
        | _ ->
            interceptor job
