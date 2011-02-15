(** Useful extensions to the {!Unix} module. *)

exception ForkCallException of exn
exception ForkCallFailure of exn
exception ForkCallExited of int
exception ForkCallKilled of int
(**/**) (* should never occur *)
exception ForkCallStopped of int
(**/**)

exception TimedOut 


(** Call a function in a forked process and return the result. Note that {!Format.std_formatter} and
    {!Format.err_formatter} are reset in the forked process.
        @param f is the function to call
        @param x is the argument to the function

        @return the result of [f x]

        @raise ForkCallException if [f x] raises an exception; {e note that the raised exception cannot be
                pattern-matched due to a limitation in the Ocaml runtime (see http://caml.inria.fr/mantis/view.php?id=1624)}
        @raise ForkCallFailure if the child process failed to launch or did not return any results
        @raise ForkCallExited if the child process exited unexpectedly
        @raise ForkCallKilled if the child process was killed unexpectedly
*)
let fork_call (f : ('a -> 'b)) (x : 'a) : 'b =
    (* first, flush stdout/stderr to avoid printing twice *)
    flush stdout;
    flush stderr;

    (* create a pipe to proxy the result from child to parent *)
    let fdin, fdout = Unix.pipe () in

    let child = try
        Unix.fork ()
    with e ->
        raise (ForkCallFailure e)
    in
    if child = 0 then begin
        (* reset Format.std_formatter/Format.err_formatter *)
        List.iter begin fun ff ->
            let output, flush = Format.pp_get_formatter_output_functions ff () in
            Format.pp_set_formatter_output_functions ff (fun _ _ _ -> ()) (fun _ -> ());
            Format.pp_print_flush ff ();
            Format.pp_set_formatter_output_functions ff output flush;
        end [ Format.std_formatter; Format.err_formatter ];

        (* child process runs the function and proxies the result to the parent *)
        Unix.close fdin;

        let result = try `Result (f x) with e -> `Exception e in
        Marshal.to_channel (Unix.out_channel_of_descr fdout) result [ Marshal.Closures ];
        exit 0
    end else begin
        (* parent process waits for child, and captures the result *)
        Unix.close fdout;

        (* get the result *)
        let result = try
            (Marshal.from_channel (Unix.in_channel_of_descr fdin) : [> `Result of 'b | `Exception of exn ])
        with e ->
            (* kill the child *)
            Unix.kill child Sys.sigterm;
            `Failure e
        in

        (* make sure to not exhaust file descriptors *)
        Unix.close fdin;

        (* get the child's exit status *)
        let _, status = Unix.waitpid [] child in
        match status, result with
            | Unix.WEXITED 0, `Result res ->
                res
            | Unix.WEXITED 0, `Exception e ->
                (* Marshal does not serialize exceptions faithfully: http://caml.inria.fr/mantis/view.php?id=1624 *)
                (* TODO: provide a helper to match exception by label, using the generic printer trick in Printexc *)
                raise (ForkCallException e)
            | _, `Failure e ->
                raise (ForkCallFailure e)
            | Unix.WEXITED i, _ ->
                raise (ForkCallExited i)
            | Unix.WSIGNALED i, _ ->
                raise (ForkCallKilled i)
            | Unix.WSTOPPED i, _ ->
                (* this should never occur since waitpid wasn't given the WUNTRACED flag *)
                raise (ForkCallStopped i)
    end

(* assert a time limit for running fn () (note: uses [Unix.setitimer Unix.ITIMER_PROF]) *)
let assert_time_limit time_limit fn =
    let old_handler = Sys.signal Sys.sigprof (Sys.Signal_handle (fun _ -> raise TimedOut)) in
    ignore (Unix.setitimer Unix.ITIMER_PROF { Unix.it_interval = 0.; Unix.it_value = time_limit; });
    try
        let result = fn () in
        ignore (Unix.setitimer Unix.ITIMER_PROF { Unix.it_interval = 0.; Unix.it_value = 0.; });
        Sys.set_signal Sys.sigalrm old_handler;
        result
    with TimedOut ->
        ignore (Unix.setitimer Unix.ITIMER_PROF { Unix.it_interval = 0.; Unix.it_value = 0.; });
        Sys.set_signal Sys.sigalrm old_handler;
        raise TimedOut


(** Call a function in a forked process and return the result, or raise {TimedOut} if timeout occurs. 
    Note that {!Format.std_formatter} and {!Format.err_formatter} are reset in the forked process.
        @param time_limit is the time limit in seconds
        @param fn is the function to call

        @return the result of [fn ()]

        @raise Timedout if timeout occurs
*)
let timed_call time_limit fn =
    assert_time_limit time_limit (fork_call fn)

