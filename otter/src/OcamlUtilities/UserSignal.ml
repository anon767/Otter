
exception UserInterrupt
exception TimedOut

let default_timeout = ref 0

let using_signals ?(timeout=(!default_timeout)) f =
    let old_INT_handler = Sys.signal Sys.sigint Sys.Signal_ignore in
    let old_ALRM_handler = Sys.signal Sys.sigalrm Sys.Signal_ignore in
    let old_USR1_handler = Sys.signal Sys.sigusr1 Sys.Signal_ignore in

    let reset () =
        ignore (Unix.alarm 0);
        Sys.set_signal Sys.sigalrm old_ALRM_handler;
        Sys.set_signal Sys.sigint old_INT_handler;
        Sys.set_signal Sys.sigint old_USR1_handler
    in
    let handle exn =
        Sys.Signal_handle (fun _ -> reset (); raise exn)
    in

    Sys.set_signal Sys.sigint (handle UserInterrupt);

    Sys.set_signal Sys.sigalrm (handle TimedOut);
    if timeout < 0 then invalid_arg "timeout must not be negative";
    if timeout > 0 then ignore (Unix.alarm timeout);

    (* print a stack trace upon SIGUSR1 *)
    Sys.set_signal Sys.sigusr1 begin Sys.Signal_handle begin fun _ ->
        let child = Unix.fork () in
        if child = 0 then begin
            (* move stderr to a different file descriptor and redirect the original to /dev/null, to avoid printing
             * any unflushed output as well as the standard "Fatal error" message from the uncaught exception below *)
            let stderr = Unix.out_channel_of_descr (Unix.dup Unix.stderr) in
            let null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o666 in
            Unix.dup2 null Unix.stdout;
            Unix.dup2 null Unix.stderr;
            (* start recording, raise an (uncaught) exception, and print the recorded backtrace; use a local exception
             * so that it cannot be caught anywhere else *)
            Printexc.record_backtrace true;
            let module M = struct exception StackTrace end in
            at_exit begin fun () ->
                output_string stderr "UserSignal.StackTrace\n";
                Printexc.print_backtrace stderr
            end;
            raise M.StackTrace
        end else
            ignore (Unix.waitpid [] child)
    end end;

    let x = f () in
    reset ();
    x

let options = [
	("--timeout",
		Arg.Set_int default_timeout,
		"<time in seconds> Set a timeout");
]

