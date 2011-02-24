
exception UserInterrupt
exception TimedOut

let default_timeout = ref 0

let using_signals ?(timeout=(!default_timeout)) f =
    let old_INT_handler = Sys.signal Sys.sigint Sys.Signal_ignore in
    let old_ALRM_handler = Sys.signal Sys.sigalrm Sys.Signal_ignore in

    let reset () =
        ignore (Unix.alarm 0);
        Sys.set_signal Sys.sigalrm old_ALRM_handler;
        Sys.set_signal Sys.sigint old_INT_handler
    in
    let handle exn =
        Sys.Signal_handle (fun _ -> reset (); raise exn)
    in

    Sys.set_signal Sys.sigint (handle UserInterrupt);
    Sys.set_signal Sys.sigalrm (handle TimedOut);

    if timeout < 0 then invalid_arg "timeout must not be negative";
    if timeout > 0 then ignore (Unix.alarm timeout);

    let x = f () in
    reset ();
    x

let options = [
	("--timeout",
		Arg.Set_int default_timeout,
		"<time in seconds> Set a timeout");
]

