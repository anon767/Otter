(* This program exercises Delimcc while running a timer with a signal handler that raises an exception, attempting to
 * reveal any race conditions that might occur when the timer interrupts a core Delimcc operation.
 *
 * It should run forever without failing any assertions or raising any uncaught exceptions.
 *)
open Delimcc

let p = new_prompt ()
let q = new_prompt ()
let r = new_prompt ()
let n = ref 0

let rec baz () =
    try
        incr n;
        if Random.int 6 == 0 then
            abort p ()
        else
            take_subcont p bar
    with
        | Exit when !n < 3 ->
            baz ()
        | Exit ->
            ()

and bar sk () =
    if Random.bool () then begin
        n := 3; (* to stop recursing in baz *)
        push_subcont sk (fun () -> incr n);
        push_subcont sk (fun () -> incr n);
    end else begin
        push_delim_subcont sk (fun () -> baz ());
        push_delim_subcont sk (fun () -> baz ());
    end

let foo () =
    n := 0;
    baz ()

let () =
    Random.self_init ();
    let en = ref false in
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> if Random.int 20 == 0 && !en then raise Exit));
    ignore (Unix.setitimer Unix.ITIMER_REAL { Unix.it_interval=1e-6; Unix.it_value=1e-3 });
    while true do
        try
            en := true;
            (if Random.bool () then push_prompt r else (fun f -> f ())) (fun () ->
                push_prompt p (fun () ->
                    (if Random.bool () then push_prompt q else (fun f -> f ())) foo));
            en := false;
        with Exit ->
            en := false
    done
