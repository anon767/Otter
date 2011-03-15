let timing_methods = [
    "real", `TimeReal;           (* Wall-clock time *)
    "stp-calls", `TimeStpCalls;  (* Time approximated by # of STP queries *)
]

let default_timing_method = ref `TimeStpCalls


(* time of (entry_fn, other_fn) *)
let timer_ref = ref (0.0, 0.0)

let reset_time () = timer_ref := (0.0, 0.0)

(* Non recursion safe *)
let time tkind fn = 
    let get_time_now =
        match !default_timing_method with
        | `TimeReal -> Unix.gettimeofday
        | `TimeStpCalls ->
            fun () ->
                let count = DataStructures.NamedCounter.get "stpc_query" in
                float_of_int count
    in
    let time_elapsed = get_time_now () in
    let result = fn () in
    let time_elapsed = get_time_now () -. time_elapsed in
    let entry_time, other_time = !timer_ref in
    timer_ref := (
        match tkind with
        | `TKindEntry ->
            (entry_time +. time_elapsed, other_time)
        | `TKindOther ->
            (entry_time, other_time +. time_elapsed)
    );
    result
    
let options = [
    "--backotter-timing-method",
        Arg.Symbol (fst (List.split timing_methods), fun name -> default_timing_method := List.assoc name timing_methods),
        " Set the default timing method (default: " ^ (fst (List.find (fun (_, x) -> x = !default_timing_method) timing_methods)) ^ ")";
]

