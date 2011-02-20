let timing_methods = [
    "real", `TimeReal;          (* Wall-clock time *)
    "stpcount", `TimeStpCount;  (* Time approximated by # of STP queries *)
]

let default_timing_method = ref `TimeStpCount

let get_time_now =
    match !default_timing_method with
    | `TimeReal -> Unix.gettimeofday
    | `TimeStpCount ->
        fun () ->
            let count = DataStructures.NamedCounter.get "stpc_query" in
            float_of_int count

let options = [
    "--timing-method",
        Arg.Symbol (fst (List.split timing_methods), fun name -> default_timing_method := List.assoc name timing_methods),
        "<timing method> Set the default timing method (default: " ^ (fst (List.find (fun (_, x) -> x = !default_timing_method) timing_methods)) ^ ")";
]

