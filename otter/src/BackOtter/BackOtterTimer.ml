let timing_methods = [
    "real", `TimeReal;           (* Wall-clock time *)
    "stp-calls", `TimeStpCalls;  (* Time approximated by # of STP queries *)
    "weighted", `TimeWeighted;   (* A weighted formula of #STP calls and regular steps *)
]

let default_timing_method = ref `TimeStpCalls

let get_time_now =
    (* TODO: make this a function ref instead of pattern match *)
    match !default_timing_method with
    | `TimeReal -> Unix.gettimeofday
    | `TimeStpCalls ->
        fun () ->
            let count = DataStructures.NamedCounter.get "stpc_query" in
            float_of_int count
    | `TimeWeighted ->
        fun () ->
            let stp_count = DataStructures.NamedCounter.get "stpc_query" in
            let step_count = DataStructures.NamedCounter.get "step" in
            float_of_int (50 * stp_count + step_count)

class t = object (self)
    val t_entryfn = 0.0
    val t_otherfn = 0.0
    val last = None

    method time_elapsed = (t_entryfn, t_otherfn)

    method private time kind =
        DataStructures.NamedCounter.incr "step";
        let t_now = get_time_now () in
        match last with
        | None -> {< last = Some (t_now, kind) >}
        | Some (t_last, kind_last) ->
            let t_elapsed = t_now -. t_last in
            match kind_last with
            | `TKindEntry ->
                    {< last = Some (t_now, kind); 
                       t_entryfn = t_entryfn +. t_elapsed; >}
            | `TKindOther ->
                    {< last = Some (t_now, kind); 
                       t_otherfn = t_otherfn +. t_elapsed; >}

    method time_entryfn = self#time `TKindEntry
    method time_otherfn = self#time `TKindOther
end


let options = [
    "--backotter-timing-method",
        Arg.Symbol (fst (List.split timing_methods), fun name -> default_timing_method := List.assoc name timing_methods),
        " Set the default timing method (default: " ^ (fst (List.find (fun (_, x) -> x = !default_timing_method) timing_methods)) ^ ")";
]

