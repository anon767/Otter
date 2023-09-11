type timing_methods = [
    | `TimeReal      (* Wall-clock time *)
    | `TimeStpCalls  (* Time approximated by # of STP queries *)
    | `TimeWeighted  (* A weighted formula of #STP calls and regular steps *)
]

let timing_methods = [
    "real", `TimeReal;           (* Wall-clock time *)
    "stp-calls", `TimeStpCalls;  (* Time approximated by # of STP queries *)
    "weighted", `TimeWeighted;   (* A weighted formula of #STP calls and regular steps *)
]

let default_timing_method = ref `TimeStpCalls
let arg_stp_call_weight = ref 50

let get_time_now_with_method (timing_method:timing_methods) =
    (* TODO: make this a function ref instead of pattern match *)
    match timing_method with
    | `TimeReal -> Unix.gettimeofday ()
    | `TimeStpCalls ->
            let count = DataStructures.NamedCounter.get "stpc_query" in
            float_of_int count
    | `TimeWeighted ->
            let stp_count = DataStructures.NamedCounter.get "stpc_query" in
            let step_count = DataStructures.NamedCounter.get "step" in
            float_of_int ((!arg_stp_call_weight) * stp_count + step_count)

let get_time_now () = get_time_now_with_method (!default_timing_method)

class t ?(timing_method=(!default_timing_method)) () = object (self)
    val t_entryfn = 0.0
    val t_otherfn = 0.0
    val last = None

    method time_elapsed = (t_entryfn, t_otherfn)

    method private time kind =
        DataStructures.NamedCounter.incr "step";
        let t_now = get_time_now_with_method timing_method in
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

    method time_forward = self#time `TKindEntry
    method time_backward = self#time `TKindOther
end


let options = [
    "--backotter-timing-method",
        Arg.Symbol (fst (List.split timing_methods), fun name -> default_timing_method := List.assoc name timing_methods),
        " Set the default timing method (default: " ^ (fst (List.find (fun (_, x) -> x = !default_timing_method) timing_methods)) ^ ")";
    "--backotter-timing-stp-weight",
        Arg.Set_int arg_stp_call_weight,
        Printf.sprintf "<weight> Set the weight of an STP call for weighted timing method (default:%d)" (!arg_stp_call_weight);
]

