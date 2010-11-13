(* Timer *)
module StringMap = Map.Make (String)

type time_record = {
    time_elapsed : float;
    timer_started : bool;
}

let empty_time_record = {
    time_elapsed = 0.0;
    timer_started = false;
}

let global_timer = ref StringMap.empty

let time key f x =
    let time_record =
        try StringMap.find key (!global_timer)
        with Not_found -> empty_time_record
    in
    if time_record.timer_started then
        f x
    else begin
        global_timer := StringMap.add key {time_record with timer_started = true} (!global_timer);
        let starting_time = Unix.gettimeofday () in
        let return = f x in
        let time_elapsed = Unix.gettimeofday () -. starting_time in
        global_timer := StringMap.add key {
            time_elapsed = time_record.time_elapsed +. time_elapsed;
            timer_started = false;
        } (!global_timer);
        return
    end

let lookupTime key =
    let time_record =
        try StringMap.find key (!global_timer)
        with Not_found -> empty_time_record
    in
    time_record.time_elapsed

let keys () =
    StringMap.fold (fun k _ lst -> k :: lst) (!global_timer) []

