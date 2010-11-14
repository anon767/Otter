(* Timer *)
module StringMap = Map.Make (String)

type time_record = {
    time_elapsed : float;
    timer_started : bool;
    count : int;
}

let empty_time_record = {
    time_elapsed = 0.0;
    timer_started = false;
    count = 0;
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
        global_timer := StringMap.add key { time_record with timer_started = true } (!global_timer);
        let starting_time = Unix.gettimeofday () in
        let return = f x in
        let time_elapsed = Unix.gettimeofday () -. starting_time in
        global_timer := StringMap.add key {
            time_elapsed = time_record.time_elapsed +. time_elapsed;
            timer_started = false;
            count = time_record.count + 1;
        } (!global_timer);
        return
    end

let global_printer ff =
    Format.fprintf ff "%40s  %8s %10s@\n" "" "count" "time (s)";
    StringMap.iter begin fun key time_record ->
        Format.fprintf ff "%40s: %8d %10.2f@\n" key time_record.count time_record.time_elapsed
    end !global_timer

