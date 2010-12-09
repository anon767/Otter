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
    let assoc_list =
        StringMap.fold (fun key time_record assoc_list -> (key, time_record) :: assoc_list) !global_timer []
    in
    let assoc_list = List.stable_sort (fun (key1, _) (key2, __) -> Pervasives.compare key1 key2) assoc_list in
    let max_length = List.fold_left (fun m (k, _) -> max m (String.length k)) 0 assoc_list in
    let dotted str =
        let len = max_length - String.length str in
        if len <= 0 then "" else String.make len '.'
    in
    Format.fprintf ff "%*s  %8s %10s@\n" max_length "" "count" "time (s)";
    List.iter begin fun (key, time_record) ->
        Format.fprintf ff "%s%s: %8d %10.2f@\n" key (dotted key) time_record.count time_record.time_elapsed
    end assoc_list

