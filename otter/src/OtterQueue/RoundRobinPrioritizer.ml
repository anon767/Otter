(** Job are prioritized from a list of prioritizer_live_list in rotation.  *)

open DataStructures

module Queue = RoundRobinQueue.Queue

class ['self] t prioritizer_live_list = object (self : 'self)
    initializer if List.length prioritizer_live_list < 2 then invalid_arg "RoundRobinPrioritizer.t: less than 2 prioritizers"

    val mutable prioritizer_live_list = Queue.from_list prioritizer_live_list

    method add job =
        {< prioritizer_live_list = Queue.map (fun (prioritizer,live) -> prioritizer#add job,live) prioritizer_live_list >}

    method remove job =
        {< prioritizer_live_list = Queue.map (fun (prioritizer,live) -> prioritizer#remove job,live) prioritizer_live_list >}

    method prioritize subset_opt = OcamlUtilities.Profiler.global#call "RoundRobinPrioritizer.t#prioritize" begin fun () ->
        let rec choose prioritizer_live_list =
            try
                let prioritizer_live_list, (prioritizer,live) = Queue.dequeue prioritizer_live_list in
                if live () then Some(prioritizer, Queue.enqueue (prioritizer,live) prioritizer_live_list)
                else 
                    let _ = OcamlUtilities.Output.must_printf "Remove a prioritizer@." in
                    choose prioritizer_live_list
            with Not_found -> None
        in
        match choose prioritizer_live_list with
        | None -> []
        | Some (prioritizer, prioritizer_live_list') ->
            prioritizer_live_list <- prioritizer_live_list';
            prioritizer#prioritize subset_opt
    end
end


