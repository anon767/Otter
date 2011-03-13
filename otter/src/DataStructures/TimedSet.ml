(** 
 * A set module that keeps track of times when elements are added.
 * The underlying data-structure is a map from the set elements to times as floats.
 *
 * For intersection/union, when an element appears in both sets, the earlier time is used.
 *)
module Record = struct
    type t = {
        first_seen: float
    }

    let empty = {
        first_seen = max_float
    }
    
    let merge r1 r2 = {
        first_seen = Pervasives.min r1.first_seen r2.first_seen
    }
end

module Make (Ord: Set.OrderedType) = struct
    
    let time_elapsed =
        let time_start = Unix.gettimeofday () in
        function () -> Unix.gettimeofday () -. time_start

    module M = Map.Make (Ord)

    type t = Record.t M.t

    let empty = M.empty
    let is_empty = M.is_empty
    let cardinal = M.cardinal
    let iter f s = M.iter (fun elt _ -> f elt) s
    let elements s = M.fold (fun elt t lst -> (elt, t)::lst) s []

    let add elt s = 
        let t = time_elapsed () in
        try
            let r = M.find elt s in
            if r.Record.first_seen <= t then s else M.add elt {Record.first_seen = t} s
        with Not_found -> M.add elt {Record.first_seen = t} s

    let union s1 s2 = M.merge (
        fun _ r1opt r2opt -> match r1opt, r2opt with
        | Some r, None
        | None, Some r -> Some r
        | Some r1, Some r2 -> Some (Record.merge r1 r2)
        | None, None -> None (* Can't happen *)
    ) s1 s2

    let inter s1 s2 = M.merge (
        fun _ r1opt r2opt -> match r1opt, r2opt with
        | Some r1, Some r2 -> Some (Record.merge r1 r2)
        | _, _ -> None 
    ) s1 s2

    let diff s1 s2 =  M.merge (
        fun _ r1opt r2opt -> match r1opt, r2opt with
        | Some r, None -> Some r
        | _, _ -> None 
    ) s1 s2

end
