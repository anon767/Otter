(** 
 * A set module that keeps track of times when elements are added.
 * The underlying data-structure is a map from the set elements to times as floats.
 *
 * For intersection/union, when an element appears in both sets, the earlier time is used.
 *)
module Make (Ord: Set.OrderedType) = struct
    
    let time_elapsed =
        let time_start = Unix.gettimeofday () in
        function () -> Unix.gettimeofday () -. time_start

    module M = Map.Make (Ord)

    type t = float M.t

    let empty = M.empty
    let is_empty = M.is_empty
    let cardinal = M.cardinal
    let iter f s = M.iter (fun elt _ -> f elt) s
    let elements s = M.fold (fun elt t lst -> (elt, t)::lst) s []

    let add elt s = 
        let t = time_elapsed () in
        try
            let t' = M.find elt s in
            if t' <= t then s else M.add elt t s
        with Not_found -> M.add elt t s

    let union s1 s2 = M.merge (
        fun _ t1opt t2opt -> match t1opt, t2opt with
        | Some t, None
        | None, Some t -> Some t
        | Some t1, Some t2 -> Some (Pervasives.min t1 t2)
        | None, None -> None (* Can't happen *)
    ) s1 s2

    let inter s1 s2 = M.merge (
        fun _ t1opt t2opt -> match t1opt, t2opt with
        | Some t1, Some t2 -> Some (Pervasives.min t1 t2)
        | _, _ -> None 
    ) s1 s2

    let diff s1 s2 =  M.merge (
        fun _ t1opt t2opt -> match t1opt, t2opt with
        | Some t, None -> Some t
        | _, _ -> None 
    ) s1 s2

end
