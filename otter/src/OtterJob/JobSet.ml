module Map = Map.Make(OcamlUtilities.Module.Int)

let empty = Map.empty
let is_empty = Map.is_empty
let mem elt = Map.mem elt#node_id
let add elt = Map.add elt#node_id elt
let singleton elt = Map.singleton elt#node_id elt
let remove elt = Map.remove elt#node_id
let compare x y = Map.compare (fun _ _ -> 0) x y
let equal x y = Map.equal (fun _ _ -> true) x y
let iter f = Map.iter (fun k v -> f v)
let fold f = Map.fold (fun k v a -> f v a)
let for_all p = Map.for_all (fun k v -> p v)
let exists p = Map.exists (fun k v -> p v)
let filter p = Map.filter (fun k v -> p v)
let partition p = Map.partition (fun k v -> p v)
let cardinal = Map.cardinal
let min_elt s = snd (Map.min_binding s)
let max_elt s = snd (Map.max_binding s)
let choose s = snd (Map.choose s)
let split elt s = let l, data, r = Map.split elt s in l, (match data with None -> false | Some _ -> true), r

let union s1 s2 = Map.merge (fun k v1 v2 -> match v1,v2 with None, None -> None | None, Some(v) | Some(v), _ -> Some(v)) s1 s2
let inter s1 s2 = Map.merge (fun k v1 v2 -> match v1,v2 with Some(v), Some(_) -> Some(v) | _ -> None) s1 s2
let diff s1 s2 = Map.merge (fun k v1 v2 -> match v1,v2 with Some(v), None -> Some(v) | _ -> None) s1 s2

let elements s = fold (fun elt lst -> elt :: lst) s []

(* TODO:
let subset = 
*)
