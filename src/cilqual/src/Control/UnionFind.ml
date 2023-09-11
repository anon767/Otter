open Data

(* Union-find based many-to-one map, with Map-like interface *)
module UnionFindMap (Key : OrderedType) = struct
    module Map = Map.Make (Key)
    type 'a unified = Unified of Key.t
                    | Head of int * 'a
    type 'a t = 'a unified Map.t

    let empty = Map.empty
    let is_empty = Map.is_empty
    let mem = Map.mem

    let find_compress key map =
        (* path compression: collect all keys during traversal to the head *)
        let rec follow key trail trail' = match Map.find key map with
            | Unified parent     -> follow parent (key::trail) trail
            | Head (rank, value) -> (key, rank, value, trail')
        in
        let head, rank, value, trail = follow key [] [] in
        (* rehook all keys traversed to the head key *)
        let map = List.fold_left (fun u x -> Map.add x (Unified head) u) map trail in
        (head, rank, value, map)

    let find key map =
        (* without path compression *)
        let rec follow key = match Map.find key map with
            | Unified parent  -> follow parent
            | Head (_, value) -> value
        in
        follow key

    let union x y map =
        let x, rx, vx, map = find_compress x map in
        let y, ry, vy, map = find_compress y map in
        if Key.compare x y == 0 then
            (x, y, None, map)
        else
            (* union-by-rank *)
            if rx >= ry then
                let rx = if rx = ry then rx + 1 else rx in
                (x, y, Some (vx, vy), Map.add x (Head (rx, vx)) (Map.add y (Unified x) map))
            else
                (y, x, Some (vy, vx), Map.add y (Head (ry, vy)) (Map.add x (Unified y) map))

    let add key value map = try
        let head, rank, _, map = find_compress key map in
        Map.add head (Head (rank, value)) map
    with Not_found ->
        Map.add key (Head (0, value)) map

    let fold f map acc = Map.fold begin fun key unified acc ->
        let value = match unified with
            | Unified parent  -> find parent map
            | Head (_, value) -> value
        in
        f key value acc
    end map acc
end


(* Like UnionFindMap, but additionally keeps track of the equivalence classes of keys *)
module UnionFindClassMap (Key : OrderedType) = struct
    module Map = UnionFindMap (Key)
    module Class = Set.Make (Key)
    module ClassSet = Set.Make (Class)

    type 'a t = ('a * Class.t) Map.t * ClassSet.t

    let empty = (Map.empty, ClassSet.empty)
    let is_empty (map, _) = Map.is_empty map
    let mem x (map, _) = Map.mem x map

    let find_compress key (map, classes) =
        let head, rank, value, map = Map.find_compress key map in
        (head, rank, fst value, (map, classes))

    let find key (map, _) = fst (Map.find key map)

    let union x y (map, classes) =
        let x, y, unified, map = Map.union x y map in
        match unified with
            | Some ((vx, sx), (vy, sy)) ->
                let sxy = (Class.union sx sy) in
                let map = Map.add x (vx, sxy) map in
                let classes = ClassSet.add sxy (ClassSet.remove sx (ClassSet.remove sy classes)) in
                (x, y, Some (vx, vy), (map, classes))
            | None ->
                (x, y, None, (map, classes))

    let add key value (map, classes) =
        try
            let _, _, (_, set), map = Map.find_compress key map in
            (Map.add key (value, set) map, classes)
        with Not_found ->
            let set = Class.singleton key in
            (Map.add key (value, set) map, ClassSet.add set classes)

    let fold f (map, _) acc = Map.fold (fun key (value, _) acc -> f key value acc) map acc

    let find_class key (map, _) = snd (Map.find key map)
    let classes (_, classes) = classes 
    let cardinal (_, classes) = ClassSet.cardinal classes
end

