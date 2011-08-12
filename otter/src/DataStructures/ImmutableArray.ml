(** Sparse immutable array *)

module type ElementType = sig
    type t
    val default : t Lazy.t (* lazy to enable use of recursive modules *)
    val equal : t -> t -> bool
    val hash : t -> int
end

module type S = sig
    exception Out_of_bounds
    type elt
    type t
    val length : t -> int
    val get : t -> int -> elt
    val set : t -> int -> elt -> t
    val sub : t -> int -> int -> t
    val make : int -> elt ->t
    val of_list : elt list -> t
    val equal : t -> t -> bool
    val hash : t -> int
    val foldi : ('acc -> int -> elt -> 'acc) -> 'acc -> t -> 'acc
    val fold_left : ('acc -> elt -> 'acc) -> 'acc -> t -> 'acc
    val map : (elt -> elt) -> t -> t
    val exists : (elt -> bool) -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
end

module Make (Elt : ElementType) : S with type elt = Elt.t = struct
    module IndexMap = Map.Make (struct
        type t = int
        let compare (a : int) (b : int) = Pervasives.compare a b
    end)


    exception Out_of_bounds


    type elt = Elt.t


    type t = { length : int; map : Elt.t IndexMap.t } (* invariant: map must never contain Elt.default *)


    let length array = array.length


    let get array i =
        if i < 0 || i >= array.length then raise Out_of_bounds;
        try
            IndexMap.find i array.map
        with Not_found ->
            Lazy.force Elt.default


    let set array i x =
        if i < 0 || i >= array.length then raise Out_of_bounds;
        let map =
            if Elt.equal x (Lazy.force Elt.default) then
                IndexMap.remove i array.map
            else
                IndexMap.add i x array.map
        in
        { array with map }


    let sub array offset length =
        if offset < 0 then invalid_arg "negative offset";
        if length <= 0 then invalid_arg "negative or zero length";
        let rec sub i map =
            (* copy into a new map, so this is optimized for small slices; the alternative would be to store an offset
             * as part of the type to optimize sub for big slices, but that in turn is ill-optimized for the equal function *)
            if i < length then
                sub (i + 1) (try IndexMap.add i (IndexMap.find (i + offset) array.map) map with Not_found -> map)
            else
                map
        in
        { length; map = sub 0 IndexMap.empty }


    let make length x =
        let map =
            if Elt.equal x (Lazy.force Elt.default) then
                IndexMap.empty
            else
                let rec make i map = if i < length then make (i + 1) (IndexMap.add i x map) else map in
                make 0 IndexMap.empty
        in
        { length; map }


    let of_list xs =
        let default = Lazy.force Elt.default in
        let length, map = List.fold_left begin fun (i, map) x ->
            (i + 1, if Elt.equal x default then map else IndexMap.add i x map)
        end (0, IndexMap.empty) xs in
        { length; map }


    let equal xs ys =
        xs == ys || xs.length = ys.length && (xs.map == ys.map || IndexMap.equal Elt.equal xs.map ys.map)


    let hash xs =
        IndexMap.fold (fun i x h -> Hashtbl.hash (i, Elt.hash x, h)) xs.map xs.length


    (* TODO: remove everything below, as uses of the below cannot take advantage of the sparsity of ImmutableArrays *)

    let foldi f acc array =
        let rec foldi acc index =
            if index < array.length then
                foldi (f acc index (get array index)) (index + 1)
            else
                acc
        in
        foldi acc 0

    let fold_left ff a bs =
        let len = length bs in
        let rec impl ff a bs i =
            if i>=len then a
            else let aa = ff a (get bs i) in
                impl ff aa bs (i+1)
        in
            impl ff a bs 0

    let map ff bs =
        let fff lst bb =
            (ff bb)::lst
        in
        let l1 = fold_left fff [] bs in
        let l2 = List.rev_append l1 [] in
            of_list l2

    let exists p arr =
        let len = length arr in
        let rec exists_aux i =
            if i >= len
            then false
            else p (get arr i) || exists_aux (succ i)
        in
        exists_aux 0

    let for_all p arr =
        let len = length arr in
        let rec for_all_aux i =
            if i >= len
            then true
            else p (get arr i) && for_all_aux (succ i)
        in
        for_all_aux 0
end
