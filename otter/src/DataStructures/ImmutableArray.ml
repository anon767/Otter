
module type ElementType = sig
    type t
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


    type t =
        {
            map: Elt.t IndexMap.t;
            length: int;
            offset: int;
            default: Elt.t option;
        }


    let length array = array.length


    let get array i =
        if i < 0 || i >= array.length then raise Out_of_bounds;
        try
            IndexMap.find (array.offset + i) array.map
        with Not_found ->
            match array.default with
                | None -> failwith "Array element not initialized" (* error occurs when someone declares an array of size 0, and read from it *)
                | Some x -> x


    let set array i x =
        if i < 0 || i >= array.length then raise Out_of_bounds;
        { array with map = IndexMap.add (i + array.offset) x array.map }


    let sub array offset length =
        if offset < 0 then invalid_arg "negative offset";
        if length <= 0 then invalid_arg "negative or zero length";
        if length > array.length || offset > array.length - length then raise Out_of_bounds;
        { array with length; offset = array.offset + offset }


    let make length x =
        if length <= 0 then invalid_arg "negative or zero length";
        { length; offset = 0; map = IndexMap.empty; default = Some x }


    let of_list xs =
        if xs = [] then invalid_arg "empty list";
        let length, map = List.fold_left (fun (i, map) x -> (i + 1, IndexMap.add i x map)) (0, IndexMap.empty) xs in
        { length; offset = 0; map; default = Some (List.hd xs) }


    let equal xs ys =
        let module E = struct exception Not_equal end in
        let shallow_equal () =
            let default_equal () = xs.default == ys.default ||
                match xs.default, ys.default with
                    | Some x, Some y -> Elt.equal x y
                    | _ -> false
            in
            xs.offset = ys.offset
            && xs.map == ys.map
            && default_equal ()
        in
        let deep_equal () =
            try
                if xs.length > 0 then
                    for xk = xs.offset to xs.offset + xs.length - 1 do
                        let xv_opt = try Some (IndexMap.find xk xs.map) with Not_found -> xs.default in

                        let yk = xk - xs.offset + ys.offset in
                        let yv_opt = try Some (IndexMap.find yk ys.map) with Not_found -> ys.default in

                        match xv_opt, yv_opt with
                            | Some xv, Some yv when Elt.equal xv yv -> ()
                            | None, None -> ()
                            | _, _ -> raise E.Not_equal
                    done;
                true
            with E.Not_equal ->
                false
        in
        xs == ys || xs.length = ys.length && (shallow_equal () || deep_equal ())


    let hash xs =
        let rec hash h n =
            if n < xs.length then
                let x_opt = try Some (IndexMap.find (n + xs.offset) xs.map) with Not_found -> xs.default in
                let h = match x_opt with
                    | Some x -> Hashtbl.hash (Elt.hash x, h)
                    | None -> Hashtbl.hash (`None, h)
                in
                hash h (n + 1)
            else
                h
        in
        hash (Hashtbl.hash xs.length) 0


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
