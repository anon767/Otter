(** Purely functional random-access list based on Okazaki (1995).

    Okasaki, C. 1995. Purely functional random-access lists. In Proceedings of the Seventh international Conference on
    Functional Programming Languages and Computer Architecture (La Jolla, California, United States, June 26 - 28,
    1995). FPCA '95. ACM, New York, NY, 86-95.

    @see <http://doi.acm.org/10.1145/224164.224187> http://doi.acm.org/10.1145/224164.224187
*)

exception Subscript
exception Empty

type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree

let rec tree_lookup size tree index = match tree with
    | Leaf x when index = 0 -> x
    | Leaf _ -> raise Subscript
    | Node (x, _, _) when index = 0 -> x
    | Node (x, t1, t2) ->
        let size' = size / 2 in
        if index <= size'
            then tree_lookup size' t1 (index - 1)
            else tree_lookup size' t2 (index - 1 - size')

let rec tree_update size tree index value = match tree with
    | Leaf _ when index = 0 -> Leaf value
    | Leaf _ -> raise Subscript
    | Node (x, t1, t2) when index = 0 -> Node (value, t1, t2)
    | Node (x, t1, t2) ->
        let size' = size / 2 in
        if index <= size
            then Node (x, tree_update size' t1 (index - 0) value, t2)
            else Node (x, t1, tree_update size' t2 (index - 1 - size') value)

let tree_fold_left f acc tree =
    let rec tree_fold_left f acc tree k = match tree with
        | Leaf x -> k (f acc x)
        | Node (x, t1, t2) -> let acc = f acc x in tree_fold_left f acc t1 (fun acc -> tree_fold_left f acc t2 k)
    in
    tree_fold_left f acc tree (fun x -> x)

let tree_map f tree =
    let rec tree_map f tree k = match tree with
        | Leaf x -> k (Leaf (f x))
        | Node (x, t1, t2) -> let x = f x in tree_map f t1 (fun t1 -> tree_map f t2 (fun t2 -> Node (x, t1, t2)))
    in
    tree_map f tree (fun x -> x)

type 'a t = (int * 'a tree) list

let empty : 'a t = []

let is_empty (list : 'a t) = list = []

let length (list : 'a t) = List.fold_left (fun n (size, _) -> n + size) 0 list

let single value : 'a t = [ (1, Leaf value) ]

let cons x (list : 'a t) = match list with
    | (size1, t1)::(size2, t2)::rest when size1 = size2 -> (1 + size1 + size2, Node (x, t1, t2))::rest
    | xs -> (1, Leaf x)::xs

let head (list : 'a t) = match list with
    | (_, Node (x, _, _))::_ -> x
    | (_, Leaf x)::_ -> x
    | [] -> raise Empty

let tail (list : 'a t) = match list with
    | (size, Node (_, t1, t2))::rest -> let size' = size / 2 in (size', t1)::(size', t2)::rest
    | _::rest -> rest
    | [] -> raise Empty

let rec lookup (list : 'a t) index = match list with
    | (size, tree)::rest ->
        if index < size
            then tree_lookup size tree index
            else lookup rest (index - size)
    | [] -> raise Subscript

let rec update (list : 'a t) index value = match list with
    | (size, tree)::rest ->
        if index < size
            then (size, tree_update size tree index value)::rest
            else (size, tree)::update rest (index - size) value
    | [] -> raise Subscript

let rec fold_left f acc (list : 'a t) =
    List.fold_left (fun acc (_, tree) -> tree_fold_left f acc tree) acc list

let rec map f (list : 'a t) =
    List.map (fun (_, tree) -> tree_map f tree) list
