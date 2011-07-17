(** Utilities for module operations *)

module Int = struct
    type t = int
    let hash i = i
    let equal = Pervasives.(=)
    let compare = Pervasives.compare
end

module CombineHashedTypes (T1 : Hashtbl.HashedType) (T2 : Hashtbl.HashedType) : Hashtbl.HashedType with type t = T1.t * T2.t = struct
    type t = T1.t * T2.t
    let hash (t1, t2) = Hashtbl.hash (T1.hash t1, T2.hash t2)
    let equal (t1, t2) (t1', t2') = T1.equal t1 t1' && T2.equal t2 t2'
end

module CombineHashedTypes3 (T1 : Hashtbl.HashedType) (T2 : Hashtbl.HashedType) (T3 : Hashtbl.HashedType) : Hashtbl.HashedType with type t = T1.t * T2.t * T3.t = struct
    type t = T1.t * T2.t * T3.t
    let hash (t1, t2, t3) = Hashtbl.hash (T1.hash t1, T2.hash t2, T3.hash t3)
    let equal (t1, t2, t3) (t1', t2', t3') = T1.equal t1 t1' && T2.equal t2 t2' && T3.equal t3 t3'
end

module CombineOrderedTypes (T1 : Map.OrderedType) (T2 : Map.OrderedType) : Map.OrderedType with type t = T1.t * T2.t = struct
    type t = T1.t * T2.t
    let compare (t1, t2) (t1', t2') = match T1.compare t1 t1' with 0 -> T2.compare t2 t2' | i -> i
end

module CombineOrderedTypes3 (T1 : Map.OrderedType) (T2 : Map.OrderedType) (T3 : Map.OrderedType) : Map.OrderedType with type t = T1.t * T2.t * T3.t = struct
    type t = T1.t * T2.t * T3.t
    let compare (t1, t2, t3) (t1', t2', t3') = match T1.compare t1 t1' with 0 -> (match T2.compare t2 t2' with 0 -> T3.compare t3 t3' | i -> i) | i -> i
end
