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
