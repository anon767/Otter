
(* common types *)
module type Type = sig
    type t
end

module type PrintableType = sig
    include Type
    val printer : Format.formatter -> t -> unit
end

module type OrderedType = sig
    include Type
    val compare : t -> t -> int
end

module type PrintableOrderedType = sig
    include OrderedType
    val printer : Format.formatter -> t -> unit
end

module type HashedType = sig
    include OrderedType
    val hash : t -> int
end

module type PrintableHashedType = sig
    include HashedType
    val printer : Format.formatter -> t -> unit
end

module type ComparableType = sig
    include HashedType
    val equal : t -> t -> bool
end

module type PrintableComparableType = sig
    include ComparableType
    val printer : Format.formatter -> t -> unit
end

module Unit = struct
    type t = unit
    let compare () () = 0
    let hash () = 0
    let equal () () = true
    let printer ff () = Format.fprintf ff "()"
end

module String = struct
    include String
    let equal (x : string) (y : string) = x = y
    let hash (s : string) = Hashtbl.hash s
    let printer ff = Format.fprintf ff "%s"
end

