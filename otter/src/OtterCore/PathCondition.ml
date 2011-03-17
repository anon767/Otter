open OtterBytes

module T : sig
    type t
    val empty : t
    val is_empty : t -> bool
    val length : t -> int
    val add : Bytes.bytes -> bool -> t -> t
    val clauses : t -> Bytes.bytes list
    val fold : ('a -> Bytes.bytes -> bool -> 'a) -> 'a -> t -> 'a
end = struct
    type t = Bytes.bytes list * bool list * int
    let empty = ([], [], 0)
    let is_empty (_, _, length) = length = 0
    let length (_, _, length) = length
    let add bytes track (clauses, tracked, length) = (bytes::clauses, track::tracked, length + 1)
    let clauses (clauses, _, _) = clauses
    let fold f acc (clauses, tracked, _) = List.fold_left2 f acc clauses tracked
end

include T
