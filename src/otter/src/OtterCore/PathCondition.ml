open OtterBytes

module T : sig
    type t
    val empty : t
    val is_empty : t -> bool
    val length : t -> int
    val add : Bytes.bytes -> bool -> t -> t
    val clauses : t -> Bytes.bytes list
    val fold : ('a -> Bytes.bytes -> bool -> 'a) -> 'a -> t -> 'a
    val print : Format.formatter -> t -> unit
end = struct
    type t = Bytes.bytes list * bool list * int
    let empty = ([], [], 0)
    let is_empty (_, _, length) = length = 0
    let length (_, _, length) = length
    let add bytes track (clauses, tracked, length) = (bytes::clauses, track::tracked, length + 1)
    let clauses (clauses, _, _) = clauses
    let fold f acc (clauses, tracked, _) = List.fold_left2 f acc clauses tracked
    let print ff (clauses, tracked, length) =
        Format.fprintf ff "length: %d@;" length;
        Format.fprintf ff "clauses:@[<v>%a@]@;" (OcamlUtilities.FormatPlus.pp_print_list (fun ff (clause, tracked) -> Format.fprintf ff "@[%a@], tracked:%B" BytesPrinter.bytes clause tracked) "@;") (List.map2 (fun a b -> (a,b)) clauses tracked);
        ()
end

include T
