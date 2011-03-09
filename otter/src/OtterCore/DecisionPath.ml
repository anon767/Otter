
type t = Decision.t list * int

let compare (l1, s1) (l2, s2) = 
    match Pervasives.compare s1 s2 with
    | 0 -> OcamlUtilities.ListPlus.compare Decision.compare l1 l2
    | i -> i

let equal x y = compare x y = 0

let hash = Hashtbl.hash

let empty = ([], 0)

let add decision (decisions, length) = (decision :: decisions, length + 1)

let hd (decisions, _) = List.hd decisions

let tl (decisions, length) = (List.tl decisions, length - 1)

let rev (decisions, length) = (List.rev decisions, length)

let length (_, length) = length

let print ff (decisions, length) =
    if length = 0 then
        Format.fprintf ff "Decision: (none)@\n"
    else
        List.iter (Decision.print ff) decisions
