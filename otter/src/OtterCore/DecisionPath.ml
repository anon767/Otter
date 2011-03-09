
type t = {
    decision_path: Decision.t list;
    length: int;
}

let empty = {
    decision_path = [];
    length = 0;
}

let compare {decision_path=l1; length=s1} {decision_path=l2; length=s2} = 
    match Pervasives.compare s1 s2 with
    | 0 -> OcamlUtilities.ListPlus.compare Decision.compare l1 l2
    | i -> i

let equal x y = compare x y = 0

let hash = Hashtbl.hash

let add decision {decision_path=decisions; length=length} = {decision_path = decision :: decisions; length = length + 1}

let hd {decision_path=decisions; length=_} = List.hd decisions

let tl {decision_path=decisions; length=length} = {decision_path=List.tl decisions; length=length - 1}

let rev {decision_path=decisions; length=length} = {decision_path=List.rev decisions; length=length}

let length {decision_path=_; length=length} = length

let print ff {decision_path=decisions; length=length} =
    if length = 0 then
        Format.fprintf ff "Decision: (none)@\n"
    else
        List.iter (Decision.print ff) decisions
