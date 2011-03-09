
type t = {
    path: Decision.t list;
    length: int;
}

let empty = {
    path = [];
    length = 0;
}

(* Structural compare *)
let compare p1 p2 = 
    match Pervasives.compare p1.length p2.length with
    | 0 -> OcamlUtilities.ListPlus.compare Decision.compare p1.path p2.path
    | i -> i

(* Structural equality *)
let equal x y = compare x y = 0

let hash = Hashtbl.hash

let add decision decision_path = { 
    path = decision :: decision_path.path; 
    length = decision_path.length + 1; 
}

let hd decision_path = List.hd decision_path.path

let tl decision_path = {
    path = List.tl decision_path.path; 
    length = decision_path.length - 1
}

let rev decision_path = {decision_path with path = List.rev decision_path.path}

let length decision_path = decision_path.length

let print ff decision_path =
    if decision_path.length = 0 then
        Format.fprintf ff "Decision: (none)@\n"
    else
        List.iter (Decision.print ff) decision_path.path
