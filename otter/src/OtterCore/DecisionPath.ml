open DataStructures

let decision_counter = Counter.make ~start:0 ()

type t = {
    path: (Decision.t * int) list;  (* A path with id's taken to be snd (List.hd path) *)
    length: int;
}

let empty = {
    path = [];
    length = 0;
}

(* TODO: do we want "physical" comparison instead? *)
(* Structural comparison *)
let compare p1 p2 = 
    match Pervasives.compare p1.length p2.length with
    | 0 -> OcamlUtilities.ListPlus.compare (fun (d1,_) (d2,_) -> Decision.compare d1 d2) p1.path p2.path
    | i -> i

(* Structural equality *)
let equal x y = compare x y = 0

let hash = Hashtbl.hash

let add decision decision_path = { 
    path = (decision,Counter.next decision_counter) :: decision_path.path; 
    length = decision_path.length + 1; 
}

let hd decision_path = fst (List.hd decision_path.path)

let tl decision_path = {
    path = List.tl decision_path.path; 
    length = decision_path.length - 1
}

(* id's are only used when decision paths are not reversed. *)
let rev decision_path = {decision_path with path = List.rev decision_path.path}

let length decision_path = decision_path.length

let id decision_path = if decision_path = empty then -1 else snd (List.hd decision_path.path)

let print ff decision_path =
    if decision_path.length = 0 then
        Format.fprintf ff "Decision: (none)@\n"
    else
        List.iter (fun (d,_) -> Decision.print ff d) decision_path.path

let to_string ff decision_path =
    List.iter (fun (d,_) -> Decision.to_string ff d) decision_path.path
