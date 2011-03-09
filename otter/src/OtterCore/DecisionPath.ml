type t = Decision.t list
let compare = OcamlUtilities.ListPlus.compare Decision.compare 

let print ff decisions =
    if decisions = [] then
        Format.fprintf ff "Decision: (none)@\n"
    else
        List.iter (Decision.print ff) decisions
