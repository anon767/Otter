open DataStructures

type t = {
    path: string list;
    activated: bool;
    length: int;
}

let empty = {
    path = [];
    activated = false;
    length = 0;
}

let from_str =
    let remove_extra_spaces =
        let replace_newlines str = for i = 0 to String.length str - 1 do if str.[i] = '\n' then str.[i] <- ' ' done in
        let re = Str.regexp "[ \t]+" in
        fun str ->
            replace_newlines str;
            let str = Str.global_replace re "" str in
            str
    in
    let re = Str.regexp "Decision:" in
    fun str ->
        let str = remove_extra_spaces str in
        let path = Str.split re str in
        path

let create str =
    let path = from_str str in
    { path = path;
      activated = false;
      length = List.length path
    }

let length decision_path = decision_path.length

let activated decision_path = decision_path.activated

let match_hd decision_path str = (List.hd decision_path.path) = (match (from_str str) with [r] -> r | _ -> failwith "ExternalBoundingPath: unreachable")

let tl decision_path = {
    path = List.tl decision_path.path;
    activated = true;
    length = decision_path.length - 1
}

let print ff decision_path = 
    if decision_path.length = 0 then
        Format.fprintf ff "ExternalDecision: (none)@\n"
    else
        List.iter (fun str -> Format.fprintf ff "ExternalDecision: %s@\n" str) decision_path.path
