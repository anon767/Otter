open CilUtilities
open OcamlUtilities
open OtterCore

let ranker eval lst =
    (* TODO: doesn't need to recalculate every time (although this is not bottleneck *)
    let lst = List.map (fun (file, fundec) -> (fundec, eval (file, fundec))) lst in
    let lst = List.sort (fun (f1, r1) (f2, r2) -> (Pervasives.compare r2 r1)) lst in
    List.map (fun (f, _) -> f) lst

(* FIXME: this can be inefficient if the function is unreachable from entryfn *)
let distance_from_entryfn (file, fundec) =
    let entry_fn = ProgramPoints.get_entry_fundec file in
    let distance = CilCallgraph.get_distance file entry_fn fundec in
    -. (float_of_int distance)

(* TODO: this is not compatible with line-targets *)
let distance_from_failurefn (file, fundec) =
    let failure_fn = ProgramPoints.get_failure_fundec file in
    let distance = CilCallgraph.get_distance file fundec failure_fn in
    -. (float_of_int distance)

let random_function _ = Random.float 1.0

let total rank_fn = rank_fn
let partial rank_fn = if Random.bool () then rank_fn else (ranker random_function)

let queues = [
    "closest-to-entry", `ClosestToEntry;
    "closest-to-failure", `ClosestToFailure;
    "partial*closest-to-entry", `Partial `ClosestToEntry;
    "random-function", `RandomFunction;
]

let get = function
    | `ClosestToEntry -> total (ranker distance_from_entryfn)
    | `Partial `ClosestToEntry -> partial (ranker distance_from_entryfn)
    | `ClosestToFailure -> total (ranker distance_from_failurefn)
    | `RandomFunction -> total (ranker random_function)

let default_brank = ref `ClosestToEntry

let get_default_brank () = get !default_brank

let options = [
    "--backward-function-rank",
        Arg.Symbol (fst (List.split queues), fun name -> default_brank := List.assoc name queues),
        "<ranking name> Set the default backward function ranking in BackOtter (default: " ^ (fst (List.find (fun (_, x) -> x = !default_brank) queues)) ^ ")";
]

