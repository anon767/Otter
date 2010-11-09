open BackOtterUtilities
open CilUtilities
open OcamlUtilities
open OtterCore

let distance_from_entryfn job =
    let file = job.Job.file in
    let entry_fn =
        try
            FindCil.fundec_by_name file !OtterJob.Job.entry_function
        with Not_found ->
            FormatPlus.failwith "Entry function %s not found" !OtterJob.Job.entry_function
    in
    let distance = get_distance_from file entry_fn (get_origin_function job) in
    -. (float_of_int distance)

let distance_from_failurefn job =
    let file = job.Job.file in
    let failure_fn =
        let fname = !Executeargs.arg_failurefn in
        try FindCil.fundec_by_name file fname
        with Not_found -> OcamlUtilities.FormatPlus.failwith "Failure function %s not found" fname
    in
    let distance = get_distance_from file (get_origin_function job) failure_fn in
    -. (float_of_int distance)

let random_function job = Random.float 1.0

let total rank_fn = fun () -> rank_fn
let partial rank_fn = fun () -> if Random.bool () then rank_fn else random_function

let queues = [
    "closest-to-entry", `ClosestToEntry;
    "closest-to-failure", `ClosestToFailure;
    "partial:closest-to-entry", `Partial `ClosestToEntry;
    "random-function", `RandomFunction;
]

let get = function
    | `ClosestToEntry -> total distance_from_entryfn
    | `Partial `ClosestToEntry -> partial distance_from_entryfn
    | `ClosestToFailure -> total distance_from_failurefn
    | `RandomFunction -> total random_function

let default_brank = ref `ClosestToEntry

let get_default_brank () = get !default_brank

let options = [
    "--backward-function-rank",
        Arg.Symbol (fst (List.split queues), fun name -> default_brank := List.assoc name queues),
        "<ranking name> Set the default backward function ranking in BackOtter (default: " ^ (fst (List.find (fun (_, x) -> x = !default_brank) queues)) ^ ")";
]

