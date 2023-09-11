open CilUtilities
open OcamlUtilities
open OtterCore

(** Evaluation function used by ranker, smaller the better *)
let distance_from_entryfn (file, fundec) =
    if FunctionManager.is_ready_to_run file fundec then
        let entry_fn = ProgramPoints.get_entry_fundec file in
        let distance = CilCallgraph.get_distance file entry_fn fundec in
        float_of_int distance
    else max_float

let random_function _ = Random.float 1.0

let eval_ranker eval lst =
    let lst = List.map (fun (file, fundec) -> (fundec, eval (file, fundec))) lst in
    let lst = List.sort (fun (f1, r1) (f2, r2) -> (Pervasives.compare r1 r2)) lst in
    List.map (fun (f, _) -> f) lst

let least_recently_run_ranker =
    let counter = ref 0 in
    let module FundecMap = Map.Make (CilData.CilFundec) in
    let history = ref FundecMap.empty in
    fun lst ->
        let fundecs = eval_ranker (fun (_, fundec) -> try FundecMap.find fundec (!history) with Not_found -> -1 (* fresh *)) lst in
        assert(fundecs <> []);
        history := FundecMap.add (List.hd fundecs) (!counter) (!history); 
        counter := (!counter) + 1;
        fundecs

let round_robin_ranker rankers =
    let rankers = ref rankers in
    fun lst ->
        let ranker = List.hd (!rankers) in
        rankers := (List.tl (!rankers)) @ [ranker];
        ranker lst


let queues = [
    "closest-to-entry", `ClosestToEntry;
    "random-function", `RandomFunction;
    "least-recently-run", `LeastRecentlyRun;
    "round-robin", `RoundRobin;
]

let rec get = function
    | `ClosestToEntry -> eval_ranker distance_from_entryfn
    | `RandomFunction -> eval_ranker random_function
    | `LeastRecentlyRun -> least_recently_run_ranker
    | `RoundRobin -> round_robin_ranker [ get `ClosestToEntry; get `LeastRecentlyRun ]

let default_function_rank = ref `RoundRobin

let get_default_function_rank () = get !default_function_rank

let options = [
    "--backward-function-rank",
        Arg.Symbol (fst (List.split queues), fun name -> default_function_rank := List.assoc name queues),
        " Set the default backward function ranking in BackOtter (default: " ^ (fst (List.find (fun (_, x) -> x = !default_function_rank) queues)) ^ ")";
]

