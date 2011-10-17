open OcamlUtilities

(* Forward *)
type queues = [
    | `BackOtterClosestToTargets
    | `BackOtterClosestToTargetsIntraprocedural
    | `BackOtterClosestToTargetsPathWeighted
    | `BackOtterRoundRobinSDSERandomPath
    | `BackOtterRandomPathReachable
    | `BackOtterBatchInterSDSE
    | `BackOtterBatchInterSDSE_rr
    | `BackOtterInterSDSE_rr
    | `BackOtterInterSDSE_pr
    | OtterQueue.Queue.queues
    (* Aliases of strategy names used in the DSE paper *)
    | `BackOtterIntraSDSE  (* Same as `BackOtterClosestToTargetsIntraprocedural *)
    | `BackOtterInterSDSE  (* Same as `BackOtterClosestToTargets *)
]

let queues : (string * queues) list = [
    "backotter-closest-to-targets", `BackOtterClosestToTargets;
    "backotter-closest-to-targets-intraprocedural", `BackOtterClosestToTargetsIntraprocedural;
    "backotter-closest-to-targets-path-weighted", `BackOtterClosestToTargetsPathWeighted;
    "backotter-roundrobin-SDSE-RandomPath", `BackOtterRoundRobinSDSERandomPath;
    "backotter-RandomPath-reachable", `BackOtterRandomPathReachable;
    (* Aliases of strategy names used in the DSE paper *)
    "backotter-IntraSDSE", `BackOtterIntraSDSE;
    "backotter-InterSDSE", `BackOtterInterSDSE;
    "backotter-InterSDSE-roundrobin", `BackOtterInterSDSE_rr;
    "backotter-InterSDSE-probabilistic", `BackOtterInterSDSE_pr;
    "backotter-batch-InterSDSE", `BackOtterBatchInterSDSE;
    "backotter-batch-InterSDSE-roundrobin", `BackOtterBatchInterSDSE_rr;
] @ (OtterQueue.Queue.queues :> (string * queues) list)

(* BackOtter's ClosestToTargetsStrategy is different from that in OtterQueue *)
let rec get = function
    | `BackOtterClosestToTargets -> new OtterQueue.RankedQueue.t [ new ClosestToTargetsStrategy.t OtterQueue.ClosestToTargetsStrategy.inversely_proportional]
    | `BackOtterClosestToTargetsIntraprocedural -> new OtterQueue.RankedQueue.t [ new ClosestToTargetsStrategy.t ~interprocedural:false OtterQueue.ClosestToTargetsStrategy.inversely_proportional]
    | `BackOtterClosestToTargetsPathWeighted -> new OtterQueue.RankedQueue.t [ new ClosestToTargetsStrategy.t OtterQueue.ClosestToTargetsStrategy.quantized; new OtterQueue.WeightedRandomStrategy.t (new OtterQueue.PathWeightedStrategy.t) ]
    | `BackOtterRoundRobinSDSERandomPath -> new OtterQueue.BatchQueue.t (new OtterQueue.RoundRobinQueue.t [ get `BackOtterInterSDSE_rr; get `PathWeighted ])
    | `BackOtterRandomPathReachable -> new OtterQueue.RandomPathQueue.t (fun job -> ClosestToTargetsStrategy.weight (fun d->d) job < max_int)
    | `BackOtterIntraSDSE  -> get `BackOtterClosestToTargetsIntraprocedural
    | `BackOtterInterSDSE  -> get `BackOtterClosestToTargets
    | `BackOtterInterSDSE_rr  -> new OtterQueue.RankedQueue.t [ new RoundRobinClosestToTargetsStrategy.t OtterQueue.ClosestToTargetsStrategy.inversely_proportional]
    | `BackOtterInterSDSE_pr  -> new OtterQueue.RankedQueue.t [ new OtterQueue.WeightedRandomStrategy.t (new ClosestToTargetsStrategy.t OtterQueue.ClosestToTargetsStrategy.inversely_proportional) ]
    | `BackOtterBatchInterSDSE  -> new OtterQueue.BatchQueue.t (get `BackOtterInterSDSE)
    | `BackOtterBatchInterSDSE_rr  -> new OtterQueue.BatchQueue.t (get `BackOtterInterSDSE_rr)
    | #OtterQueue.Queue.queues as queue -> OtterQueue.Queue.get queue

let default_fqueue = ref (`Generational `BreadthFirst)
let get_default_fqueue () = get !default_fqueue

(* Backward *)
let get_function_backward_queue function_queue queue =
    new FunctionQueue.t (FunctionRanker.get function_queue) (fun () -> get queue)

let default_bqueue = ref `RandomPath
let get_default_bqueue () = get_function_backward_queue !FunctionRanker.default_function_rank !default_bqueue

let options = [
    "--forward-queue",
        Arg.Symbol (fst (List.split queues), fun name -> default_fqueue := List.assoc name queues),
        " Set the default forward job queue in bi-directional BackOtter (default: " ^ (fst (List.find (fun (_, x) -> x = !default_fqueue) queues)) ^ ")";
    "--backward-queue",
        Arg.Symbol (fst (List.split queues), fun name -> default_bqueue := List.assoc name queues),
        " Set the default backward job queue in bi-directional BackOtter (default: " ^ (fst (List.find (fun (_, x) -> x = !default_bqueue) queues)) ^ ")";
]

