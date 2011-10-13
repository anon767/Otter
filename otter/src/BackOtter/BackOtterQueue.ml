open OcamlUtilities

(* Forward *)
type queues = [
    | `BackOtterClosestToTargets
    | `BackOtterRoundRobinClosestToTargets
    | `BackOtterProbabilisticClosestToTargets
    | `BackOtterClosestToTargetsIntraprocedural
    | `BackOtterClosestToTargetsPathWeighted
    | `SDSERP
    | `RandomPathPruneUnreachable
    | OtterQueue.Queue.queues
    (* Aliases of strategy names used in the DSE paper *)
    | `BackOtterIntraSDSE  (* Same as `BackOtterClosestToTargetsIntraprocedural *)
    | `BackOtterInterSDSE  (* Same as `BackOtterClosestToTargets *)
    | `BackOtterInterSDSE_rr  (* Same as `BackOtterRoundRobinClosestToTargets *)
    | `BackOtterInterSDSE_pr  (* Same as `BackOtterProbabilisticClosestToTargets *)
]

let queues : (string * queues) list = [
    "backotter-closest-to-targets", `BackOtterClosestToTargets;
    "backotter-round-robin-closest-to-targets", `BackOtterRoundRobinClosestToTargets;
    "backotter-probabilistic-closest-to-targets", `BackOtterProbabilisticClosestToTargets;
    "backotter-closest-to-targets-intraprocedural", `BackOtterClosestToTargetsIntraprocedural;
    "backotter-closest-to-targets-path-weighted", `BackOtterClosestToTargetsPathWeighted;
    "SDSE-RP", `SDSERP;
    "random-path-prune-unreachable", `RandomPathPruneUnreachable;
    (* Aliases of strategy names used in the DSE paper *)
    "backotter-IntraSDSE", `BackOtterIntraSDSE;
    "backotter-InterSDSE", `BackOtterInterSDSE;
    "backotter-InterSDSE-round-robin", `BackOtterInterSDSE_rr;
    "backotter-InterSDSE-probabilistic", `BackOtterInterSDSE_pr;
] @ (OtterQueue.Queue.queues :> (string * queues) list)

(* BackOtter's ClosestToTargetsStrategy is different from that in OtterQueue *)
let rec get = function
    | `BackOtterClosestToTargets -> new OtterQueue.RankedQueue.t [ new ClosestToTargetsStrategy.t OtterQueue.ClosestToTargetsStrategy.inversely_proportional]
    | `BackOtterRoundRobinClosestToTargets -> new OtterQueue.RankedQueue.t [ new RoundRobinClosestToTargetsStrategy.t OtterQueue.ClosestToTargetsStrategy.inversely_proportional]
    | `BackOtterProbabilisticClosestToTargets -> new OtterQueue.RankedQueue.t [ new OtterQueue.WeightedRandomStrategy.t (new ClosestToTargetsStrategy.t OtterQueue.ClosestToTargetsStrategy.inversely_proportional) ]
    | `BackOtterClosestToTargetsIntraprocedural -> new OtterQueue.RankedQueue.t [ new ClosestToTargetsStrategy.t ~interprocedural:true OtterQueue.ClosestToTargetsStrategy.inversely_proportional]
    | `BackOtterClosestToTargetsPathWeighted -> new OtterQueue.RankedQueue.t [ new ClosestToTargetsStrategy.t OtterQueue.ClosestToTargetsStrategy.quantized; new OtterQueue.WeightedRandomStrategy.t (new OtterQueue.PathWeightedStrategy.t) ]
    | `SDSERP -> new OtterQueue.RoundRobinQueue.t [ get `RandomPath; get `BackOtterClosestToTargets]
    | `RandomPathPruneUnreachable -> new OtterQueue.RandomPathQueue.t (fun job -> ClosestToTargetsStrategy.weight (fun d->d) job < max_int)
    (* Aliases of strategy names used in the DSE paper *)
    | `BackOtterIntraSDSE  -> get `BackOtterClosestToTargetsIntraprocedural
    | `BackOtterInterSDSE  -> get `BackOtterClosestToTargets
    | `BackOtterInterSDSE_rr  -> get `BackOtterRoundRobinClosestToTargets
    | `BackOtterInterSDSE_pr  -> get `BackOtterProbabilisticClosestToTargets

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

