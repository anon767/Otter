open OcamlUtilities

(* Forward *)
let queues = OtterQueue.Queue.queues

(* BackOtter's ClosestToTargetsStrategy is different from that in OtterQueue *)
let rec get = function
    | `ClosestToTargets -> new OtterQueue.RankedQueue.t [ new ClosestToTargetsStrategy.t OtterQueue.ClosestToTargetsStrategy.inversely_proportional]
    | `ClosestToTargetsPathWeighted -> new OtterQueue.RankedQueue.t [ new ClosestToTargetsStrategy.t OtterQueue.ClosestToTargetsStrategy.quantized; new OtterQueue.WeightedRandomStrategy.t (new OtterQueue.PathWeightedStrategy.t) ]
    | queue -> OtterQueue.Queue.get queue

let default_fqueue = ref (`Generational `BreadthFirst)
let get_default_fqueue () = get !default_fqueue

(* Backward *)
let get_function_backward_queue function_queue queue =
    new FunctionQueue.t (FunctionRanker.get function_queue) (fun () -> get queue)

let default_bqueue = ref `RandomPath
let get_default_bqueue () = get_function_backward_queue !FunctionRanker.default_brank !default_bqueue

let options = [
    "--forward-queue",
        Arg.Symbol (fst (List.split queues), fun name -> default_fqueue := List.assoc name queues),
        " Set the default forward job queue in bi-directional BackOtter (default: " ^ (fst (List.find (fun (_, x) -> x = !default_fqueue) queues)) ^ ")";
    "--backward-queue",
        Arg.Symbol (fst (List.split queues), fun name -> default_bqueue := List.assoc name queues),
        " Set the default backward job queue in bi-directional BackOtter (default: " ^ (fst (List.find (fun (_, x) -> x = !default_bqueue) queues)) ^ ")";
]

