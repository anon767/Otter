open OcamlUtilities

(* Borrow these queues from OtterQueue *)
module BatchQueue = OtterQueue.BatchQueue
module BreadthFirstStrategy = OtterQueue.BreadthFirstStrategy
module ClosestToUncoveredStrategy = OtterQueue.ClosestToUncoveredStrategy
module DepthFirstStrategy = OtterQueue.DepthFirstStrategy
module GenerationalStrategy = OtterQueue.GenerationalStrategy
module LeastCoveredStrategy = OtterQueue.LeastCoveredStrategy
module RankedQueue = OtterQueue.RankedQueue
module RandomPathQueue = OtterQueue.RandomPathQueue
module RoundRobinQueue = OtterQueue.RoundRobinQueue

(* Forward *)
let queues = OtterQueue.Queue.queues

let rec get = function
    | `BreadthFirst -> new RankedQueue.t [ new BreadthFirstStrategy.t ]
    | `DepthFirst -> new RankedQueue.t [ new DepthFirstStrategy.t ]
    | `RandomPath -> new RandomPathQueue.t
    | `Generational `BreadthFirst -> new RankedQueue.t [ new GenerationalStrategy.t; new BreadthFirstStrategy.t ]
    | `Generational `DepthFirst -> new RankedQueue.t [ new GenerationalStrategy.t; new DepthFirstStrategy.t ]
    | `Generational `Random -> new RankedQueue.t [ new GenerationalStrategy.t ]
    | `LeastCovered -> new RankedQueue.t  [ new LeastCoveredStrategy.t ]
    | `ClosestToUncovered -> new RankedQueue.t [ new ClosestToUncoveredStrategy.t ]
    | `ClosestToTargets -> new RankedQueue.t [ new ClosestToTargetsStrategy.t ] (* This ClosestToTargetsStrategy is different from that in OtterQueue *)
    | `Generational `ClosestToTargets -> new RankedQueue.t [ new GenerationalStrategy.t; new ClosestToTargetsStrategy.t ]
    | `RoundRobin queues -> new RoundRobinQueue.t (List.map get queues)
    | `KLEE -> new BatchQueue.t (get (`RoundRobin [ `ClosestToUncovered; `RandomPath ]))
    | `SAGE -> new RankedQueue.t [ new GenerationalStrategy.t; new ClosestToUncoveredStrategy.t ]

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

