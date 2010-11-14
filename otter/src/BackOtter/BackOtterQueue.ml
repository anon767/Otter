open OcamlUtilities

(* Borrow these queues from OtterQueue *)
module GenerationalStrategy = OtterQueue.GenerationalStrategy
module BreadthFirstStrategy = OtterQueue.BreadthFirstStrategy
module ClosestToUncoveredStrategy = OtterQueue.ClosestToUncoveredStrategy
module DepthFirstStrategy = OtterQueue.DepthFirstStrategy
module LeastCoveredStrategy = OtterQueue.LeastCoveredStrategy
module RankedQueue = OtterQueue.RankedQueue
module RandomPathQueue = OtterQueue.RandomPathQueue
module RoundRobinQueue = OtterQueue.RoundRobinQueue

(* Forward *)
let queues = OtterQueue.Queue.queues

let rec get targets_ref = function
    | `BreadthFirst -> new RankedQueue.t [ new BreadthFirstStrategy.t ]
    | `DepthFirst -> new RankedQueue.t [ new DepthFirstStrategy.t ]
    | `RandomPath -> new RandomPathQueue.t
    | `Generational `BreadthFirst -> new RankedQueue.t [ new GenerationalStrategy.t; new BreadthFirstStrategy.t ]
    | `Generational `DepthFirst -> new RankedQueue.t [ new GenerationalStrategy.t; new DepthFirstStrategy.t ]
    | `Generational `Random -> new RankedQueue.t [ new GenerationalStrategy.t ]
    | `LeastCovered -> new RankedQueue.t  [ new LeastCoveredStrategy.t ]
    | `ClosestToUncovered -> new RankedQueue.t [ new ClosestToUncoveredStrategy.t ]
    | `ClosestToTargets -> new RankedQueue.t [ new ClosestToTargetsStrategy.t targets_ref ]
    | `Generational `ClosestToTargets -> new RankedQueue.t [ new GenerationalStrategy.t; new ClosestToTargetsStrategy.t targets_ref ]
    | `RoundRobin queues -> new RoundRobinQueue.t (List.map (get targets_ref) queues)
    | `KLEE -> get targets_ref (`RoundRobin [ `ClosestToUncovered; `RandomPath ])

let default_fqueue = ref (`Generational `BreadthFirst)
let get_default_fqueue targets_ref = get targets_ref !default_fqueue

(* Backward *)
let get_function_backward_queue targets_ref function_queue queue =
    new FunctionQueue.t (BackwardRank.get function_queue) (fun () -> get targets_ref queue)

let default_bqueue = ref `ClosestToTargets
let get_default_bqueue targets_ref = get_function_backward_queue targets_ref !BackwardRank.default_brank !default_bqueue

let options = [
    "--forward-queue",
        Arg.Symbol (fst (List.split queues), fun name -> default_fqueue := List.assoc name queues),
        "<queue name> Set the default forward job queue in bi-directional BackOtter (default: " ^ (fst (List.find (fun (_, x) -> x = !default_fqueue) queues)) ^ ")";
    "--backward-queue",
        Arg.Symbol (fst (List.split queues), fun name -> default_bqueue := List.assoc name queues),
        "<queue name> Set the default backward job queue in bi-directional BackOtter (default: " ^ (fst (List.find (fun (_, x) -> x = !default_bqueue) queues)) ^ ")";
]

