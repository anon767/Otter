open OcamlUtilities

(* Borrow these queues from OtterQueue *)
module BreadthFirstQueue = OtterQueue.BreadthFirstQueue
module DepthFirstQueue = OtterQueue.DepthFirstQueue
module RandomPathQueue = OtterQueue.RandomPathQueue
module GenerationalQueue = OtterQueue.GenerationalQueue
module LeastCoveredQueue = OtterQueue.LeastCoveredQueue
module ClosestToUncoveredQueue = OtterQueue.ClosestToUncoveredQueue

(* Forward *)
let queues = OtterQueue.Queue.queues

let get targets_ref = function
    | `BreadthFirst -> new BreadthFirstQueue.t
    | `DepthFirst -> new DepthFirstQueue.t
    | `RandomPath -> new RandomPathQueue.t
    | `Generational `BreadthFirst -> new GenerationalQueue.t (new BreadthFirstQueue.t)
    | `Generational `DepthFirst -> new GenerationalQueue.t (new DepthFirstQueue.t)
    | `Generational `RandomPath -> new GenerationalQueue.t (new RandomPathQueue.t)
    | `LeastCovered -> new LeastCoveredQueue.t
    | `ClosestToUncovered -> new ClosestToUncoveredQueue.t
    | `ClosestToTargets -> new ClosestToTargetsQueue.t targets_ref
    | `Generational `ClosestToTargets -> new GenerationalQueue.t (new ClosestToTargetsQueue.t targets_ref)

let default_fqueue = ref (`Generational `BreadthFirst)
let get_default_fqueue targets_ref = get targets_ref !default_fqueue

(* Backward *)
let get_function_backward_queue targets_ref function_queue queue =
    new FunctionQueue.t (BackwardRank.get function_queue) (fun () -> get targets_ref queue)

let default_bqueue = ref `ClosestToTargets
let get_default_bqueue targets_ref = get_function_backward_queue targets_ref !BackwardRank.default_brank !default_bqueue

(* TODO: enable SimpleOtherfnQueue *)
let options = [
    "--forward-queue",
        Arg.Symbol (fst (List.split queues), fun name -> default_fqueue := List.assoc name queues),
        "<queue name> Set the default forward job queue in bi-directional BackOtter (default: " ^ (fst (List.find (fun (_, x) -> x = !default_fqueue) queues)) ^ ")";
    "--backward-queue",
        Arg.Symbol (fst (List.split queues), fun name -> default_bqueue := List.assoc name queues),
        "<queue name> Set the default backward job queue in bi-directional BackOtter (default: " ^ (fst (List.find (fun (_, x) -> x = !default_bqueue) queues)) ^ ")";
]

