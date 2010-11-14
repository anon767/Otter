(** Default Otter job queue based on command-line options. *)

open OcamlUtilities

let queues = [
    "breadth-first", `BreadthFirst;
    "depth-first", `DepthFirst;
    "random-path", `RandomPath;
    "generational*breadth-first", `Generational `BreadthFirst;
    "generational*depth-first", `Generational `DepthFirst;
    "generational*random", `Generational `Random;
    "least-covered", `LeastCovered;
    "closest-to-uncovered", `ClosestToUncovered;
    "closest-to-targets", `ClosestToTargets;
    "generational*closest-to-targets", `Generational `ClosestToTargets;
    "KLEE", `KLEE
]

let default_queue = ref (`Generational `BreadthFirst)

(* to get around value restriction that limits polymorphism in the queues list *)
let rec get = function
    | `BreadthFirst -> new RankedQueue.t [ new BreadthFirstStrategy.t ]
    | `DepthFirst -> new RankedQueue.t [ new DepthFirstStrategy.t ]
    | `RandomPath -> new RandomPathQueue.t
    | `Generational `BreadthFirst -> new RankedQueue.t [ new GenerationalStrategy.t; new BreadthFirstStrategy.t ]
    | `Generational `DepthFirst -> new RankedQueue.t [ new GenerationalStrategy.t; new DepthFirstStrategy.t ]
    | `Generational `Random -> new RankedQueue.t [ new GenerationalStrategy.t ]
    | `LeastCovered -> new RankedQueue.t  [ new LeastCoveredStrategy.t ]
    | `ClosestToUncovered -> new RankedQueue.t [ new ClosestToUncoveredStrategy.t ]
    | `ClosestToTargets -> new RankedQueue.t [ new ClosestToTargetsStrategy.t ]
    | `Generational `ClosestToTargets -> new RankedQueue.t [ new GenerationalStrategy.t; new ClosestToTargetsStrategy.t ]
    | `RoundRobin queues -> new RoundRobinQueue.t (List.map get queues)
    | `KLEE -> get (`RoundRobin [ `ClosestToUncovered; `RandomPath ])

let get_default () = get !default_queue

let options = [
    "--queue",
        Arg.Symbol (fst (List.split queues), fun name -> default_queue := List.assoc name queues),
        "<queue name> Set the default job queue (default: " ^ (fst (List.find (fun (_, x) -> x = !default_queue) queues)) ^ ")";
]

