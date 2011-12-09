(** Default Otter job queue based on command-line options. *)

open OcamlUtilities

type queues = [
    | `BreadthFirst
    | `DepthFirst
    | `RandomPath
    | `Generational of [ `BreadthFirst | `DepthFirst | `Random ]
    | `LeastCovered
    | `ClosestToUncovered
    | `ClosestToTargets
    | `ClosestToTargetsIntraprocedural
    | `ClosestToTargetsPathWeighted
    | `DistanceToUncoveredWeighted
    | `DistanceToTargetsWeighted
    | `PathWeighted
    | `KLEE
    | `SAGE
    | `ESD
    | `RoundRobin of (queues list)
]

let queues : (string * queues) list = [
    "breadth-first", `BreadthFirst;
    "depth-first", `DepthFirst;
    "random-path", `RandomPath;
    "generational*breadth-first", `Generational `BreadthFirst;
    "generational*depth-first", `Generational `DepthFirst;
    "generational*random", `Generational `Random;
    "least-covered", `LeastCovered;
    "closest-to-uncovered", `ClosestToUncovered;
    "closest-to-targets", `ClosestToTargets;
    "closest-to-targets-intraprocedural", `ClosestToTargetsIntraprocedural;
    "closest-to-targets-path-weighted", `ClosestToTargetsPathWeighted;
    "distance-to-uncovered-weighted", `DistanceToUncoveredWeighted;
    "distance-to-targets-weighted", `DistanceToTargetsWeighted;
    "path-weighted", `PathWeighted;
    "KLEE", `KLEE;
    "SAGE", `SAGE;
    "ESD", `ESD;
    "generational*random,breadth-first", `RoundRobin [`Generational `Random ; `BreadthFirst ];
]

let default_queue = ref `RandomPath

(* to get around value restriction that limits polymorphism in the queues list *)
let rec get = function
    | `BreadthFirst -> new RankedQueue.t [ new BreadthFirstStrategy.t ]
    | `DepthFirst -> new RankedQueue.t [ new DepthFirstStrategy.t ]
    | `RandomPath -> new RandomPathQueue.t (fun _ -> true)
    | `Generational `BreadthFirst -> new RankedQueue.t [ new GenerationalStrategy.t; new BreadthFirstStrategy.t ]
    | `Generational `DepthFirst -> new RankedQueue.t [ new GenerationalStrategy.t; new DepthFirstStrategy.t ]
    | `Generational `Random -> new RankedQueue.t [ new GenerationalStrategy.t ]
    | `LeastCovered -> new RankedQueue.t  [ new LeastCoveredStrategy.t ]
    | `ClosestToUncovered -> new RankedQueue.t [ new ClosestToUncoveredStrategy.t ]
    | `ClosestToTargets -> new RankedQueue.t [ new ClosestToTargetsStrategy.t ClosestToTargetsStrategy.inversely_proportional ]
    | `ClosestToTargetsIntraprocedural -> new RankedQueue.t [ new ClosestToTargetsStrategy.t ~interprocedural:false ClosestToTargetsStrategy.inversely_proportional ]
    | `DistanceToUncoveredWeighted -> new RankedQueue.t [ new WeightedRandomStrategy.t (new ClosestToUncoveredStrategy.t) ]
    | `DistanceToTargetsWeighted -> new RankedQueue.t [ new WeightedRandomStrategy.t (new ClosestToTargetsStrategy.t ClosestToTargetsStrategy.quantized) ]
    | `PathWeighted -> new RankedQueue.t [ new WeightedRandomStrategy.t (new PathWeightedStrategy.t) ]
    | `ClosestToTargetsPathWeighted -> new RankedQueue.t [ new ClosestToTargetsStrategy.t ClosestToTargetsStrategy.quantized; new WeightedRandomStrategy.t (new PathWeightedStrategy.t) ]
    | `RoundRobin queues -> new RoundRobinQueue.t (List.map get queues)
    | `KLEE -> new BatchQueue.t (get (`RoundRobin [ `DistanceToUncoveredWeighted; `PathWeighted ]))
    | `SAGE -> new SageQueue.t
    | `ESD -> new RankedQueue.t [ new OtterESD.ProximityGuidedStrategy.t ClosestToTargetsStrategy.quantized ]

let get_default () = get !default_queue

let options = [
    "--queue",
        Arg.Symbol (fst (List.split queues), fun name -> default_queue := List.assoc name queues),
        " Set the default job queue (default: " ^ (fst (List.find (fun (_, x) -> x = !default_queue) queues)) ^ ")";
] @ ClosestToUncoveredStrategy.options

