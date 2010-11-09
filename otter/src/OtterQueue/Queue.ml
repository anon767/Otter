(** Default Otter job queue based on command-line options. *)

open OcamlUtilities

let queues = [
    "breadth-first", `BreadthFirst;
    "depth-first", `DepthFirst;
    "random-path", `RandomPath;
    "generational*breadth-first", `Generational `BreadthFirst;
    "generational*depth-first", `Generational `DepthFirst;
    "generational*random-path", `Generational `RandomPath;
    "least-covered", `LeastCovered;
    "closest-to-uncovered", `ClosestToUncovered;
    "closest-to-targets", `ClosestToTargets;
    "generational*closest-to-targets", `Generational `ClosestToTargets;
]

let default_queue = ref (`Generational `BreadthFirst)

(* to get around value restriction that limits polymorphism in the queues list *)
let get = function
    | `BreadthFirst -> new BreadthFirstQueue.t
    | `DepthFirst -> new DepthFirstQueue.t
    | `RandomPath -> new RandomPathQueue.t
    | `Generational `BreadthFirst -> new GenerationalQueue.t (new BreadthFirstQueue.t)
    | `Generational `DepthFirst -> new GenerationalQueue.t (new DepthFirstQueue.t)
    | `Generational `RandomPath -> new GenerationalQueue.t (new RandomPathQueue.t)
    | `LeastCovered -> new LeastCoveredQueue.t
    | `ClosestToUncovered -> new ClosestToUncoveredQueue.t
    | `ClosestToTargets -> new ClosestToTargetsQueue.t
    | `Generational `ClosestToTargets -> new GenerationalQueue.t (new ClosestToTargetsQueue.t)

let get_default () = get !default_queue

let options = [
    "--queue",
        Arg.Symbol (fst (List.split queues), fun name -> default_queue := List.assoc name queues),
        "<queue name> Set the default job queue (default: " ^ (fst (List.find (fun (_, x) -> x = !default_queue) queues)) ^ ")";
]

