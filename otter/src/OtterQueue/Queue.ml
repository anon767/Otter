(** Default Otter job queue based on command-line options. *)

open OcamlUtilities

let queues = [
    "best-first", `BestFirst;
    "breadth-first", `BreadthFirst;
    "depth-first", `DepthFirst;
    "random-path", `RandomPath;
    "generational:breadth-first", `Generational `BreadthFirst;
    "generational:depth-first", `Generational `DepthFirst;
    "generational:random-path", `Generational `RandomPath;
]

let default_queue = ref (`Generational `BreadthFirst)

(* need get_default to get around value restriction that limits polymorphism in the queues list *)
let get_default () = match !default_queue with
    | `BestFirst -> BestFirstQueue.make_default () 
    | `BreadthFirst -> BreadthFirstQueue.make ()
    | `DepthFirst -> DepthFirstQueue.make ()
    | `RandomPath -> RandomPathQueue.make ()
    | `Generational `BreadthFirst -> GenerationalQueue.make (BreadthFirstQueue.make ())
    | `Generational `DepthFirst -> GenerationalQueue.make (DepthFirstQueue.make ())
    | `Generational `RandomPath -> GenerationalQueue.make (RandomPathQueue.make ())

let options = [
    "--driver",
        Arg.Symbol (fst (List.split queues), fun name -> default_queue := List.assoc name queues),
        "<queue name> Set the default job queue (default: generational:breadth-first)";
]

