(** Default Otter job queue based on command-line options. *)

open OcamlUtilities

let queues = [
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
    | `BreadthFirst -> new BreadthFirstQueue.t
    | `DepthFirst -> new DepthFirstQueue.t
    | `RandomPath -> new RandomPathQueue.t
    | `Generational `BreadthFirst -> new GenerationalQueue.t (new BreadthFirstQueue.t)
    | `Generational `DepthFirst -> new GenerationalQueue.t (new DepthFirstQueue.t)
    | `Generational `RandomPath -> new GenerationalQueue.t (new RandomPathQueue.t)

let options = [
    "--driver",
        Arg.Symbol (fst (List.split queues), fun name -> default_queue := List.assoc name queues),
        "<queue name> Set the default job queue (default: " ^ (fst (List.find (fun (_, x) -> x = !default_queue) queues)) ^ ")";
]

