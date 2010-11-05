open OtterCore


let default_max_nodes = ref 0
let default_max_paths = ref 0
let default_max_abandoned = ref 0


class ['self] t
            ?(max_nodes=max 0 !default_max_nodes)
            ?(max_paths=max 0 !default_max_paths)
            ?(max_abandoned=max 0 !default_max_abandoned)
        ()
        = object (_ : 'self)
    val nodes = 0
    val paths = 0
    val abandoned = 0
    val completed = []

    method report = function
        | Job.Complete completion ->
            {<
                nodes = nodes + 1;
                paths = paths + (match completion with Job.Truncated _ -> 0 | _ -> 1);
                abandoned = abandoned + (match completion with Job.Abandoned _ -> 1 | _ -> 0);
                completed = completion::completed;
            >}

        | Job.Active _
        | Job.Fork _ ->
            {< nodes = nodes + 1 >}

        | Job.Paused _ ->
            invalid_arg "unexpected Job.Paused"

    method should_continue =
        (max_nodes = 0 || nodes < max_nodes)
            && (max_paths = 0 || paths < max_paths)
            && (max_abandoned = 0 || abandoned < max_abandoned)

    method completed = completed

    method get_stats = (nodes, paths, abandoned)

end


(** {1 Command-line options} *)

let options = [
    "--max-nodes",
        Arg.Set_int default_max_nodes,
        "<bound> Bound the number of nodes in the execution tree to explore (default: unbounded)";
    "--max-paths",
        Arg.Set_int default_max_paths,
        "<bound> Bound the number of paths to execute to completion (default: unbounded)";
    "--max-abandoned",
        Arg.Set_int default_max_abandoned,
        "<bound> Bound the number of abandoned paths to return (default: unbounded)";
]

