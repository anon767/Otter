(** Random path Otter job queue, based on KLEE.

    Jobs are organized in a compact execution tree (with no empty or singleton branches), and selected by randomly
    choosing which branch to descend at each branching node. This scheme ensures that:
        - for execution trees before any jobs are completed, branches are choosen fairly;
        - the above implies that for balanced execution trees, paths are choosen fairly.

    When all jobs below a certain branch have been completed, the branch is removed from the execution tree. Because
    remaining branches are choosen randomly, this scheme is biased towards the side of the execution tree where more
    jobs have completed.

    Two possible alternatives are:
        - selecting active jobs at random, i.e., the frontier of the execution tree: this scheme is biased towards to
          the side of the execution tree with more jobs, i.e., the frontier deeper in the execution tree;
        - choosing paths fairly, e.g., by randomly selecting jobs with weights equal to 1/log(|path condition|): the
          main issue is floating point precision, since 1/log(256) is the smallest float; an implementation would
          likely need to work in log-space.

    KLEE: Unassisted and Automatic Generation of High-Coverage Tests for Complex Systems Programs. Cristian Cadar,
    Daniel Dunbar, Dawson Engler USENIX Symposium on Operating Systems Design and Implementation (OSDI 2008).

    @see <https://www.usenix.org/events/osdi08/tech/full_papers/cadar/cadar_html/>
    https://www.usenix.org/events/osdi08/tech/full_papers/cadar/cadar_html/
*)

open DataStructures

module JobSet = struct
    module M = Map.Make (struct type t = int let compare = Pervasives.compare end)
    let empty = M.empty
    let add job set =
        let jobs = try M.find job#path_id set with Not_found -> [] in
        M.add job#path_id (job::jobs) set
    let mem job set =
        try
            let jobs = M.find job#path_id set in
            List.memq job jobs
        with Not_found ->
            false
    let remove job set =
        try
            let jobs = M.find job#path_id set in
            let jobs = List.filter ((!=) job) jobs in
            if jobs = [] then
                M.remove job#path_id set
            else
                M.add job#path_id jobs set
        with Not_found ->
            set
end


class ['self] t = object (_ : 'self)
    (* zipper-based search queue context *)
    val context = `Top
    val leaves = RandomBag.empty
    val removed = JobSet.empty

    method put job =
        {< leaves = RandomBag.put (`Job job) leaves >}

    method remove job =
        {< removed = JobSet.add job removed >}

    method get = OcamlUtilities.Profiler.global#call "RandomPathQueue.t#get" begin fun () ->
        (* first, zip the tree, making sure no branches are empty unless the tree is empty *)
        let rec zip branches = function
            | `Top -> `Branches branches
            | `Node (siblings, context) when RandomBag.is_empty siblings -> zip branches context
            | `Node (siblings, context) when RandomBag.is_empty branches -> zip siblings context
            | `Node (siblings, context) -> zip (RandomBag.put (`Branches branches) siblings) context
        in

        (* then, descend randomly to a job; if the job is a removed job, repeat the selection *)
        let rec descend removed context = function
            | `Branches nodes ->
                begin match RandomBag.get nodes with
                    | None -> None
                    | Some (nodes, node) -> descend removed (`Node (nodes, context)) node
                end
            | `Job job when JobSet.mem job removed ->
                descend (JobSet.remove job removed) `Top (zip RandomBag.empty context)
            | `Job job ->
                Some ({< context = context; leaves = RandomBag.empty; removed = removed >}, job)
        in
        descend removed `Top (zip leaves context)
    end
end
