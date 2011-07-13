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


(* job set which give a new id to a job upon entry into the set; if the job previously existed, the old id is replaced;
 * this is to support delayed removal from the internal compact execution tree, while still allowing jobs to be
 * garbage-collected upon removal *)
module JobIndirectSet = struct
    module M = Map.Make (struct type t = int let compare = Pervasives.compare end)

    let empty = (M.empty, M.empty, Counter.make ())

    let remove job (job_to_id, id_to_job, counter) =
        try
            (M.remove job#node_id job_to_id, M.remove (M.find job#node_id job_to_id) id_to_job, counter)
        with Not_found ->
            (job_to_id, id_to_job, counter)

    let add job (job_to_id, id_to_job, counter) =
        let job_to_id, id_to_job, counter = remove job (job_to_id, id_to_job, counter) in
        let new_id = Counter.next counter in
        ((M.add job#node_id new_id job_to_id, M.add new_id job id_to_job, counter), new_id)

    let mem id (_, id_to_job, _) = M.mem id id_to_job
    let find id (_, id_to_job, _) = M.find id id_to_job
end


class ['self] t keep = object (self : 'self)
    (* zipper-based search queue context *)
    val context = `Top
    val leaves = RandomBag.empty
    val jobs = JobIndirectSet.empty

    method put job =
        if keep job then
            let jobs, job_id = JobIndirectSet.add job jobs in
            {< leaves = RandomBag.put (`Job job_id) leaves; jobs = jobs >}
        else 
            let _ = OcamlUtilities.Output.debug_printf "Pruned job %d@\n" job#node_id in
            self

    method remove job =
        {< jobs = JobIndirectSet.remove job jobs >}

    method get = OcamlUtilities.Profiler.global#call "RandomPathQueue.t#get" begin fun () ->
        (* first, zip the tree, making sure no branches are empty unless the tree is empty *)
        let rec zip branches = function
            | `Top -> `Branches branches
            | `Node (siblings, context) when RandomBag.is_empty siblings -> zip branches context
            | `Node (siblings, context) when RandomBag.is_empty branches -> zip siblings context
            | `Node (siblings, context) -> zip (RandomBag.put (`Branches branches) siblings) context
        in

        (* then, descend randomly to a job; if the job is a removed job, repeat the selection *)
        let rec descend jobs context = function
            | `Branches nodes ->
                begin match RandomBag.get nodes with
                    | None -> None
                    | Some (nodes, node) -> descend jobs (`Node (nodes, context)) node
                end
            | `Job job_id when not (JobIndirectSet.mem job_id jobs) ->
                descend jobs `Top (zip RandomBag.empty context)
            | `Job job_id ->
                let job = JobIndirectSet.find job_id jobs in
                Some ({< context = context; leaves = RandomBag.empty; jobs = JobIndirectSet.remove job jobs >}, job)
        in
        descend jobs `Top (zip leaves context)
    end
end
