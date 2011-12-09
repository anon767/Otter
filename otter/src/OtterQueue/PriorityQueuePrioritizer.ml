open DataStructures
open OcamlUtilities

(** Jobs are grouped by their priorities in the PrioritySearchQueue. Priorities are used as keys. An auxiliary mapping from job#node_id's to priorities (i.e., keys in PrioritySearchQueue) is used by delete to locate jobs.  *)
module JobQueue = struct

    module JobSet = OtterJob.JobSet
    module JobMap = Map.Make (Module.Int)  (* Map job#node_id to its weight *)
    module Weight = struct type t = float let compare x y = - Pervasives.compare x y end  (* Max-first *)
    module PSQueue = PrioritySearchQueue.Make (Weight) (Weight)

    let empty = (PSQueue.empty, JobMap.empty)

    let is_empty (pqueue, pri) = PSQueue.is_empty pqueue

    let insert w job (pqueue, pri) =
        let pqueue = PSQueue.insert ~combine:(fun (p1,s1) (p2,s2) -> assert(p1=p2); p1, JobSet.union s1 s2) w w (JobSet.singleton job) pqueue in
        let pri = JobMap.add job#node_id w pri in
        (pqueue, pri)

    let delete job (pqueue, pri) =
        let w = JobMap.find job#node_id pri in
        let _, wjobs = PSQueue.lookup w pqueue in
        let pqueue = PSQueue.delete w pqueue in
        let wjobs = JobSet.remove job wjobs in
        let pqueue = if JobSet.is_empty wjobs then pqueue else PSQueue.insert w w wjobs pqueue in
        (pqueue, pri)

    let find_max (pqueue, _) =
        let _, w, wjobs = PSQueue.find_min pqueue in
        List.map (fun job -> job, w) (JobSet.elements wjobs)

    let fold f (pqueue, _) x =
        let rec fold pqueue x =
            if PSQueue.is_empty pqueue then x
            else
                let _, _, wjobs = PSQueue.find_min pqueue in
                let pqueue = PSQueue.delete_min pqueue in
                let x = JobSet.fold f wjobs x in
                fold pqueue x
        in
        fold pqueue x

end

class ['self] t ?(need_update=(fun () -> false)) score = object (self : 'self)

    val mutable queue = JobQueue.empty

    method add job = Profiler.global#call "PriorityQueuePrioritizer.add" begin fun () ->
        let w = score job in
        {< queue = JobQueue.insert w job queue >}
    end

    method remove job = {< queue = JobQueue.delete job queue >}

    method prioritize subset = Profiler.global#call "PriorityQueuePrioritizer.get" begin fun () ->
        if need_update () then begin
            queue <- JobQueue.fold (fun job queue -> let w = score job in JobQueue.insert w job queue) queue JobQueue.empty
        end;
        let jobs = if JobQueue.is_empty queue then [] else JobQueue.find_max queue in
        match subset with
        | None -> jobs
        | Some jobs' -> (* TODO: this is inefficient *)
            let jobs =
                let jobs' = List.map (fun (job,_) -> job) jobs' in
                List.filter (fun (job,_) -> List.memq job jobs') jobs
            in
            if jobs = [] then jobs'  (* Give up *)
            else jobs
    end

end

