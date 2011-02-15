open OtterCore
open Job


module JobSet = struct
    module M = Map.Make (struct type t = int let compare = Pervasives.compare end)
    let empty = M.empty
    let is_empty = M.is_empty
    let mem job jobs = M.mem job#jid_unique jobs
    let remove job jobs = M.remove job#jid_unique jobs
    let add job jobs = M.add job#jid_unique job jobs
    let elements jobs = M.fold (fun _ job jobs -> job::jobs) jobs []
end

(* This queue adds two methods on top of Otter's queues: get_contents and length.
 * ContentQueue is necessary: when BackOtter finds a new target, it goes through
 * all existing jobs to see if any of them are calling the target and are ready to 
 * examine.
 *)
class ['self] t queue = object (_ : 'self)
    val queue = queue
    val contents = JobSet.empty
    val length = 0

    method put job =
        assert(not (JobSet.mem job contents));
        {< queue = queue#put job; contents = JobSet.add job contents; length = length + 1; >}

    method get =
        match queue#get with
        | Some (queue, job) ->
            assert(JobSet.mem job contents);
            Some ({< queue = queue; contents = JobSet.remove job contents; length = length - 1; >}, job)
        | None -> assert(JobSet.is_empty contents); None

    method get_contents = JobSet.elements contents

    method length = length

end

