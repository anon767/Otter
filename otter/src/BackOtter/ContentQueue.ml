open OtterCore
open Job


module JobSet = Set.Make (JobOrderedType)

(* This queue adds two methods on top of Otter's queues: get_contents and length *)
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

