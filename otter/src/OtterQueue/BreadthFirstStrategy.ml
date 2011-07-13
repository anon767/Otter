(** Breadth-first Otter strategy: jobs are weighted by 1/n where n is the number of steps a job has executed. *)

open DataStructures
open OtterCore

module JobSteps = Map.Make
    (struct type t = int let compare = Pervasives.compare end)


class ['self] t = object (_ : 'self)
    val queue = JobSteps.empty
    val steps = 1

    method add job =
        let queue = JobSteps.add job#node_id steps queue in
        {< queue = queue >}

    method remove job =
        let steps = JobSteps.find job#node_id queue in
        let queue = JobSteps.remove job#node_id queue in
        let steps = steps + 1 in
        {< queue = queue; steps = steps >}

    method weights jobs =
        List.map (fun job -> 1. /. float_of_int (JobSteps.find job#node_id queue)) jobs
end

