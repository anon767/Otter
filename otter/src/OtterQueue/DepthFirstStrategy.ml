(** Depth-first Otter strategy: jobs are weighted by the number of steps a job has executed. *)

open DataStructures
open OtterCore

module JobSteps = Map.Make
    (struct type t = Job.job let compare x y = Pervasives.compare x.Job.jid y.Job.jid end)


class ['self] t = object (_ : 'self)
    val queue = JobSteps.empty
    val steps = 1

    method add job =
        let queue = JobSteps.add job steps queue in
        {< queue = queue >}

    method remove job =
        let steps = JobSteps.find job queue in
        let queue = JobSteps.remove job queue in
        let steps = steps + 1 in
        {< queue = queue; steps = steps >}

    method weights jobs =
        List.map begin fun job ->
            let steps = JobSteps.find job queue in
            float_of_int steps
        end jobs
end

