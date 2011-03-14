(** Depth-first Otter strategy: jobs are weighted by the number of steps a job has executed. *)

open DataStructures
open OtterCore

module JobSteps = Map.Make
    (struct type t = int let compare = Pervasives.compare end)


class ['self] t = object (_ : 'self)
    val queue = JobSteps.empty
    val steps = 1

    method add job =
        let queue = JobSteps.add job#path_id steps queue in
        {< queue = queue >}

    method remove job =
        let steps = JobSteps.find job#path_id queue in
        let queue = JobSteps.remove job#path_id queue in
        let steps = steps + 1 in
        {< queue = queue; steps = steps >}

    method find_max_jobs =
        RankedQueue.find_max_jobs begin fun job ->
            float_of_int (JobSteps.find job#path_id queue)
        end
end

