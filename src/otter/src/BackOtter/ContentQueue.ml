open OcamlUtilities
open OtterCore
open Job


module JobSet = struct
    module M = Map.Make (struct type t = int let compare = Pervasives.compare end)
    let empty = M.empty
    let is_empty = M.is_empty
    let mem job jobs = M.mem job#node_id jobs
    let remove job jobs = M.remove job#node_id jobs
    let add job jobs = M.add job#node_id job jobs
    let fold f jobs acc = M.fold (fun _ job acc -> f job acc) jobs acc
end

(* This queue adds two methods on top of Otter's queues: fold and length.
 * ContentQueue is necessary: when BackOtter finds a new target, it goes through
 * all existing jobs to see if any of them are calling the target and are ready to
 * examine.
 *)
class ['self] t queue = object (_ : 'self)
    val queue = queue
    val contents = JobSet.empty
    val length = 0

    method put job =
        Profiler.global#call "ContentQueue.t#put" begin fun () ->
            assert(not (JobSet.mem job contents));
            {< queue = queue#put job; contents = JobSet.add job contents; length = length + 1; >}
        end

    method remove job =
        Profiler.global#call "ContentQueue.t#remove" begin fun () ->
            assert(JobSet.mem job contents);
            {< queue = queue#remove job; contents = JobSet.remove job contents; length = length - 1; >}
        end

    method get =
        Profiler.global#call "ContentQueue.t#get" begin fun () ->
            match queue#get with
                | Some (queue, job) ->
                    assert(JobSet.mem job contents);
                    Some ({< queue = queue; contents = JobSet.remove job contents; length = length - 1; >}, job)
                | None ->
                    assert(JobSet.is_empty contents);
                    None
        end

    method fold f acc =
        Profiler.global#call "ContentQueue.t#fold" begin fun () ->
            JobSet.fold f contents acc
        end

    method length = length

end

