(** Composing two or more prioritizers, by calling prioritize one by one.
*)

open DataStructures

(* TODO: use PolymorphicSet *)
module JobSet = struct
    module M = Map.Make (struct type t = int let compare = Pervasives.compare end)
    let empty = M.empty
    let is_empty = M.is_empty
    let remove job jobs = M.remove job#path_id jobs
    let add job jobs = M.add job#path_id job jobs
    let elements jobs = M.fold (fun _ job jobs -> job::jobs) jobs []
end


class ['self] t prioritizers = object (self : 'self)
    val jobs = JobSet.empty
    val prioritizers = prioritizers

    method add job =
        let jobs = JobSet.add job jobs in
        let prioritizers = List.map (fun prioritizer -> prioritizer#add job) prioritizers in
        {< jobs = jobs; prioritizers = prioritizers >}

    method remove job =
        let jobs = JobSet.remove job jobs in
        let prioritizers = List.map (fun prioritizer -> prioritizer#remove job) prioritizers in
        {< jobs = jobs; prioritizers = prioritizers >}

    method prioritize subset_opt = OcamlUtilities.Profiler.global#call "CompositePrioritizer.t#prioritize" begin fun () ->
        List.fold_left (fun subset_opt prioritizer -> Some(prioritizer#prioritize)) subset_opt prioritizers
    end
end

