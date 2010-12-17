module Make (Ord: Set.OrderedType) = struct
    let time_elapsed =
        let time_start = Unix.gettimeofday () in
        function () -> Unix.gettimeofday () -. time_start

    module TimedOrd = struct
        type t = Ord.t * float
        let compare (e1, _) (e2, _) = Ord.compare e1 e2
    end
    module S = Set.Make (TimedOrd)
    include S

    let add elt s = S.add (elt, time_elapsed ()) s
    let iter f s = S.iter (fun (elt, _) -> f elt) s
end
