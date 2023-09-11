(** Weighted random strategy: jobs are first weighted by a sub-strategy, then a single job is selected by weighted
    random selection and re-weighted as 1.0, and the remaining jobs re-weighted as 0.0. *)


open CilUtilities
open DataStructures
open OtterCFG
open OtterCore


class ['self] t substrategy = object (self : 'self)

    method add job = self

    method remove job = self

    method weights jobs =
        let weights = substrategy#weights jobs in
        let rev_weights, norm = List.fold_left begin fun (rev_weights, norm) weight ->
            (weight::rev_weights, norm +. weight)
        end ([], 0.) weights in

        let choice = Random.float norm in
        let rec select new_weights sum = function
            | x::[] ->
                1.::new_weights
            | x::xs when sum +. x < choice ->
                select (0.::new_weights) (sum +. x) xs
            | x::xs ->
                List.fold_left (fun new_weights _ -> 0.::new_weights) (1.::new_weights) xs
            | [] ->
                invalid_arg "no jobs"
        in
        select [] 0. rev_weights

end

