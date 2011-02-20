(** Closest-to-targets first strategy: jobs are weighted by 1/n where n is the shortest distance to a call to __FAILURE() or a target function. *)

open CilUtilities
open DataStructures
open OtterCFG
open OtterCore


class ['self] t = object (self : 'self)

    method add job = self

    method remove job = self

    method weight job =
        let target_fundecs = BackOtterTargets.get_target_fundecs () in
        (* target_fundecs includes failure_fn *)
        let distance = BackOtterUtilities.get_distance_to_targets target_fundecs job in
        1. /. float_of_int (distance)
end



