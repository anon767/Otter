(** Path weighted strategy: jobs are weighted by 2^-n, where n is the path length, normalized over the list of requested jobs. *)

open CilUtilities
open DataStructures
open OtterCFG
open OtterCore


class ['self] t = object (self : 'self)

    method add job = self

    method remove job = self

    method weights jobs =
        let min_length = List.fold_left (fun min_length job -> min min_length job#path_length) max_int jobs in
        List.map (fun job -> 2. ** float (min_length - job#path_length)) jobs

end

