(** Path weighted strategy: jobs are weighted by 1/2^n, where n is the path length, normalized over the list of requested jobs. *)

open CilUtilities
open DataStructures
open OtterCFG
open OtterCore


class ['self] t = object (self : 'self)

    method add job = self

    method remove job = self

    method weights jobs =
        let max_length = List.fold_left (fun max_length job -> max max_length job#path_length) 0 jobs in
        List.map (fun job -> 2. ** float (job#path_length - max_length)) jobs

end

