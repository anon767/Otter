(** *)

open DataStructures
open OtterCore

class ['self] t = object (self : 'self)

    method add job = self

    method remove job = self

    method weights jobs =
        List.map (fun job -> match job#external_bounding_paths with
            | Some (_::_) -> 1.
            | _ -> 0.
        ) jobs
end
