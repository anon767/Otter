(** Deferred computation of a value over a state. *)

type ('state, 'value) t =
    | Immediate of 'value
    | Deferred of ('state -> ('state * 'value))

(** Return the value of a deferred computation, computing the value in the given state if necessary. *)
let force state = function
    | Immediate x -> (state, x)
    | Deferred f -> f state

