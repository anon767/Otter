
type ('state, 'value) t =
    | Immediate of 'value
    | Deferred of ('state -> ('state * 'value))

let force state = function
	| Immediate x -> (state, x)
	| Deferred f -> f state

