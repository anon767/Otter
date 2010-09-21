
type ('state, 'value) t =
    | Immediate of 'value
    | Deferred of ('state -> ('state * 'value))

let force state = function
	| Immediate x -> (state, x)
	| Deferred f -> f state

let force_with_update state update = function
    | Immediate x ->
        (state, x)
    | Deferred f ->
        let state, x = f state in
        (update state x, x)
