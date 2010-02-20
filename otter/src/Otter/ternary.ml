
type t = True | False | Unknown

let ternary_not = function
	| True -> False
	| False -> True
	| Unknown -> Unknown
