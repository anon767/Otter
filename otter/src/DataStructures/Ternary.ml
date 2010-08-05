
type t = True | False | Unknown

let not = function
	| True -> False
	| False -> True
	| Unknown -> Unknown
