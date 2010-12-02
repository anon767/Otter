type terminal = { text : string }
type nonterminal = { name : string }

type either = Term of terminal | Nonterm of nonterminal
type production = either list

module NontermMap = Map.Make(struct type t = nonterminal let compare = compare end)
module N = NontermMap
type grammar = production list NontermMap.t
