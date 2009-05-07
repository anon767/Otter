open Control.Data
open Control.Graph


module type QualVar = sig
    include PrintableComparableType
    val fresh : int -> t
end


module type QualConst = PrintableComparableType


module type Qual = sig
    module Var : QualVar
    module Const : QualConst
    type t = Var of Var.t
           | Const of Const.t
    val fresh : int -> t
    val compare : t -> t -> int
    val hash : t -> int
    val equal : t -> t -> bool
    val printer : Format.formatter -> t -> unit
end


module type Constraint = EdgeLabel


module type QualGraphType = sig
    module Qual : Qual
    module Constraint : Constraint
    include PrintableGraphType with type vertex = Qual.t
end


module type QualGraphAutomatonType = sig
    include QualGraphType
    module Automaton : sig
        include PrintableOrderedType
        val start : t
        val accept : t -> bool
    end
    val fold_forward : (vertex -> Automaton.t -> 'a -> 'a) -> t -> vertex -> Automaton.t -> 'a -> 'a
    val fold_backward : (vertex -> Automaton.t -> 'a -> 'a) -> t -> vertex -> Automaton.t -> 'a -> 'a
end

