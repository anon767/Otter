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
    type qual = Var of Var.t
              | Const of Const.t
    include PrintableComparableType with type t = qual
    val fresh : int -> t
end


module type Constraint = EdgeLabel


module type QualGraphType = sig
    module Qual : Qual
    module Constraint : Constraint
    include PrintableGraphType with type vertex = Qual.t
end


module type QualGraphAutomataType = sig
    include QualGraphType
    module Automata : sig
        include OrderedType
        val start : t
        val accept : t -> bool
    end
    val fold_forward : (vertex -> Automata.t -> 'a -> 'a) -> t -> vertex -> Automata.t -> 'a -> 'a
    val fold_backward : (vertex -> Automata.t -> 'a -> 'a) -> t -> vertex -> Automata.t -> 'a -> 'a
end

