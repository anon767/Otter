open Control.Data
open Control.Monad
open Control.Graph

open TypeQual.Qual
open TypeQual.QualType


module Context = struct
    type t = Cil.location
    let compare = Cil.compareLoc
    let default = Cil.locUnknown
    let printer ff loc =
        if loc == Cil.locUnknown then
            Format.fprintf ff "*:*"
        else
            Format.fprintf ff "%s:%d" loc.Cil.file loc.Cil.line
end


module type CilQualTypeMonad = sig
    include QualTypeMonad with type QualType.Qual.Const.t = string

    val emptyContext : Context.t

    val askContext : Context.t monad
    val inContext : (Context.t -> Context.t) -> 'a monad -> 'a monad
end


module CilQualTypeT (QualVar : PrintableComparableType) (M : Monad) = struct
    (* setup CilQual constraint graph *)
    module TQV = TypedQualVar (QualVar)
    module QT = Qual (TQV) (String)

    module TQC = TypedConstraint (TQV)
    module QC = Constraint (TQC)
    module ContextQC = ContextualEdge (Context) (QC)

    (* setup CilQual monad stack *)
    module GraphM = BidirectionalGraphT (QT) (ContextQC) (M)
    module ContextGraphM = ContextualEdgeGraphT (ContextQC) (GraphM)
    module QualContextGraphM = struct
        include QualT (QT) (QC) (ContextGraphM)
        let local f m = Fresh (fun s -> ContextGraphM.local f (runFresh m s))
    end
    module QualTypeQualContextGraphM = QualTypeT (TQV) (TQC) (QualContextGraphM)
    include QualTypeQualContextGraphM

    let emptyContext = Context.default

    (* lift monad operations *)
    let askContext = QualTypeQualContextGraphM.lift (QualContextGraphM.lift ContextGraphM.ask)
    let inContext f m = QualTypeQualContextGraphM.lift (QualContextGraphM.local f m)
end

