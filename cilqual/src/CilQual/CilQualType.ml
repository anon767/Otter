open Control.Data
open Control.Monad
open Control.Graph

open TypeQual.Qual
open TypeQual.QualType


module type CilQualContext = sig
    include PrintableOrderedType with type t = Cil.location
    val default : t
    val file : Cil.file -> t
end


module Context = struct
    type t = Cil.location
    let compare = Cil.compareLoc
    let default = Cil.locUnknown
    let file f = { Cil.file = f.Cil.fileName; Cil.line = -1; Cil.byte = -1 }
    let printer ff loc =
        if loc == Cil.locUnknown then
            Format.fprintf ff ""
        else if loc.Cil.line <= 0 then
            Format.fprintf ff "%s:" loc.Cil.file
        else
            Format.fprintf ff "%s:%d:" loc.Cil.file loc.Cil.line
end


module type CilQualTypeMonad = sig
    include QualTypeMonad with type QualType.Qual.Const.t = string

    val emptyContext : Context.t
    val fileContext : Cil.file -> Context.t

    val askContext : Context.t monad
    val inContext : (Context.t -> Context.t) -> 'a monad -> 'a monad
end


module CilQualTypeT (QualVar : PrintableComparableType) (Context : CilQualContext) (M : Monad) = struct
    (* setup CilQual constraint graph *)
    module TQV = struct
        include TypedQualVar (QualVar)
        let printer =
            let super = printer in
            let rec printer ff = function
                | Deref (Embed x) -> Format.fprintf ff "%a" Embed.printer x
                | Deref v -> Format.fprintf ff "*%a" printer v
                | Embed x -> Format.fprintf ff "&%a" Embed.printer x
                | x -> super ff x
            in
            printer
    end
    module QT = struct
        include Qual (TQV) (String)
        let printer ff = function
            | Const c -> Format.fprintf ff "$%a" Const.printer c
            | Var v -> Var.printer ff v
    end

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
    let fileContext = Context.file

    (* lift monad operations *)
    let askContext = QualTypeQualContextGraphM.lift (QualContextGraphM.lift ContextGraphM.ask)
    let inContext f m = QualTypeQualContextGraphM.lift (QualContextGraphM.local f m)
end

