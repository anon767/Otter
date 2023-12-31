open Control.Data
open Control.Monad
open Control.Graph

open TypeQual.Qual
open TypeQual.UnionQualType


module type CilQualContext = sig
    include PrintableOrderedType with type t = Cil.location
    val default : t
    val file : Cil.file -> t
end


module Context = CilData.CilLocation


module type CilUnionQualTypeMonad = sig
    include UnionQualTypeMonad with type QualType.Qual.Const.t = string

    val emptyUnionTable : UnionTable.t
    val emptyContext : Context.t
    val fileContext : Cil.file -> Context.t

    val askContext : Context.t monad
    val inContext : (Context.t -> Context.t) -> 'a monad -> 'a monad
end


module CilUnionQualTypeT (QualVar : PrintableComparableType) (Context : CilQualContext) (M : Monad) = struct
    (* setup CilQual constraint graph *)
    module TQV = struct
        include TypedQualVar (QualVar)
        let printer =
            let super = printer in
            let rec printer ff = function
                | Deref (Deref _ as v) -> Format.fprintf ff "*%a" printer v
                | Deref (FnArg (i, Embed x)) -> Format.fprintf ff "arg%d(%a)" i Embed.printer x
                | FnArg (i, Embed x) -> Format.fprintf ff "&arg%d(%a)" i Embed.printer x
                | Deref (Embed x) -> Format.fprintf ff "%a" Embed.printer x
                | Embed x -> Format.fprintf ff "&%a" Embed.printer x
                | FnRet (Embed x) -> Format.fprintf ff "return(%a)" Embed.printer x
                | Deref v -> Format.fprintf ff "<%a>" super v
                | v -> Format.fprintf ff "&<%a>" super v
            in
            printer
    end
    module QT = struct
        include Qual (TQV) (String)
        let printer ff = function
            | Const c -> Format.fprintf ff "$%a" Const.printer c
            | Var v -> Var.printer ff v
    end

    module QC = Constraint (struct include Unit let default = () let printer ff () = () end)
    module ContextQC = ContextualEdge (Context) (QC)

    (* setup CilQual monad stack *)
    module GraphM = BidirectionalGraphT (QT) (ContextQC) (M)
    module ContextGraphM = ContextualEdgeGraphT (ContextQC) (GraphM)
    module QualContextGraphM = struct
        include QualT (QT) (QC) (ContextGraphM)
        let inContext f m = Fresh (fun s -> ContextGraphM.local f (runFresh m s))
        let askContext = lift ContextGraphM.ask
    end
    module UnionQualTypeQualContextGraphM = struct
        include UnionQualTypeT (TQV) (QualContextGraphM)
        let inContext f m = State (fun s -> QualContextGraphM.inContext f (runState m s))
        let askContext = lift QualContextGraphM.askContext
    end
    include UnionQualTypeQualContextGraphM

    let emptyUnionTable = UnionTable.empty
    let emptyContext = Context.default
    let fileContext = Context.file
end

