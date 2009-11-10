open Control.Monad
open TypeQual.QualType
open CilData


module CilFieldOrVarOrCast = struct
    type t = CilVar of CilVar.t
           | CilField of CilField.t
           | CilCast of CilExp.t

    let compare x y = if x == y then 0 else match x, y with
        | CilVar x, CilVar y -> CilVar.compare x y
        | CilField x, CilField y -> CilField.compare x y
        | CilCast x, CilCast y -> CilExp.compare x y
        | x, y ->
            let rank = function CilVar _ -> 0 | CilField _ -> 1 | CilCast _ -> 2 in
            Pervasives.compare (rank x) (rank y)

    let hash = function
        | CilVar x -> 2 * CilVar.hash x
        | CilField x -> 1 + 2 * CilField.hash x
        | CilCast x -> 2 + 2 * CilExp.hash x

    let equal x y = compare x y = 0

    let printer ff = function
        | CilVar x -> CilVar.printer ff x
        | CilField x -> CilField.printer ff x
        | CilCast x -> CilExp.printer ff x
end


module type InterpreterMonad = sig
    include Type.InterpreterMonad
    module Env : Map.S with type key = CilFieldOrVarOrCast.t
    type env
    val emptyEnv : env

    val embed_cast : CilExp.t -> QualType.t monad
    val lookup_var : CilVar.t -> QualType.t monad
    val get_field : QualType.t -> CilField.t -> QualType.t monad
end


(* interpreter for Cil.exp side-effect free expressions *)
module InterpreterT (T : Type.InterpreterMonad with type QualType.Var.Embed.t = CilFieldOrVarOrCast.t) = struct
    open CilFieldOrVarOrCast

    (* setup monad stack *)
    module EnvironmentT = struct
        include EnvT (CilFieldOrVarOrCast) (T.QualType) (T)
        let emptyEnv = empty

        module UnionTable = T.UnionTable
        module QualGraph = T.QualGraph
        module QualType = T.QualType
        let emptyUnionTable = T.emptyUnionTable
        let emptyContext = T.emptyContext
        let fileContext = T.fileContext

        (* lift monad operations *)
        let inContext f m = Env (fun s -> T.inContext f (runEnv m s))
        let askContext = lift T.askContext
        let assign x y = lift (T.assign x y)
        let join x y = lift (T.join x y)
        let meet x y = lift (T.meet x y)
        let embed x qt = lift (T.embed x qt)
        let fresh qt = lift (T.fresh qt)
        let empty = lift T.empty
        let annot qt clist = lift (T.annot qt clist)
        let has_annot qt = lift (T.has_annot qt)
        let deref qt = lift (T.deref qt)
        let app qtf qta = lift (T.app qtf qta)
        let retval qtf = lift (T.retval qtf)
        let args qtf = lift (T.args qtf)
        let annot_attr qt attrlist = lift (T.annot_attr qt attrlist)
        let embed_lval v t = lift (T.embed_lval v t)
        let embed_rval e t = lift (T.embed_rval e t)
    end
    include EnvironmentT
    module Ops = MonadOps (EnvironmentT)
    open Ops

    let embed_cast = function
        | (Cil.CastE (t, _) as c, loc) -> perform
            embed_rval (CilCast (c, loc)) t
        | _ ->
            failwith "embed_cast of an expression that is not a cast!"

    let embed_field_or_var fv =
        let typ, attrlist, loc = match fv with
            | CilVar v -> v.Cil.vtype, v.Cil.vattr, v.Cil.vdecl
            | CilField f -> f.Cil.ftype, f.Cil.fattr, f.Cil.floc
            | CilCast f -> failwith "Impossible!"
        in
        inContext (fun _ -> loc) begin perform
            qt <-- embed_lval fv typ;
            annot_attr qt attrlist
        end

    let lookup_field_or_var v = perform
        qtopt <-- lookup v;
        match qtopt with
            | Some qt -> return qt;
            | None -> perform
                qt <-- embed_field_or_var v;
                update v qt;
                return qt

    let lookup_var v = lookup_field_or_var (CilVar v)

    let get_field qt f =
        if f.Cil.fcomp.Cil.cstruct then
            lookup_field_or_var (CilField f) (* field-based: just substitute the field *)
        else
            return qt (* don't lookup union fields *)
end

