open Control.Monad
open TypeQual.QualType
open CilData


module CilFieldOrVar = struct
    type t = CilVar of CilVar.t
           | CilField of CilField.t

    let compare x y = if x == y then 0 else match x, y with
        | CilVar x, CilVar y -> CilVar.compare x y
        | CilField x, CilField y -> CilField.compare x y
        | x, y ->
            let rank = function CilVar _ -> 0 | CilField _ -> 1 in
            Pervasives.compare (rank x) (rank y)

    let hash = function
        | CilVar x -> 2 * CilVar.hash x
        | CilField x -> 1 + 2 * CilField.hash x

    let equal x y = compare x y = 0

    let printer ff = function
        | CilVar x -> CilVar.printer ff x
        | CilField x -> CilField.printer ff x
end


module type InterpreterMonad = sig
    include Type.InterpreterMonad
    module Env : Map.S with type key = CilFieldOrVar.t
    type env
    val emptyEnv : env

    val lookup_var : CilVar.t -> QualType.t monad
    val update_var : CilVar.t -> QualType.t -> unit monad

    val lookup_field : CilField.t -> QualType.t monad
    val update_field : CilField.t -> QualType.t -> unit monad
end


(* interpreter for Cil.exp side-effect free expressions *)
module InterpreterT (T : Type.InterpreterMonad with type QualType.Var.Embed.t = CilFieldOrVar.t) = struct
    open CilFieldOrVar

    (* setup monad stack *)
    module EnvironmentT = struct
        include EnvT (CilFieldOrVar) (T.QualType) (T)
        let emptyEnv = empty

        module QualGraph = T.QualGraph
        module QualType = T.QualType
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
        let deref qt = lift (T.deref qt)
        let app qtf qta = lift (T.app qtf qta)
        let retval qtf = lift (T.retval qtf)
        let args qtf = lift (T.args qtf)
        let annot_attr qt attrlist = lift (T.annot_attr qt attrlist)
        let embed_lval v t = lift (T.embed_lval v t)
        let embed_rval t = lift (T.embed_rval t)
    end
    include EnvironmentT
    module Ops = MonadOps (EnvironmentT)
    open Ops

    let embed_field_or_var fv =
        let typ, attrlist, loc = match fv with
            | CilVar v -> v.Cil.vtype, v.Cil.vattr, v.Cil.vdecl
            | CilField f -> f.Cil.ftype, f.Cil.fattr, f.Cil.floc
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
    let update_var v qt = update (CilVar v) qt
    let lookup_field f = lookup_field_or_var (CilField f)
    let update_field f qt = update (CilField f) qt
end

