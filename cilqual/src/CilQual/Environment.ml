open Control.Monad
open TypeQual.QualType
open CilData


module CilFieldOrVar = struct
    type t = [`Var of CilVar.t | `Field of CilField.t]
    let compare x y = match x, y with
        | `Var x, `Var y -> CilVar.compare x y
        | `Field x, `Field y -> CilField.compare x y
        | x, y ->
            let rank = function `Var _ -> 0 | `Field _ -> 1 in
            Pervasives.compare (rank x) (rank y)
    let printer ff = function
        | `Var x -> CilVar.printer ff x
        | `Field x -> CilField.printer ff x
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
module InterpreterT (T : Type.InterpreterMonad) = struct
    (* setup monad stack *)
    module EnvironmentT = EnvT (CilFieldOrVar) (QualType) (T)
    include EnvironmentT
    module Ops = MonadOps (EnvironmentT)
    open Ops

    let emptyEnv = empty

    module Constraints = T.Constraints

    (* lift monad operations *)
    let assign x y = lift (T.assign x y)
    let join x y = lift (T.join x y)
    let meet x y = lift (T.meet x y)
    let create qt = lift (T.create qt)
    let empty = lift T.empty
    let annot qt clist = lift (T.annot qt clist)
    let deref qt = lift (T.deref qt)
    let app qtf qta = lift (T.app qtf qta)
    let retval qtf = lift (T.retval qtf)
    let args qtf = lift (T.args qtf)
    let parse_annot attrlist = lift (T.parse_annot attrlist)
    let annot_attr qt attrlist = lift (T.annot_attr qt attrlist)
    let embed_lval t = lift (T.embed_lval t)
    let embed_rval t = lift (T.embed_rval t)

    let embed_field_or_var fv =
        let typ, attrlist = match fv with
            | `Var v -> v.Cil.vtype, v.Cil.vattr
            | `Field f -> f.Cil.ftype, f.Cil.fattr
        in perform
        qt <-- embed_lval typ;
        annot_attr qt attrlist

    let lookup_field_or_var v = perform
        qtopt <-- lookup v;
        match qtopt with
            | Some qt -> return qt;
            | None -> perform
                qt <-- embed_field_or_var v;
                update v qt;
                return qt

    let lookup_var v = lookup_field_or_var (`Var v)
    let update_var v qt = update (`Var v) qt
    let lookup_field f = lookup_field_or_var (`Field f)
    let update_field f qt = update (`Field f) qt
end

