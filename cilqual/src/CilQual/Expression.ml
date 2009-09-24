open Control.Monad
open TypeQual.QualType
open CilData


module type InterpreterMonad = sig
    include Environment.InterpreterMonad

    val access_lval : Cil.lval -> QualType.t monad
    val access_rval : Cil.lval -> QualType.t monad
    val interpret_exp : Cil.exp -> QualType.t monad
end


(* interpreter for Cil.exp side-effect free expressions *)
module InterpreterT (E : Environment.InterpreterMonad) = struct
    (* setup monad stack *)
    include E
    module Ops = MonadOps (E)
    open Ops

    let rec access_lval (l, o) =
        let rec apply_offset v = function
            | Cil.NoOffset ->
                return v
            | Cil.Field (f, o) -> perform
                v <-- get_field v f;
                apply_offset v o
            | Cil.Index (_, o) -> perform
                apply_offset v o
        in
        perform
            v <-- begin match l with
                | Cil.Var v -> lookup_var v    (* x *)
                | Cil.Mem e -> interpret_exp e (* *x *)
            end;
            apply_offset v o
    and access_rval l = perform
        qt <-- access_lval l;
        deref qt
    and interpret_exp = function
        (* enums may have attributes: this only occurs when Cil.lowerConstants is false *)
        | Cil.Const (Cil.CEnum (e, _, _)) ->
            interpret_exp e

        (* constants have no qualifiers *)
        | Cil.Const _
        | Cil.SizeOf _ | Cil.SizeOfE _ | Cil.SizeOfStr _
        | Cil.AlignOf _ | Cil.AlignOfE _
        (* boolean/comparison operations changes type *)
        | Cil.BinOp (Cil.Lt, _, _, _) | Cil.BinOp (Cil.Gt, _, _, _) | Cil.BinOp (Cil.Le, _, _, _)
        | Cil.BinOp (Cil.Ge, _, _, _) | Cil.BinOp (Cil.Eq, _, _, _) | Cil.BinOp (Cil.Ne, _, _, _)
        (* pointer interval also changes type *)
        | Cil.BinOp (Cil.MinusPP, _, _, _) ->
            empty
        (* !e changes type if e is a pointer *)
        | Cil.UnOp (Cil.LNot, e, _) when Cil.isPointerType (Cil.typeOf e) ->
            empty

        (* unary/pointer operations are pass-through *)
        | Cil.UnOp (_, e, _)
        | Cil.BinOp (Cil.PlusPI, e, _, _)
        | Cil.BinOp (Cil.IndexPI, e, _, _)
        | Cil.BinOp (Cil.MinusPI, e, _, _) ->
            interpret_exp e

        | Cil.CastE (t, e) ->
            if CilType.is_cil_inserted_type t then
                interpret_exp e (* ignore casts inserted by Cil *)
            else
                (* TODO:
                 * - if there are type qualifiers, need to mask the originally qualifiers appropriately;
                 * - don't just replace e, but augment its qualtype with the typecast.
                 *)
                embed_rval t (* safe to ignore e since it's side-effect free *)

        | Cil.BinOp (u, e1, e2, t) -> perform
            (* least-upper-bound of the operands *)
            qt1 <-- interpret_exp e1;
            qt2 <-- interpret_exp e2;
            join qt1 qt2

        | Cil.Lval l ->
            access_rval l

        | Cil.AddrOf l     (* &x *)
        | Cil.StartOf l -> (* &x[0] == x where x is an array *)
            access_lval l
end

