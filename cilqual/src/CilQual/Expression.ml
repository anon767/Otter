open Control.Monad
open TypeQual.QualType


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

    (* Cil setup *)
    let cil_inserted_attribute_string = "cilqual_cil_inserted"
    let is_cil_inserted_type typ = Cil.hasAttribute cil_inserted_attribute_string (Cil.typeAttrs typ)

    let init_cil () =
        (* don't annotate inserted variables *)
        Cabs2cil.typeForInsertedVar := Cil.visitCilType begin object
            inherit Cil.nopCilVisitor
            method vattr attr = Cil.ChangeTo (Cil.dropAttribute Config.attribute_string [ attr ])
        end end;
        (* mark inserted casts *)
        Cabs2cil.typeForInsertedCast := Cil.typeAddAttributes [ Cil.Attr (cil_inserted_attribute_string, []) ];
        init_cil ()
    

    let rec access_lval (l, o) =
        let rec outermost_field f = function
            | Cil.NoOffset -> f
            | Cil.Field (f', o) -> outermost_field (Some f') o
            | Cil.Index (_, o) -> outermost_field f o
        in
        let lookup_lval = function
            | Cil.Var v -> lookup_var v (* x *)
            | Cil.Mem e -> interpret_exp e (* *x *)
        in
        match Cil.typeSig (Cil.typeOfLval (l, Cil.NoOffset)) with
            | Cil.TSComp (false, _, _) -> lookup_lval l (* don't lookup union fields *)
            | _ -> begin match outermost_field None o with
                | Some f -> lookup_field f
                | None -> lookup_lval l
                end
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
            if is_cil_inserted_type t then
                interpret_exp e (* ignore casts inserted by Cil *)
            else
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

