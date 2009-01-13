
(** Add one to an numeric expression. Avoids boolean expressions. *)

class patchmaker () =
    object (self)
        inherit Randommod.patchmakingVisitor
        method name = "addOne"

        (* TODO: move skiptbl into patchmakingVisitor? *)
        val skiptbl = Hashtbl.create 8
        method vexpr e = match e with
            | _ when Hashtbl.mem skiptbl e -> Hashtbl.remove skiptbl e; []
            (* skip relops *)
            | Cil.BinOp((Cil.Lt | Cil.Gt | Cil.Le | Cil.Ge | Cil.Eq | Cil.Ne | Cil.LAnd | Cil.LOr), _, _, _)
            | Cil.UnOp(Cil.LNot, _, _) ->
                    []
            | _ -> begin
                    let t = Cil.typeOf e in
                    match t with
                        (* only numeric expressions *)
                        | Cil.TInt(_, _) | Cil.TFloat(_, _) as t -> [ Cil.BinOp(Cil.PlusA, e, Cil.one, t) ]
                        | _ -> []
                end
        method vstmt s = match s.Cil.skind with
            (* skip conditionals *)
            | Cil.If(e, _, _, _) -> Hashtbl.add skiptbl e (); []
            | _ -> []
    end;;
Randommod.registerPatchmaker new patchmaker
