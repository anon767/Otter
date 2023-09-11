
(** Invert conditional branches. This will include all logical && and || as well, since CIL simplifies
    them to conditional branches (unless Cil.useLogicalOperators is true). *)

class patchmaker () =
    object (self)
        inherit Randommod.patchmakingVisitor
        method name = "flipBranch"
        method vstmt s = match s.Cil.skind with
            (* remove/insert not *)
            | Cil.If(Cil.UnOp(Cil.LNot, exp, _), thenb, elseb, loc) ->
                    [ { s with Cil.skind = Cil.If(exp, thenb, elseb, loc) } ];
            | Cil.If(exp, thenb, elseb, loc) ->
                    [ { s with Cil.skind = Cil.If(Cil.UnOp(Cil.LNot, exp, Cil.intType), thenb, elseb, loc) } ];
            | _ -> []
    end;;
Randommod.registerPatchmaker new patchmaker
