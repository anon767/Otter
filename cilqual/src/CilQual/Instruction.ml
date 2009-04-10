open Control.Monad
open TypeQual.QualType


module type InterpreterMonad = sig
    include Expression.InterpreterMonad

    val assign_lval : Cil.lval -> QualType.t -> unit monad
    val interpret_instr : Cil.instr -> unit monad
end


module InterpreterT (E : Expression.InterpreterMonad) = struct
    include E
    module Ops = MonadOps (E)
    open Ops

    let lift x = x

    let assign_lval l qtv = perform
        qtl <-- access_rval l;
        assign qtl qtv

    let rec interpret_instr = function
        | Cil.Set (l, e, loc) ->
            inContext (fun _ -> loc) begin perform
                qt <-- interpret_exp e;
                assign_lval l qt
            end
        | Cil.Call (lopt, Cil.Lval f, a, loc) ->
            inContext (fun _ -> loc) begin perform
                qtf <-- access_lval f;
                qta <-- mapM interpret_exp a;
                qta <-- begin match Cil.typeSig (Cil.typeOfLval f) with
                        | Cil.TSFun (_, _, false, _) ->
                            return qta (* not vararg: map the arguments as is *)
                        | Cil.TSFun (_, atypelist, true, _) -> perform
                            (* is vararg: map the first n arguments, and fold the rest *)
                            qtvararg <-- embed_rval Cil.voidPtrType;
                            (_, qta) <-- foldM begin fun (n, a) x ->
                                if n > 0 then
                                    return (n - 1, x::a)
                                else begin perform
                                    assign qtvararg x;
                                    return (0, a)
                                end
                            end (List.length atypelist, []) qta;
                            return (List.rev (qtvararg::qta))
                        | _ -> failwith "Impossible!"
                    end;
                qtr <-- app qtf qta;
                begin match lopt with
                    | Some l -> assign_lval l qtr
                    | None -> return ()
                end
            end
        | Cil.Call (_, _, _, _) ->
            failwith "Does Cil generate other variations of function call expressions?"
        | Cil.Asm _ ->
            return () (*failwith "TODO: do something with asm"*)
end

