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
                qtr <-- app qtf qta;
                begin match Cil.typeSig (Cil.typeOfLval f) with
                    (* for vararg functions, fold the vararg arguments *)
                    | Cil.TSFun (_, _, true, _) -> perform
                        qtp <-- args qtf;
                        foldM begin fun ps a -> match ps with
                            | p::[] -> perform assign p a; return ps
                            | _::ps -> return ps
                            | _ -> failwith "Impossible!"
                        end qtp qta;
                        return ()
                    | Cil.TSFun (_, _, false, _) ->
                        return ()
                    | _ ->
                        failwith "Impossible!"
                end;
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

