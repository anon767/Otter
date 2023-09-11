open Control.Monad
open TypeQual.QualType


module type InterpreterMonad = sig
    include Instruction.InterpreterMonad

    val interpret_block : Cil.block -> QualType.t monad
    val interpret_stmt : Cil.stmt -> QualType.t monad
end


module InterpreterT (I : Instruction.InterpreterMonad) = struct
    include I
    module Ops = MonadOps (I)
    open Ops

    let lift x = x

    let rec interpret_block { Cil.bstmts=stmtlist } = perform
        (* collect the qualified types of return statements *)
        qt <-- empty;
        foldM (fun a stmt -> perform qt <-- interpret_stmt stmt; join a qt) qt stmtlist

    and interpret_stmt { Cil.skind=skind } = match skind with
        (* only return interprets to a qualified type (of the expression) *)
        | Cil.Return ((Some e), loc) -> perform
            inContext (fun _ -> loc) (interpret_exp e)

        (* interpret instructions for side effects *)
        | Cil.Instr instrlist -> perform
            mapM_ interpret_instr instrlist;
            empty

        (* side-effect free statements *)
        | Cil.Return (None, loc)
        | Cil.Goto (_, loc)
        | Cil.Break loc
        | Cil.Continue loc ->
            empty

        (* safe to ignore condition expression since it's side-effect free *)
        | Cil.If (_, ifblock, elseblock, loc) ->
            inContext (fun _ -> loc) begin perform
                qtif <-- interpret_block ifblock;
                qtelse <-- interpret_block elseblock;
                join qtif qtelse
            end
        | Cil.Switch (_, block, _, loc)
        | Cil.Loop (block, loc, _, _) -> perform
            inContext (fun _ -> loc) (interpret_block block)
        | Cil.Block block ->
            interpret_block block

        | Cil.TryFinally (_, _, _)
        | Cil.TryExcept (_, _, _, _) ->
            failwith "MSVC structured exception not supported"
end

