open Control.Monad
open Control.Graph

(* setup CilQual interpreter monad stack *)
module G =
    CilQual.Global.InterpreterT
    (CilQual.Statement.InterpreterT
    (CilQual.Instruction.InterpreterT
    (CilQual.Expression.InterpreterT
    (CilQual.Environment.InterpreterT
    (CilQual.Type.InterpreterT
    (CilQual.CilUnionQualType.CilUnionQualTypeT (CilQual.Environment.CilFieldOrVar) (CilQual.CilUnionQualType.Context)
    (Identity)))))))
module GOps = MonadOps (G)
open G
open GOps


(* setup CilQual solver *)
let consts = [ "null"; "nonnull" ]
module DiscreteSolver = TypeQual.QualSolver.DiscreteOrder (G.QualGraph)


module Interpreter (T : Config.BlockConfig) = struct
    module CallSet = Set.Make (CilQual.CilData.CilFundec)


    (* Cil setup *)
    let init_cil = CilQual.Feature.init_cil
    let prepare_file = CilQual.Feature.prepare_file


    let calls_in_function fn =
        let rec calls_in_instr calls = function
            | Cil.Call (lopt, Cil.Lval f, a, loc) ->
                f::calls
            | Cil.Call (_, _, _, _) ->
                failwith "Does Cil generate other variations of function call expressions?"
            | _ ->
                calls
        and calls_in_block calls { Cil.bstmts=stmtlist } =
            List.fold_left calls_in_stmt calls stmtlist
        and calls_in_stmt calls { Cil.skind=skind } = match skind with
            | Cil.Instr instrlist ->
                List.fold_left calls_in_instr calls instrlist
            | Cil.If (_, ifblock, elseblock, _) ->
                let calls = calls_in_block calls ifblock in
                let calls = calls_in_block calls elseblock in
                calls
            | Cil.Switch (_, block, _, _)
            | Cil.Loop (block, _, _, _)
            | Cil.Block block ->
                calls_in_block calls block
            | _ ->
                calls
        in
        calls_in_block [] fn.Cil.sbody


    let call dispatch file fn expState k =
        Format.eprintf "Evaluating typed block...@.";

        (* first, determine the call graph inside the typed-block *)
        let rec find_calls typed_calls other_calls = function
            | fn::fnwork ->
                let typed_calls, other_calls, fnwork =
                    List.fold_left begin fun (typed_calls, other_calls, fnwork) call ->
                        match call with
                            | Cil.Var v, Cil.NoOffset ->
                                let f = Cilutility.search_function v in
                                if CallSet.mem f typed_calls || CallSet.mem f other_calls then
                                    (typed_calls, other_calls, fnwork)
                                else if T.is_model_block v.Cil.vattr then
                                    (* model functions are not interpreted *)
                                    (typed_calls, other_calls, fnwork)
                                else if T.should_enter_block v.Cil.vattr then
                                    (* recurse into typed functions *)
                                    (CallSet.add f typed_calls, other_calls, f::fnwork)
                                else
                                    (* other functions are delegated *)
                                    (typed_calls, CallSet.add f other_calls, fnwork)
                            | Cil.Mem _, _ ->
                                failwith "TODO: support calls through function pointers"
                            | _, _ ->
                                failwith "Does Cil generate calls through (_, offset)?"
                    end (typed_calls, other_calls, fnwork) (calls_in_function fn)
                in
                find_calls typed_calls other_calls fnwork

            | [] ->
                (typed_calls, other_calls)
        in
        let typed_calls, other_calls = find_calls CallSet.empty CallSet.empty [ fn ] in

        (* generate the interpreter monad and evaluate *)
        let expM = mapM_ interpret_function (fn::(CallSet.elements typed_calls)) in
        let expState = G.run expM expState in

        let rec call_others others expState = match others with
            | [] -> k expState
            | call::callwork -> dispatch (`TypedBlock (file, call, expState, call_others callwork))
        in
        call_others (CallSet.elements other_calls) expState


    let exec file =
        (* generate and evaluate everything before main *)
        let expM = interpret_init file in
        let expState = G.run expM ((((((), G.QualGraph.empty), (G.emptyContext)), 0), G.emptyUnionTable), G.emptyEnv) in

        (* prepare the return continuation to perform the final check *)
        let return (((((_, constraints), _), _), _), _) =
            let solution = DiscreteSolver.solve consts constraints in

            (* TODO: properly explain error *)
            if DiscreteSolver.Solution.is_unsatisfiable solution then
                Format.eprintf "Unsatisfiable solution in TypedBlock.exec@.";

            (file, solution)
        in

        (* dispatch call to main *)
        let mainfn = Function.from_name_in_file "main" file in
        `TypedBlock (file, mainfn, expState, return)


    let dispatch chain dispatch = function
        | `TypedBlock (file, fn, expState, k) when T.should_enter_block fn.Cil.svar.Cil.vattr ->
            call dispatch file fn expState k
        | work ->
            chain work
end

