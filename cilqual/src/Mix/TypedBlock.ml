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
    (CilQual.CilUnionQualType.CilUnionQualTypeT (CilQual.Environment.CilFieldOrVarOrCast) (CilQual.CilUnionQualType.Context)
    (Identity)))))))
module GOps = MonadOps (G)
open G
open GOps

open Otter


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
                (f, loc)::calls
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


    let call dispatch stack file fn expState k =
        Format.eprintf "Evaluating typed block at %s...@." fn.Cil.svar.Cil.vname;

        (* first, determine the call graph inside the typed-block *)
        let rec find_calls typed_calls other_calls = function
            | fn::fnwork ->
                let typed_calls, other_calls, fnwork =
                    let categorize_call (typed_calls, other_calls, fnwork) f =
                        if CallSet.mem f typed_calls || CallSet.mem f other_calls then
                            (typed_calls, other_calls, fnwork)
                        else if T.is_model_block f.Cil.svar.Cil.vattr then
                            (* model functions are not interpreted *)
                            (typed_calls, other_calls, fnwork)
                        else if T.should_enter_block f.Cil.svar.Cil.vattr then
                            (* recurse into typed functions *)
                            (CallSet.add f typed_calls, other_calls, f::fnwork)
                        else
                            (* other functions are delegated *)
                            (typed_calls, CallSet.add f other_calls, fnwork)
                    in
                    List.fold_left begin fun calls (call, loc) ->
                        match call with
                            | Cil.Var v, Cil.NoOffset ->
                                begin try
                                    let f = Cilutility.search_function v in
                                    categorize_call calls f
                                with Not_found ->
                                    (* is an external function *)
                                    calls
                                end
                            | Cil.Mem exp, _ ->
                                (* handle function pointers using Cil's pointer analysis *)
                                let points_to = Ptranal.resolve_funptr exp in
                                Format.eprintf "%s:%d: warning:@[Function pointer call %s:@ @[%t@]@]@\n"
                                    loc.Cil.file loc.Cil.line
                                    (Pretty.sprint 0 (Cil.d_exp () (Cil.Lval call)))
                                    begin fun ff -> ignore begin List.fold_left begin fun b f ->
                                        Format.fprintf ff "%(%)%s"
                                            b
                                            (Pretty.sprint 0 (Cil.d_lval () (Cil.var f.Cil.svar)));
                                        ", "
                                    end "" points_to end end;
                                List.fold_left categorize_call calls points_to
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
        let expM = mapM_ begin fun fn ->
            inContext (fun _ -> fn.Cil.svar.Cil.vdecl) (interpret_function fn)
        end (fn::(CallSet.elements typed_calls)) in
        let expState = G.run expM expState in

        (* evaluate other_calls repeatedly until the constraint graph reaches a fixpoint *)
        let rec do_fixpoint other_calls stack (((((_, old_constraints), _), _), _), _ as expState) =
            let rec call_others work block_errors stack (((((_, constraints), _), _), _), _ as expState) more_block_errors =
                let block_errors = more_block_errors @ block_errors in
                match work with
                    | call::work ->
                        dispatch stack (`TypedBlock (file, call, expState, call_others work block_errors))
                    | [] ->
                        (* ocamlgraph has no graph equality operation? *)
                        if block_errors != []
                                || (G.QualGraph.nb_vertex constraints == G.QualGraph.nb_vertex old_constraints
                                    && G.QualGraph.nb_edges constraints == G.QualGraph.nb_edges old_constraints) then
                            k stack expState block_errors
                        else
                            do_fixpoint other_calls stack expState
            in
            call_others other_calls [] stack expState []
        in
        do_fixpoint (CallSet.elements other_calls) stack expState


    let exec file =
        (* generate and evaluate everything before main *)
        let expM = interpret_init file in
        let expState = G.run expM ((((((), G.QualGraph.empty), (G.emptyContext)), 0), G.emptyUnionTable), G.emptyEnv) in

        (* prepare the completion continuation to perform the final check *)
        let completion stack (((((_, constraints), _), _), _), _) block_errors =
            Format.eprintf "Completing typed block...@.";

            let solution = DiscreteSolver.solve consts constraints in

            let explanation = if DiscreteSolver.Solution.is_unsatisfiable solution then
                Some (DiscreteSolver.explain solution)
            else
                None
            in
            begin match explanation with
                | Some explanation ->
                    Format.eprintf "@.";
                    Format.eprintf "Shortest paths between $null and $nonnull:@\n  @[%a@]@."
                        DiscreteSolver.Explanation.printer explanation;

                    (*
                     * temporary workaround to report warnings that are easier to understand, by removing aliasing edges
                     * that carry no file or line number as explanation.
                     *)
                    let no_aliasing_graph = G.QualGraph.fold_edges_e begin fun edge no_aliasing_graph ->
                        Format.fprintf Format.str_formatter "%a" G.QualGraph.edge_printer edge;
                        if Format.flush_str_formatter () = "<aliasing>" then
                            G.QualGraph.remove_edge_e no_aliasing_graph edge
                        else
                            no_aliasing_graph
                    end constraints constraints in
                    let no_aliasing_solution = DiscreteSolver.solve consts no_aliasing_graph in
                    if DiscreteSolver.Solution.is_unsatisfiable no_aliasing_solution then begin
                        Format.eprintf "@.";
                        Format.eprintf "Shortest no-aliasing paths between $null and $nonnull:@\n  @[%a@]@."
                            DiscreteSolver.Explanation.printer (DiscreteSolver.explain no_aliasing_solution)
                    end;
                    ()
                | None ->
                    ()
            end;
            if block_errors != [] then begin
                let printer ff block_errors = ignore begin List.iter begin fun (s, l, b) ->
                    Format.fprintf ff "@[%s:%d: %s@]@\n@\n" l.Cil.file l.Cil.line s;
                end block_errors end in
                Format.eprintf "@.";
                Format.eprintf "Block errors:@\n  @[%a@]@\n@." printer block_errors;
            end;

            (* summary *)
            begin match explanation with
                | Some explanation ->
                    let unsolvables =
                        DiscreteSolver.Solution.Unsolvables.cardinal (DiscreteSolver.Solution.unsolvables solution)
                    in
                    let classes =
                        DiscreteSolver.Solution.EquivalenceClasses.cardinal
                            (DiscreteSolver.Solution.unsolvables_classes solution)
                    in
                    let paths =
                        DiscreteSolver.Explanation.cardinal explanation
                    in
                    Format.eprintf "Unsatisfiable solution in TypedBlock.exec:@\n  @[";
                    Format.eprintf "%d unsolvable annotated qualifier variable%s found in %d equivalence class%s.@\n"
                        unsolvables (if unsolvables == 1 then "" else "s")
                        classes (if classes == 1 then "" else "es");
                    Format.eprintf "%d shortest path%s between $null and $nonnull reported.@\n"
                        paths (if paths == 1 then "" else "s");
                    Format.eprintf "@]@.";
                | None ->
                    ()
            end;
            if block_errors != [] then begin
                let count = List.length block_errors in
                Format.eprintf "%d block error%s in TypedBlock.exec@\n@."
                    count (if count == 1 then "" else "s");
            end;

            (file, solution, block_errors)
        in

        (* dispatch call to main *)
        let mainfn = Function.from_name_in_file "main" file in
        `TypedBlock (file, mainfn, expState, completion)


    let dispatch chain dispatch stack = function
        | `TypedBlock (file, fn, expState, k) when T.should_enter_block fn.Cil.svar.Cil.vattr ->
            call dispatch stack file fn expState k
        | work ->
            chain stack work
end

