open CilQual.Environment.CilFieldOrVarOrCast

open TypedBlock.G.QualType.Qual
open TypedBlock.G.QualType
open TypedBlock.G
open TypedBlock.GOps
open TypedBlock.DiscreteSolver
open TypedBlock

open Otter

open SwitchingUtil


module Switcher (T : Config.BlockConfig)  (S : Config.BlockConfig) = struct

    let switch dispatch file fn expState k =
        Format.eprintf "Switching from typed to symbolic at %s...@." fn.Cil.svar.Cil.vname;

        (* solve the typed constraints, needed to setup the symbolic constraints;
         * but first, connect the function prototype to the formal arguments *)
        let expM = perform
            qtf <-- lookup_var fn.Cil.svar;
            qta <-- args qtf;
            zipWithM_ (fun v a -> assign_lval (Cil.var v) a) fn.Cil.sformals qta
        in
        let ((((((), constraints), _), _), _), _ as expState) = run expM expState in
        let solution = DiscreteSolver.solve consts constraints in

        (* TODO: properly explain error *)
        if Solution.is_unsatisfiable solution then
            Format.eprintf "Unsatisfiable solution entering TypedSymbolic.switch at %s@." fn.Cil.svar.Cil.vname;

        (* convert a typed environment into a symbolic environment *)
        let state = MemOp.state__empty in

        (* first, setup global variables *)
        let (state, global) = List.fold_left begin fun (state, global) g -> match g with
            | Cil.GVarDecl (v, _) | Cil.GVar (v, _, _)
                    when not (Cil.isFunctionType v.Cil.vtype (* skip function prototypes; they're not variables *)
                              || Types.VarinfoMap.mem v state.Types.global) ->
                let (((((qt, _), _), _), _), _) = run (lookup_var v) expState in
                let state, lval_block = qt_to_lval_block file expState solution state v qt in
                (state, Types.VarinfoMap.add v lval_block global)
            | _ ->
                (state, global)
        end (state, MemOp.frame__empty) file.Cil.globals in
        let state = { state with Types.global=global } in

        (* then, setup function arguments *)
        (* TODO: handle varargs *)
        let (((((qta, _), _), _), _), _) = run begin perform
            qtf <-- lookup_var fn.Cil.svar;
            args qtf
        end expState in
        let state, args_bytes = List.fold_left2 begin fun (state, args_bytes) v qt ->
            let state, bytes = qt_to_bytes file expState solution state (Cil.Lval (Cil.var v)) qt in
            (state, bytes::args_bytes)
        end (state, []) fn.Cil.sformals qta in

        (* next, prepare the function call job *)
        let job = Executemain.job_for_function state fn args_bytes in

        (* finally, prepare the completion continuation *)
        let completion completed =
            let completed_count = List.length completed in
            Format.eprintf "Returning from symbolic to typed at %s (%d execution%s returned)...@."
                fn.Cil.svar.Cil.vname
                completed_count (if completed_count == 1 then "" else "s");

            (* prepare a monad that represents the symbolic result *)
            let expM = foldM begin fun block_errors results -> match results with
                | Types.Return (retopt, { Types.result_state=state; Types.result_history=history }), _ ->
                    inContext (fun _ -> fn.Cil.svar.Cil.vdecl) begin perform
                        (* first, the return value *)
                        begin match retopt with
                            | None ->
                                return ()
                            | Some ret -> perform
                                qtf <-- lookup_var fn.Cil.svar;
                                qtr <-- retval qtf;

                                (* bytes_to_qt requires the expression from which the value was evaluated, but Otter
                                 * doesn't tell us which return expression generated this value; so, the below calls
                                 * bytes_to_qt with every return expression and merges the results *)
                                mapM_ begin fun retstmt ->
                                    match retstmt.Cil.skind with
                                        | Cil.Return (Some retexp, _) ->
                                            bytes_to_qt file expState state Bytes.Guard_True ret retexp qtr
                                        | _ ->
                                            return ()
                                end fn.Cil.sallstmts (* sallstmts is computed by Cil.computeCFGInfo *)
                        end;

                        (* then, the global variables and function arguments *)
                        mapM_ begin fun frame ->
                            frame_to_qt file expState state frame
                        end (state.Types.global::state.Types.formals);

                        return block_errors
                    end

                | Types.Abandoned (msg, loc, result), None ->
                    let block_msg = begin
                        Format.fprintf Format.str_formatter
                            "Block errors returning from TypedSymbolic.switch at %s: %s"
                            fn.Cil.svar.Cil.vname msg;
                        Format.flush_str_formatter ()
                    end in
                    return ((block_msg, loc, `TypedSymbolicError (result, msg))::block_errors)

                | Types.Abandoned _, Some e ->
                    return (e::block_errors)

                | Types.Exit _, _         (* a program that exits cannot possibly affect the outer context *)
                | Types.Truncated _, _ -> (* truncated paths are those merged with other paths *)
                    return block_errors

            end [] completed in

            let (((((block_errors, _), _), _), _), _ as expState) = run expM expState in
            let expState = run (return ()) expState in

            (* update the constraints and return *)
            k expState block_errors
        in

        (* dispatch *)
        dispatch (`SymbolicBlock (file, job, completion))


    let dispatch chain dispatch = function
        | `TypedBlock (file, fn, expState, k) when S.should_enter_block fn.Cil.svar.Cil.vattr ->
            switch dispatch file fn expState k
        | work ->
            chain work
end

