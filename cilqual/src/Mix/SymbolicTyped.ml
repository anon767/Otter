open CilQual.Environment.CilFieldOrVarOrCast

open TypedBlock.G.QualType.Qual
open TypedBlock.G.QualType
open TypedBlock.G
open TypedBlock.GOps
open TypedBlock.DiscreteSolver
open TypedBlock


open SwitchingUtil


module Switcher (S : Config.BlockConfig)  (T : Config.BlockConfig) = struct

    let switch dispatch file job k =
        Format.eprintf "Switching from symbolic to typed...@.";

        (* 1. convert globals and formals to type constraints
         * 2. type-check fn
         * 3. kill memory and reinitialize globals, formals and return value from type constraints
         *)

        (* initialize state *)
        let expState = ((((((), G.QualGraph.empty), (G.emptyContext)), 0), G.emptyUnionTable), G.emptyEnv) in
        let state = job.Types.state in
        let fn = List.hd state.Types.callstack in


        (* setup global variables and function arguments *)
        (* TODO: handle varargs *)
        let expM = perform
            state <-- frame_to_qt file expState state state.Types.global;
            state <-- frame_to_qt file expState state (List.hd state.Types.formals);
            return state
        in
        let (((((state, _), _), _), _), _ as expState) = run expM expState in


        (* prepare the completion continuation to perform the final check *)
        let completion (((((_, constraints), _), _), _), _ as expState) block_errors =
            Format.eprintf "Returning from typed to symbolic...@.";

            (* get the location of the caller statement *)
            let loc = match List.hd job.Types.state.Types.callContexts with
                | Types.Runtime                 -> Cil.locUnknown
                | Types.Source (_, _, instr, _)
                | Types.NoReturn instr          -> Cil.get_instrLoc instr
            in

            if block_errors != [] then begin
                let result = { Types.result_state = job.Types.state; Types.result_history = job.Types.exHist } in
                k [ (Types.Abandoned ("Block errors returning from SymbolicTyped.switch", loc, result),
                     Some ("Block errors returning from SymbolicTyped.switch", loc,
                           `SymbolicTypedError (result, block_errors))) ]

            end else begin
                let solution = DiscreteSolver.solve consts constraints in

                (* TODO: properly explain error *)
                if DiscreteSolver.Solution.is_unsatisfiable solution then begin
                    let result = { Types.result_state = job.Types.state; Types.result_history = job.Types.exHist } in
                    k [ (Types.Abandoned ("Unsatisfiable solution returning from SymbolicTyped.switch", loc, result),
                         None) ]

                end else begin

                    (* first, the return value *)
                    let state, retopt = begin match fn.Cil.svar.Cil.vtype with
                        | Cil.TFun (rettyp, _, _, _) when Cil.isVoidType rettyp ->
                            (state, None)

                        | Cil.TFun (_, _, _, _) ->
                            let (((((qtr, _), _), _), _), _) = run begin perform
                                qtf <-- lookup_var fn.Cil.svar;
                                retval qtf
                            end expState in
                            (* Ptranal has no query for function return values; instead, query all the expressions in
                             * function returns, and merge them *)
                            let state, retbytes_list = List.fold_left begin fun (state, retbytes_list) retstmt ->
                                match retstmt.Cil.skind with
                                    | Cil.Return (Some retexp, _) ->
                                        let state, retbytes = qt_to_bytes file expState solution state retexp qtr in
                                        (state, ((Bytes.conditional__bytes retbytes)::retbytes_list))
                                    | _ ->
                                        (state, retbytes_list)
                            end (state, []) fn.Cil.sallstmts in (* sallstmts is computed by Cil.computeCFGInfo *)
                            begin match retbytes_list with
                                | [] -> (state, None)
                                | _  -> (state, Some (Bytes.make_Bytes_Conditional (Bytes.conditional__from_list retbytes_list)))
                            end
                        | _ ->
                            failwith "Impossible!"
                    end in

                    (* then, the globals and call stack *)
                    let state, global = qt_to_frame file expState solution state state.Types.global in

                    let state, formals = List.fold_right begin fun formal (state, formals) ->
                        let state, formal = qt_to_frame file expState solution state formal in
                        (state, (formal::formals))
                    end state.Types.formals (state, []) in

                    let state, locals = List.fold_right begin fun local (state, locals) ->
                        let state, local = qt_to_frame file expState solution state local in
                        (state, (local::locals))
                    end state.Types.locals (state, []) in

                    let state = { state with
                        Types.global=global;
                        Types.locals=locals;
                        Types.formals=formals;
                        Types.extra=Types.VarinfoMap.empty;
                        Types.malloc=Types.VarinfoMap.empty;
                    } in

                    k [ (Types.Return (retopt, { Types.result_state=state; Types.result_history=job.Types.exHist }),
                         None) ]
                end
            end
        in

        let expState = run (return ()) expState in (* TODO: fix the type *)
        dispatch (`TypedBlock (file, fn, expState, completion))


    let dispatch chain dispatch work = match work with
        | `SymbolicBlock (file, job, k)
                when T.should_enter_block (List.hd job.Types.state.Types.callstack).Cil.svar.Cil.vattr ->
            switch dispatch file job k
        | work ->
            chain work
end

