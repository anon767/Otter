open CilQual.Environment.CilFieldOrVarOrCast

open TypedBlock.G.QualType.Qual
open TypedBlock.G.QualType
open TypedBlock.G
open TypedBlock.GOps
open TypedBlock.DiscreteSolver
open TypedBlock

open Otter

open SwitchingUtil


module Switcher (S : Config.BlockConfig)  (T : Config.BlockConfig) = struct

    let typed_to_symbolic file job fn state expState solution =
        (* re-initialize the memory according to the given type constraints *)

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

        [ (Types.Return (retopt, { Types.result_state=state; Types.result_history=job.Types.exHist }), None) ]

    let switch dispatch stack file job k =
        (* 1. convert globals and formals to type constraints
         * 2. type-check fn
         * 3. kill memory and reinitialize globals, formals and return value from type constraints
         *)

        (* initialize state *)
        let expState = ((((((), G.QualGraph.empty), (G.emptyContext)), 0), G.emptyUnionTable), G.emptyEnv) in
        let state = job.Types.state in
        let fn = List.hd state.Types.callstack in

        (* get the location of the caller statement, for error reporting purposes *)
        let loc = match List.hd job.Types.state.Types.callContexts with
            | Types.Runtime                 -> Cil.locUnknown
            | Types.Source (_, _, instr, _)
            | Types.NoReturn instr          -> Cil.get_instrLoc instr
        in

        Format.eprintf "Switching from symbolic to typed at %s...@." fn.Cil.svar.Cil.vname;

        (* setup global variables and function arguments *)
        (* TODO: handle varargs *)
        let expM = perform
            frame_to_qt file expState state state.Types.global;
            frame_to_qt file expState state (List.hd state.Types.formals);
        in
        let (((((_, constraints), _), _), _), _ as expState) = run expM expState in

        let context = DiscreteSolver.solve consts constraints in

        (* TODO: need a better mechanism for explaining errors *)
        if DiscreteSolver.Solution.is_unsatisfiable context then begin
            (* don't switch; it'll return an error anyway *)
            let result = { Types.result_state = job.Types.state; Types.result_history = job.Types.exHist } in
            let explanation = DiscreteSolver.explain context in
            Format.fprintf Format.str_formatter
                "Unsatisfiable solution for context entering SymbolicTyped at %s:@\n  @[%a@]"
                fn.Cil.svar.Cil.vname
                DiscreteSolver.Explanation.printer explanation;
            Format.eprintf
                "Unsatisfiable solution for context entering SymbolicTyped at %s:@\n  @[%a@]@."
                fn.Cil.svar.Cil.vname
                DiscreteSolver.Explanation.printer explanation;
            k stack [ (Types.Abandoned (Format.flush_str_formatter (), loc, result),
                       None) ]

        end else begin
            (* prepare the completion continuation to perform the final check *)
            let completion stack (((((_, constraints), _), _), _), _ as expState) block_errors =
                Format.eprintf "Returning from typed to symbolic at %s...@." fn.Cil.svar.Cil.vname;

                if block_errors != [] then begin
                    let result = { Types.result_state = job.Types.state; Types.result_history = job.Types.exHist } in
                    let msg = "Block errors returning from SymbolicTyped.switch at " ^ fn.Cil.svar.Cil.vname in
                    k stack [ (Types.Abandoned (msg, loc, result),
                               Some (msg, loc, `SymbolicTypedError (result, block_errors))) ]

                end else begin
                    let context = DiscreteSolver.solve consts constraints in

                    (* TODO: need a better mechanism for explaining errors *)
                    if DiscreteSolver.Solution.is_unsatisfiable context then begin
                        let result = { Types.result_state = job.Types.state; Types.result_history = job.Types.exHist } in
                        let explanation = DiscreteSolver.explain context in
                        Format.fprintf Format.str_formatter
                            "Unsatisfiable solution for context returning from SymbolicTyped at %s:@\n  @[%a@]"
                            fn.Cil.svar.Cil.vname
                            DiscreteSolver.Explanation.printer explanation;
                        Format.eprintf
                            "Unsatisfiable solution for context returning from SymbolicTyped at %s:@\n  @[%a@]@."
                            fn.Cil.svar.Cil.vname
                            DiscreteSolver.Explanation.printer explanation;
                        k stack [ (Types.Abandoned (Format.flush_str_formatter (), loc, result),
                                   None) ]

                    end else begin
                        let completed = typed_to_symbolic file job fn state expState context in
                        k stack completed

                    end
                end
            in

            let expState = run (return ()) expState in (* TODO: fix the type *)
            dispatch stack (`TypedBlock (file, fn, expState, completion))
        end


    let dispatch chain dispatch stack = function
        | `SymbolicBlock (file, job, k)
                when T.should_enter_block (List.hd job.Types.state.Types.callstack).Cil.svar.Cil.vattr ->
            switch dispatch stack file job k
        | work ->
            chain stack work
end

