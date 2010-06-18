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

    let cache stack fn context completed =
        (* cache the result at the end of the stack *)
        List.rev (`SymbolicTypedCached (fn, context, completed)::(List.rev stack))


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


    let switch dispatch stack file job fn loc state expState context k =

        (* set up the recursion fixpoint operation *)
        let rec do_fixpoint stack tentative =

            (* push our tentative solution onto the stack for use by recursive calls *)
            let stack = (`SymbolicTyped (fn, context, tentative, false))::stack in

            (* prepare the completion continuation to perform the final check *)
            let completion stack (((((_, constraints), _), _), _), _ as expState) block_errors =
                Format.eprintf "Returning from typed to symbolic at %s...@." fn.Cil.svar.Cil.vname;

                (* pop the stack *)
                let recursion_detected, stack = match stack with
                    | (`SymbolicTyped (_, _, _, recursion_detected))::tail -> (recursion_detected, tail)
                    | _ -> failwith "Impossible!"
                in

                if block_errors != [] then begin
                    let result = { Types.result_state = job.Types.state; Types.result_history = job.Types.exHist } in
                    let msg = "Block errors returning from SymbolicTyped at " ^ fn.Cil.svar.Cil.vname in
                    k stack [ (Types.Abandoned (msg, loc, result),
                               Some (msg, loc, `SymbolicTypedError (result, block_errors))) ]

                end else begin
                    let solution = DiscreteSolver.solve consts constraints in

                    if recursion_detected && not (Solution.includes tentative solution) then begin
                        (* the tentative solution was used by a recursive call, but it was too optimistic;
                         * retry with new solution *)
                        Format.eprintf "Reevaluating fixpoint for %s due to recursion...@." fn.Cil.svar.Cil.vname;
                        do_fixpoint stack solution

                    end else if DiscreteSolver.Solution.is_unsatisfiable solution then begin
                        (* there was no recursion or the tentative solution was correct, but the constraints were
                         * unsatisfiable; report as block errors and bail *)

                        (* TODO: need a better mechanism for explaining errors *)
                        let result = { Types.result_state = job.Types.state; Types.result_history = job.Types.exHist } in
                        let explanation = DiscreteSolver.explain solution in
                        Format.fprintf Format.str_formatter
                            "Unsatisfiable solution returning from SymbolicTyped at %s:@\n  @[%a@]"
                            fn.Cil.svar.Cil.vname
                            DiscreteSolver.Explanation.printer explanation;
                        Format.eprintf
                            "Unsatisfiable solution returning from SymbolicTyped at %s:@\n  @[%a@]@."
                            fn.Cil.svar.Cil.vname
                            DiscreteSolver.Explanation.printer explanation;

                        let msg = Format.flush_str_formatter () in
                        let completed loc = [ (Types.Abandoned (msg, loc, result), None) ] in

                        (* cache the result and return *)
                        let stack = cache stack fn context completed in
                        k stack (completed loc)

                    end else
                        (* there was no recursion or the tentative solution was correct; cache the result and return *)
                        let completed = typed_to_symbolic file job fn state expState solution in
                        let stack = cache stack fn context (fun _ -> completed) in
                        k stack completed
                end
            in
            let expState = run (return ()) expState in (* TODO: fix the type *)
            dispatch stack (`TypedBlock (file, fn, expState, completion))
        in

        (* kick off the fixpoint computation *)
        do_fixpoint stack context


    let inspect_stack dispatch stack file job k =
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

        (* setup memory *)
        (* TODO: handle varargs *)
        let expM = perform
            (* convert the entire memory to types; although only the topmost formals (i.e., the arguments) are
             * visible to the nested typed block, other formals and local variables must also be converted to
             * correctly re-initialize the symbolic memory (from types) when returning from the typed block. *)
            frame_to_qt file expState state state.Types.global;
            mapM_ (frame_to_qt file expState state) (state.Types.formals @ state.Types.locals);
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
            (* inspect the stack to determine if this call is recursive, or if there's a suitable cached solution,
             * by finding the same function in the stack and comparing the context; the domain of context' and context
             * may differ, since the enclosing symbolic block may be different and thus may contain different local
             * variables *)
            let rec inspect_stack inspected = function
                | `SymbolicTyped (fn', context', solution, _)::tail
                        when fn' == fn && Solution.equal context' context ->
                    (* recursion detected: mark the stack and use the tentative solution *)
                    Format.eprintf "Recursion detected in SymbolicTyped for %s...@." fn.Cil.svar.Cil.vname;
                    let stack = List.rev_append inspected ((`SymbolicTyped (fn', context', solution, true))::tail) in
                    k stack (typed_to_symbolic file job fn state expState solution)

                | `SymbolicTypedCached (fn', context', completed)::_ as tail
                        when fn' == fn && Solution.equal context' context ->
                    (* use the cached solution *)
                    Format.eprintf "Using cached solution for %s...@." fn.Cil.svar.Cil.vname;
                    let stack = List.rev_append inspected tail in
                    k stack (completed loc)

                | head::tail ->
                    inspect_stack (head::inspected) tail

                | [] ->
                    (* regular call: switch into the typed block *)
                    switch dispatch stack file job fn loc state expState context k
            in
            inspect_stack [] stack

        end


    let dispatch chain dispatch stack = function
        | `SymbolicBlock (file, job, k)
                when T.should_enter_block (List.hd job.Types.state.Types.callstack).Cil.svar.Cil.vattr ->
            inspect_stack dispatch stack file job k
        | work ->
            chain stack work
end

