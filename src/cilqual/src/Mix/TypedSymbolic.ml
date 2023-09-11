open CilQual.Environment.CilFieldOrVarOrCast

open TypedBlock.G.QualType.Qual
open TypedBlock.G.QualType
open TypedBlock.G
open TypedBlock.GOps
open TypedBlock.DiscreteSolver
open TypedBlock

open OcamlUtilities
open OtterBytes
open OtterCore

open SwitchingUtil


module Switcher (T : Config.BlockConfig)  (S : Config.BlockConfig) = struct

    let cache stack fn context solution block_errors =
        (* cache the result at the end of the stack *)
        List.rev (`TypedSymbolicCached (fn, context, solution, block_errors)::(List.rev stack))


    let typed_to_typed file fn expState solution =
        (* prepare a monad that represents the result *)
        let expM = perform
            inContext (fun _ -> fn.Cil.svar.Cil.vdecl) begin perform
                (* first, the return value *)
                begin match fn.Cil.svar.Cil.vtype with
                    | Cil.TFun (rettyp, _, _, _) ->
                        if Cil.isVoidType rettyp then
                            return ()
                        else perform
                            qtf <-- lookup_var fn.Cil.svar;
                            qtr <-- retval qtf;
                            (* solution_to_qt requires the expression from which the value was evaluated, but
                             * Otter doesn't tell us which return expression generated this value; so, the below
                             * calls solution_to_qt with every return expression and merges the results *)
                            mapM_ begin fun retstmt ->
                                match retstmt.Cil.skind with
                                    | Cil.Return (Some retexp, _) ->
                                        solution_to_qt file expState solution retexp qtr
                                    | _ ->
                                        return ()
                            end fn.Cil.sallstmts (* sallstmts is computed by Cil.computeCFGInfo *)
                    | _ ->
                        failwith "Impossible!"
                end;

                (* then, the global variables *)
                mapM_ begin function
                    | Cil.GVarDecl (v, _) | Cil.GVar (v, _, _)
                            when not (Cil.isFunctionType v.Cil.vtype) -> perform
                                 (* skip function prototypes; they're not variables *)
                        qtl <-- access_rval (Cil.var v);
                        solution_to_qt file expState solution (Cil.Lval (Cil.var v)) qtl
                    | _ ->
                        return ()
                end file.Cil.globals;

                (* finally, the formals *)
                mapM_ begin fun v -> perform
                    qta <-- access_rval (Cil.var v);
                    solution_to_qt file expState solution (Cil.Lval (Cil.var v)) qta
                end fn.Cil.sformals
            end
        in

        (* return the updated constraints *)
        run expM expState


    let switch dispatch stack file fn expState context k =

        (* convert a typed environment into a symbolic environment *)
        let state = MemOp.state__empty in

        (* first, setup global variables *)
        let (state, global) = List.fold_left begin fun (state, global) g -> match g with
            | Cil.GVarDecl (v, _) | Cil.GVar (v, _, _)
                    when not (Cil.isFunctionType v.Cil.vtype (* skip function prototypes; they're not variables *)
                              || Types.VarinfoMap.mem v state.Types.global) ->
                let (((((qt, _), _), _), _), _) = run (lookup_var v) expState in
                let state, lval_block = qt_to_lval_block file expState context state v qt in
                (state, Types.VarinfoMap.add v lval_block global)
            | _ ->
                (state, global)
        end (state, MemOp.frame__empty) file.Cil.globals in
        let state = { state with Types.global=global } in

        (* then, setup function arguments *)
        (* TODO: handle varargs *)
        let state, rev_args_bytes = List.fold_left begin fun (state, args_bytes) v ->
            let (((((qt, _), _), _), _), _) = run (lookup_var v) expState in
            let state, bytes = qt_to_bytes file expState context state (Cil.Lval (Cil.var v)) (drop_qt qt) in
            (state, bytes::args_bytes)
        end (state, []) fn.Cil.sformals in

        (* next, prepare the function call job *)
        let job = Job.make file state fn (List.rev rev_args_bytes) in

        (* finally, set up the recursion fixpoint operation *)
        let rec do_fixpoint stack tentative =

            (* push our tentative solution onto the stack for use by recursive calls *)
            let stack = (`TypedSymbolic (fn, context, tentative, false))::stack in

            (* prepare the completion continuation *)
            let completion stack completed =
                let completed_count = List.length completed in
                Format.eprintf "Returning from symbolic to typed at %s (%d execution%s returned)...@."
                    fn.Cil.svar.Cil.vname
                    completed_count (if completed_count == 1 then "" else "s");

                (* pop the stack *)
                let recursion_detected, stack = match stack with
                    | (`TypedSymbolic (_, _, _, recursion_detected))::tail -> (recursion_detected, tail)
                    | _ -> failwith "Impossible!"
                in

                (* prepare a monad that represents the symbolic result *)
                let expM = foldM begin fun block_errors results -> match results with
                    | Job.Return (retopt, { Job.result_state=state; Job.result_history=history }), _ ->
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

                    | Job.Abandoned (reason, loc, result), None ->
                        Format.fprintf Format.str_formatter
                            "Block errors returning from TypedSymbolic at %s: %a"
                            fn.Cil.svar.Cil.vname Report.abandoned_reason reason;
                        Format.eprintf
                            "Block errors returning from TypedSymbolic at %s: %a@."
                            fn.Cil.svar.Cil.vname Report.abandoned_reason reason;
                        let msg = FormatPlus.as_string Report.abandoned_reason reason in
                        return ((Format.flush_str_formatter (), loc, `TypedSymbolicError (result, msg))::block_errors)

                    | Job.Abandoned _, Some e ->
                        return (e::block_errors)

                    | Job.Exit _, _ ->
                        (* a program that exits cannot possibly affect the outer context *)
                        return block_errors

                end [] completed in

                (* update the constraints and solve *)
                let (((((block_errors, constraints), _), _), _), _ as expState) = run expM expState in
                let expState = run (return ()) expState in

                let solution = DiscreteSolver.solve consts constraints in

                if recursion_detected && not (Solution.includes tentative solution) then begin
                    (* the tentative solution was used by a recursive call, but it was too optimistic;
                     * retry with new solution *)
                    Format.eprintf "Reevaluating fixpoint for %s due to recursion...@." fn.Cil.svar.Cil.vname;
                    do_fixpoint stack solution

                end else begin
                    (* there was no recursion or the tentative solution was correct; cache the result and return *)
                    let stack = cache stack fn context solution block_errors in
                    k stack expState block_errors
                end
            in

            (* dispatch *)
            dispatch stack (`SymbolicBlock (file, job, completion))
        in

        (* kick off the fixpoint computation *)
        do_fixpoint stack context


    let inspect_stack dispatch stack file fn expState k =
        Format.eprintf "Switching from typed to symbolic at %s...@." fn.Cil.svar.Cil.vname;

        (* solve the typed constraints, needed to setup the symbolic constraints;
         * but first, connect the function prototype to the formal arguments *)
        let expM = perform
            qtf <-- lookup_var fn.Cil.svar;
            qta <-- args qtf;
            zipWithM_ (fun v a -> assign_lval (Cil.var v) a) fn.Cil.sformals qta
        in
        let ((((((), constraints), _), _), _), _ as expState) = run expM expState in
        let context = DiscreteSolver.solve consts constraints in

        (* TODO: properly explain error *)
        if Solution.is_unsatisfiable context then
            Format.eprintf "Unsatisfiable solution entering TypedSymbolic at %s@." fn.Cil.svar.Cil.vname;

        (* inspect the stack to determine if this call is recursive, or if there's a suitable cached solution,
         * by finding the same function in the stack and comparing the context; the domain of context' and context
         * may differ, since the enclosing typed block may be different and thus may contain different local
         * variables *)
        let rec inspect_stack inspected = function
            | `TypedSymbolic (fn', context', solution, _)::tail
                    when fn' == fn && Solution.equal context' context ->
                (* recursion detected: mark the stack and use the tentative solution *)
                Format.eprintf "Recursion detected for %s...@." fn.Cil.svar.Cil.vname;
                let stack = List.rev_append inspected ((`TypedSymbolic (fn', context', solution, true))::tail) in
                k stack (typed_to_typed file fn expState solution) []

            | `TypedSymbolicCached (fn', context', solution, block_errors)::_ as tail
                    when fn' == fn && Solution.equal context' context ->
                (* use the cached solution *)
                Format.eprintf "Using cached solution for %s...@." fn.Cil.svar.Cil.vname;
                let stack = List.rev_append inspected tail in
                k stack (typed_to_typed file fn expState solution) block_errors

            | head::tail ->
                inspect_stack (head::inspected) tail

            | [] ->
                (* regular call: switch into the symbolic block *)
                switch dispatch stack file fn expState context k
        in
        inspect_stack [] stack


    let dispatch chain dispatch stack = function
        | `TypedBlock (file, fn, expState, k) when S.should_enter_block fn.Cil.svar.Cil.vattr ->
            inspect_stack dispatch stack file fn expState k
        | work ->
            chain stack work
end

