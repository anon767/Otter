open CilQual.Environment.CilFieldOrVarOrCast

open TypedBlock.G.QualType.Qual
open TypedBlock.G.QualType
open TypedBlock.G
open TypedBlock.GOps
open TypedBlock.DiscreteSolver
open TypedBlock

open SwitchingUtil


module Switcher (T : Config.BlockConfig)  (S : Config.BlockConfig) = struct

    let switch dispatch file fn expState k =
        Format.eprintf "Switching from typed to symbolic...@.";

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
            Format.eprintf "Unsatisfiable solution entering TypedSymbolic.switch@.";

        (* convert a typed environment into a symbolic environment *)
        let state = MemOp.state__empty in

        (* first, setup global variables *)
        let state = List.fold_left begin fun state g -> match g with
            | Cil.GVarDecl (v, _) | Cil.GVar (v, _, _)
                    when not (Types.VarinfoMap.mem v state.Types.global.Types.varinfo_to_block) ->
                let (((((qt, _), _), _), _), _) = run (lookup_var v) expState in
                let state, bytes =
                    qt_to_bytes file expState solution state (Cil.Lval (Cil.Var v, Cil.NoOffset)) (drop_qt qt)
                in
                MemOp.state__add_global state v bytes
            | _ ->
                state
        end state file.Cil.globals in

        (* then, setup function arguments *)
        (* TODO: handle varargs *)
        let (((((qta, _), _), _), _), _) = run begin perform
            qtf <-- lookup_var fn.Cil.svar;
            args qtf
        end expState in
        let state, args_bytes = List.fold_left2 begin fun (state, args_bytes) v qt ->
            let state, bytes = qt_to_bytes file expState solution state (Cil.Lval (Cil.Var v, Cil.NoOffset)) qt in
            (state, bytes::args_bytes)
        end (state, []) fn.Cil.sformals qta in

        (* next, prepare the function call job *)
        let job = Executemain.job_for_function state fn args_bytes in


        (* finally, prepare the completion continuation *)
        let completion completed =
            let completed_count = List.length completed in
            Format.eprintf "Returning from symbolic to typed (%d execution%s returned)...@."
                completed_count (if completed_count == 1 then "" else "s");

            let old_state = state in

            (* prepare a monad that represents the symbolic result *)
            let expM = mapM_ begin function
                | Types.Return (retopt, { Types.result_state=state; Types.result_history=history }) -> perform
                    (* first, the return value *)
                    state <-- begin match retopt with
                        | None ->
                            return state
                        | Some ret -> perform
                            let rettyp = match fn.Cil.svar.Cil.vtype with
                                | Cil.TFun (rettyp, _, _, _) -> rettyp
                                | _ -> failwith "Impossible!"
                            in
                            qtf <-- lookup_var fn.Cil.svar;
                            qtr <-- retval qtf;
                            bytes_to_qt old_state state rettyp ret qtr
                    end;

                    (* then, the global variables and call stack *)
                    foldM begin fun state frame ->
                        frame_bytes_to_qt old_state state frame
                    end state (state.Types.global::(state.Types.formals @ state.Types.locals));
                    return ()

                | Types.Abandoned (msg, loc, result) ->
                    failwith (Format.sprintf "TODO: handle abandoned path @@ %s:%d (%s)" loc.Cil.file loc.Cil.line msg)

                | Types.Exit _         (* a program that exits cannot possibly affect the outer context *)
                | Types.Truncated _ -> (* truncated paths are those merged with other paths *)
                    return ()

            end completed in

            (* update the constraints and return *)
            k (run expM expState)
        in

        (* dispatch *)
        dispatch (`SymbolicBlock (file, job, completion))


    let dispatch chain dispatch = function
        | `TypedBlock (file, fn, expState, k) when S.should_enter_block fn.Cil.svar.Cil.vattr ->
            switch dispatch file fn expState k
        | work ->
            chain work
end

