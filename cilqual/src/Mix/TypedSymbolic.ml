open CilQual.Environment.CilFieldOrVar

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
        let expM = perform
            (* first, setup global variables *)
            state <-- foldM begin fun state g -> match g with
                | Cil.GVarDecl (v, _) | Cil.GVar (v, _, _)
                        when not (Types.VarinfoMap.mem v state.Types.global.Types.varinfo_to_block) -> perform
                    qt <-- lookup_var v;
                    (state, bytes) <-- qt_to_bytes solution state Types.Block_type_Global v.Cil.vtype (drop_qt qt);
                    let state = MemOp.state__add_global state v bytes in
                    return state
                | _ ->
                    return state
            end state file.Cil.globals;

            (* then, setup function arguments *)
            (* TODO: handle varargs *)
            qtf <-- lookup_var fn.Cil.svar;
            qta <-- args qtf;
            (state, args_bytes) <-- List.fold_left2 begin fun accM v qt -> perform
                (state, args_bytes) <-- accM;
                (state, bytes) <-- qt_to_bytes solution state Types.Block_type_Local v.Cil.vtype qt;
                return (state, bytes::args_bytes)
            end (return (state, [])) fn.Cil.sformals qta;

            (* next, prepare the function call job *)
            let job = Executemain.job_for_function state fn args_bytes in
            return job
        in
        let (((((job, _), _), _), _), _ as expState) = run expM expState in


        (* finally, prepare the completion continuation *)
        let completion completed =
            let completed_count = List.length completed in
            Format.eprintf "Returning from symbolic to typed (%d execution%s returned)...@."
                completed_count (if completed_count == 1 then "" else "s");

            (* prepare a monad that represents the symbolic result *)
            let expM = mapM_ begin function
                | Types.Return (retopt, { Types.result_state=state; Types.result_history=history }) -> perform
                    (* first, the return value *)
                    begin match retopt with
                        | None ->
                            return ()
                        | Some ret -> perform
                            let rettyp = match fn.Cil.svar.Cil.vtype with
                                | Cil.TFun (rettyp, _, _, _) -> rettyp
                                | _ -> failwith "Impossible!"
                            in
                            qtf <-- lookup_var fn.Cil.svar;
                            qtr <-- retval qtf;
                            bytes_to_qt state rettyp ret qtr;
                            return ()
                    end;

                    (* then, the global variables and call stack *)
                    mapM_ begin fun frame ->
                        frame_bytes_to_qt state frame
                    end (state.Types.global::(state.Types.formals @ state.Types.locals))

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

