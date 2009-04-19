open CilQual.Environment.CilFieldOrVar


open TypedBlock.G.QualType.Qual
open TypedBlock.G.QualType
open TypedBlock.G
open TypedBlock.GOps
open TypedBlock.DiscreteSolver
open TypedBlock


module Switcher (T : Config.BlockConfig)  (S : Config.BlockConfig) = struct

    let switch dispatch file fn ((((((), constraints), _), _), _) as expState) k =
        Format.eprintf "Switching from typed to symbolic...@.";

        (* first, solve the typed constraints, needed to setup the symbolic constraints *)
        let solution = DiscreteSolver.solve consts constraints in

        (* TODO: properly explain error *)
        if Solution.is_unsatisfiable solution then
            Format.eprintf "Unsatisfiable solution entering TypedSymbolic.switch@.";

        let constrain_bytes state bytes qv typ =
            if Solution.is_const qv "null" solution then
                let null_constraint = Types.Bytes_Op (Types.OP_EQ, [(bytes, typ); (MemOp.bytes__zero, typ)]) in
                MemOp.state__add_path_condition state null_constraint
            else if Solution.is_const qv "nonnull" solution then
                let nonnull_constraint = Types.Bytes_Op (Types.OP_NE, [(bytes, typ); (MemOp.bytes__zero, typ)]) in
                MemOp.state__add_path_condition state nonnull_constraint
            else
                state
        in

        let qt_to_bytes state block_type typ = function
            (* take off one Ref to match the bytes representation *)
            (* TODO: this should be done in the monad to account for mismatched types *)
            | Ref (_, qt) ->
                let rec qt_to_bytes state block_type typ qt = match typ, qt with
                    | Cil.TPtr (typtarget, _), Ref (Var v, qtarget) ->
                        let state, target_bytes = qt_to_bytes state Types.Block_type_Global typtarget qtarget in
                        let target_block = MemOp.block__make
                            (Var.printer Format.str_formatter v; Format.flush_str_formatter ())
                            (MemOp.bytes__length target_bytes)
                            block_type
                        in
                        let state = MemOp.state__add_block state target_block target_bytes in
                        let bytes = Types.Bytes_Address (Some target_block, MemOp.bytes__zero) in
                        let state = constrain_bytes state bytes v typ in
                        state, bytes
                    | typ, Base (Var v) ->
                        let size = (Cil.bitsSizeOf (typ)) / 8 in
                        let size = if size <= 0 then 1 else size in
                        let bytes = MemOp.bytes__symbolic size in
                        let state = constrain_bytes state bytes v typ in
                        state, bytes
                    | _, _ ->
                        failwith "TODO: handle function pointers?"
                in
                qt_to_bytes state block_type typ qt
            | _ ->
                failwith "TODO: report qt_to_bytes of non-variable"
        in

        let varinfo_to_bytes expState state block_type v =
            (* TODO: handle structs *)
            (* force v to be initialized *)
            let (_, env) as expState = run (perform lookup_var v; return ()) expState in

            let state, bytes = qt_to_bytes state block_type v.Cil.vtype (Env.find (CilVar v) env) in
            (expState, state, bytes)
        in

        (* convert a typed environment into a symbolic environment *)
        let state = MemOp.state__empty in

        (* first, setup global variables *)
        let expState, state = List.fold_left begin
            fun (expState, state) g -> match g with
                | Cil.GVarDecl (v, _) | Cil.GVar (v, _, _)
                        when not (Types.VarinfoMap.mem v state.Types.global.Types.varinfo_to_block) ->
                    let expState, state, bytes =
                        varinfo_to_bytes expState state Types.Block_type_Global v
                    in
                    let state = MemOp.state__add_global state v bytes in
                    (expState, state)
                | _ ->
                    (expState, state)
        end (expState, state) file.Cil.globals in

        (* then, setup the function arguments *)
        let expState, state, args = List.fold_right begin
            fun v (expState, state, args) ->
                let expState, state, bytes =
                    varinfo_to_bytes expState state Types.Block_type_Local v
                in
                (expState, state, bytes::args)
        end fn.Cil.sformals (expState, state, []) in

        (* next, prepare the function call job *)
        let job = Executemain.job_for_function state fn args in

        (* finally, prepare the return continuation *)
        let return completed =
            Format.eprintf "Returning from symbolic to typed...@.";

            (* figure out the return value qualtype *)
            let _, env = expState in
            let qtr = match Env.find (CilVar fn.Cil.svar) env with
                | Fn (_, qtr, _) -> qtr
                | _ -> failwith "Impossible!"
            in

            (* prepare a monad that represents the symbolic result *)
            let expM = mapM_ begin function
                | Types.Return (retvalopt, { Types.result_state=state; Types.result_history=history }) ->
                    let constrain_qt bytes qt =
                        let rec constrain_qt bytes = function
                            (* TODO: account for mismatched types *)
                            | Ref (qv, qtarget) as qt ->
                                begin try perform
                                    let target_block, target_offset = Eval.deref state bytes in
                                    (* didn't fail, so is not a null pointer *)
                                    annot qt "nonnull";
                                    begin match qtarget with
                                        | Ref _ ->
                                            let target_bytes = MemOp.state__get_bytes_from_lval state
                                                (target_block, target_offset, Types.word__size)
                                            in
                                            (* not tail-recursive due to try! *)
                                            constrain_qt target_bytes qtarget
                                        | _ ->
                                            return ()
                                    end
                                with
                                    | Failure "Dereference a dangling pointer" -> perform
                                        (* a null pointer *)
                                        annot qt "null";
                                        return ()
                                    | Failure _ ->
                                        (* uninitialized and unused pointer *)
                                        if MemOp.same_bytes bytes MemOp.bytes__zero then perform
                                            (* is definitely a null pointer *)
                                            annot qt "null";
                                            return ()
                                        else
                                            (* TODO: otherwise, nonnull? *)
                                            return ()
                                end
                            | _ ->
                                return ()
                        in
                        constrain_qt bytes qt
                    in
                    perform
                        (* first, the return value *)
                        begin match retvalopt with
                            | None ->
                                return ()
                            | Some retval ->
                                constrain_qt retval qtr
                        end;

                        (* then, global variables *)
                        Types.VarinfoMap.fold begin fun v block expM -> perform
                            expM;
                            let qt = Env.find (CilVar v) env in
                            let bytes = MemOp.state__get_bytes_from_block state block in
                            match qt with
                                (* first, take off one Ref to match the bytes representation *)
                                (* TODO: this should be done in the monad to account for mismatched types *)
                                | Ref (_, qt) -> constrain_qt bytes qt
                                | _ -> failwith "Impossible!"
                        end state.Types.global.Types.varinfo_to_block (return ())
                | _ ->
                    failwith "TODO: handle other completion values"
            end completed in

            (* update the constraints and return *)
            k (run expM expState)
        in

        (* dispatch *)
        dispatch (`SymbolicBlock (file, job, return))


    let dispatch chain dispatch = function
        | `TypedBlock (file, fn, expState, k) when S.should_enter_block fn.Cil.svar.Cil.vattr ->
            switch dispatch file fn expState k
        | work ->
            chain work
end

