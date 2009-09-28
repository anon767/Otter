open CilQual.Environment.CilFieldOrVar


open TypedBlock.G.QualType.Qual
open TypedBlock.G.QualType
open TypedBlock.G
open TypedBlock.GOps
open TypedBlock.DiscreteSolver
open TypedBlock


module Switcher (T : Config.BlockConfig)  (S : Config.BlockConfig) = struct

    let switch dispatch file fn (((((((), constraints), _), _), variant_table), _) as expState) k =
        Format.eprintf "Switching from typed to symbolic...@.";

        (* first, solve the typed constraints, needed to setup the symbolic constraints *)
        let solution = DiscreteSolver.solve consts constraints in

        (* TODO: properly explain error *)
        if Solution.is_unsatisfiable solution then
            Format.eprintf "Unsatisfiable solution entering TypedSymbolic.switch@.";

        let drop_qt = function
            (* take off one Ref to match the bytes representation *)
            (* TODO: handle function pointers *)
            | Ref (_, qt) -> qt
            | _ -> failwith "TODO: report drop_qt of non-variable"
        in

        let constrain_bytes state bytes qv typ =
            if Solution.equal_const qv "null" solution then
                let null_constraint = Types.Bytes_Op (Types.OP_EQ, [(bytes, typ); (MemOp.bytes__zero, typ)]) in
                MemOp.state__add_path_condition state null_constraint false
            else if Solution.equal_const qv "nonnull" solution then
                let nonnull_constraint = Types.Bytes_Op (Types.OP_NE, [(bytes, typ); (MemOp.bytes__zero, typ)]) in
                MemOp.state__add_path_condition state nonnull_constraint false
            else
                state
        in

        let qt_to_bytes expState state block_type typ qt =
            let rec qt_to_bytes expState state block_type typ qt = match typ, qt with
                (* first handle qualified types *)

                | Cil.TPtr (typtarget, _), Ref (Var v, qtarget) ->
                    (* for pointers, recursively deref and generate point-to blocks *)
                    let expState, state, target_bytes =
                        qt_to_bytes expState state Types.Block_type_Global typtarget qtarget
                    in
                    let target_block = MemOp.block__make
                        (Var.printer Format.str_formatter v; Format.flush_str_formatter ())
                        (MemOp.bytes__length target_bytes)
                        block_type
                    in
                    let state = MemOp.state__add_block state target_block target_bytes in
                    let bytes = Types.Bytes_Address (Some target_block, MemOp.bytes__zero) in
                    let state = constrain_bytes state bytes v typ in
                    expState, state, bytes

                | Cil.TComp (compinfo, _), Base (Var v) when compinfo.Cil.cstruct ->
                    (* for structs, initialize and iterate over the fields *)
                    let size = (Cil.bitsSizeOf (typ)) / 8 in
                    let size = if size <= 0 then 1 else size in
                    let bytes = MemOp.bytes__symbolic size in
                    let state = constrain_bytes state bytes v typ in
                    List.fold_left begin fun (expState, state, bytes) f ->
                        (* force f to be initialized, since they may have type qualifiers *)
                        let (_, env) as expState = run (perform get_field qt f; return ()) expState in

                        let expState, state, field_bytes =
                            qt_to_bytes expState state block_type f.Cil.ftype (drop_qt (Env.find (CilField f) env))
                        in

                        let offset = Convert.lazy_int_to_bytes (Eval.field_offset f) in
                        let size = MemOp.bytes__length field_bytes in
                        let bytes = MemOp.bytes__write bytes offset size field_bytes in
                        (expState, state, bytes)
                    end (expState, state, bytes) compinfo.Cil.cfields

                | typ, Base (Var v) ->
                    let size = (Cil.bitsSizeOf (typ)) / 8 in
                    let size = if size <= 0 then 1 else size in
                    let bytes = MemOp.bytes__symbolic size in
                    let state = constrain_bytes state bytes v typ in
                    expState, state, bytes

                (* TODO: handle extended types, e.g., void * and unions *)
                | _, _ ->
                    failwith "TODO: handle function pointers? mismatched types?"
            in
            qt_to_bytes expState state block_type typ qt
        in

        (* figure out the return value and arguments qualtype *)
        let _, env = expState in
        let qtr, qta = match Env.find (CilVar fn.Cil.svar) env with
            | Fn (_, qtr, qta) -> (qtr, qta)
            | _ -> failwith "Impossible!"
        in

        (* convert a typed environment into a symbolic environment *)
        let state = MemOp.state__empty in

        (* first, setup global variables *)
        let expState, state = List.fold_left begin fun (expState, state) g -> match g with
            | Cil.GVarDecl (v, _) | Cil.GVar (v, _, _)
                    when not (Types.VarinfoMap.mem v state.Types.global.Types.varinfo_to_block) ->
                let expState, state, bytes =
                    (* force v to be initialized, since they may have type qualifiers *)
                    let (_, env) as expState = run (perform lookup_var v; return ()) expState in
                    qt_to_bytes expState state Types.Block_type_Global v.Cil.vtype (drop_qt (Env.find (CilVar v) env))
                in
                let state = MemOp.state__add_global state v bytes in
                (expState, state)
            | _ ->
                (expState, state)
        end (expState, state) file.Cil.globals in

        (* then, setup the function arguments *)
        let expState, state, args = List.fold_right2 begin fun v a (expState, state, args) ->
            (* TODO: handle varargs *)
            let expState, state, bytes = qt_to_bytes expState state Types.Block_type_Local v.Cil.vtype a in
            (expState, state, bytes::args)
        end fn.Cil.sformals qta (expState, state, []) in

        (* next, prepare the function call job *)
        let job = Executemain.job_for_function state fn args in

        (* finally, prepare the return continuation *)
        let return completed =
            let completed_count = List.length completed in
            Format.eprintf "Returning from symbolic to typed (%d execution%s returned)...@."
                completed_count (if completed_count == 1 then "" else "s"); 

            (* prepare a monad that represents the symbolic result *)
            let expM = mapM_ begin function
                | Types.Return (retvalopt, { Types.result_state=state; Types.result_history=history }) ->
                    let bytes_to_qt typ bytes qt =
                        let rec bytes_to_qt typ bytes qt = match typ, qt with
                            | Cil.TPtr (typtarget, _), Ref (Var v, qtarget) ->
                                (* unconstrained pointers should not be touched; pointers that are unannotated or are
                                 * never assigned to/from annotated pointers are unconstrained and can accept any
                                 * value. Roughly speaking, it's as if the solution for unconstrained pointers is top
                                 * (nullable), which is implicitly added to the discrete partial order. *)
                                let annot qt c =
                                    if Solution.is_constrained v solution then perform annot qt c; return ()
                                    else return ()
                                in
                                begin try perform
                                    let target_lvals = Eval.deref state bytes in
                                    (* didn't fail, so is not a null pointer *)
                                    annot qt "nonnull";
                                    begin match qtarget with
                                        | Ref _ ->
                                            let rec recurse = function
                                                | Types.Lval_Block (block, offset) ->
                                                    let target_bytes = MemOp.state__get_bytes_from_lval state
                                                        (block, offset, Types.word__size)
                                                    in
                                                    (* not tail-recursive! *)
                                                    bytes_to_qt typtarget target_bytes qtarget
                                                | Types.Lval_May (indicator, lval1, lval2) -> perform
                                                    recurse lval1;
                                                    recurse lval2
                                            in
                                            recurse target_lvals
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

                            | Cil.TComp (compinfo, _), Base (Var v) when compinfo.Cil.cstruct -> perform
                                (* for structs, iterate over the fields;
                                 * note that the monad is composed right-to-left, but executed left-to-right *)
                                List.fold_right begin fun f expM -> perform
                                    let offset = Convert.lazy_int_to_bytes (Eval.field_offset f) in
                                    let size = (Cil.bitsSizeOf (f.Cil.ftype)) / 8 in
                                    let field_bytes = MemOp.bytes__read bytes offset size in
                                    bytes_to_qt f.Cil.ftype field_bytes (drop_qt (Env.find (CilField f) env));
                                    expM;
                                end compinfo.Cil.cfields (return ())

                            | typ, Base (Var v) ->
                                return ()

                            (* TODO: handle extended types, e.g., void * and unions *)
                            | _, _ ->
                                failwith "TODO: handle function pointers? mismatched types?"
                        in
                        bytes_to_qt typ bytes qt
                    in

                    perform
                        (* first, the return value *)
                        begin match retvalopt with
                            | None ->
                                return ()
                            | Some retval ->
                                let rettyp = match fn.Cil.svar.Cil.vtype with
                                    | Cil.TFun (rettyp, _, _, _) -> rettyp
                                    | _ -> failwith "Impossible!"
                                in
                                bytes_to_qt rettyp retval qtr
                        end;

                        (* then, the function arguments (for output arguments) *)
                        (* TODO: handle varargs *)
                        let rec args_bytes_to_qt vs bs qs = match vs, bs, qs with
                            | v::vs, b::bs, q::qs -> perform
                                bytes_to_qt v.Cil.vtype b q; (args_bytes_to_qt vs bs qs)
                            | _, _, _ ->
                                return ()
                        in
                        args_bytes_to_qt fn.Cil.sformals args qta;

                        (* then, global variables *)
                        Types.VarinfoMap.fold begin fun v block expM -> perform
                            expM;
                            let bytes = MemOp.state__get_bytes_from_block state block in
                            bytes_to_qt v.Cil.vtype bytes (drop_qt (Env.find (CilVar v) env))
                        end state.Types.global.Types.varinfo_to_block (return ())

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
        dispatch (`SymbolicBlock (file, job, return))


    let dispatch chain dispatch = function
        | `TypedBlock (file, fn, expState, k) when S.should_enter_block fn.Cil.svar.Cil.vattr ->
            switch dispatch file fn expState k
        | work ->
            chain work
end

