open CilQual.Environment.CilFieldOrVar

open TypedBlock.G.QualType.Qual
open TypedBlock.G.QualType
open TypedBlock.G
open TypedBlock.GOps
open TypedBlock.DiscreteSolver
open TypedBlock


open SwitchingUtil


module Switcher (S : Config.BlockConfig)  (T : Config.BlockConfig) = struct

    let switch dispatch file job k =
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
            state <-- frame_bytes_to_qt MemOp.state__empty state state.Types.global;
            state <-- frame_bytes_to_qt MemOp.state__empty state (List.hd state.Types.formals);
            return state
        in
        let (((((state, _), _), _), _), _ as expState) = run expM expState in


        (* prepare the completion continuation to perform the final check *)
        let completion (((((_, constraints), _), _), _), _ as expState) =
            let solution = DiscreteSolver.solve consts constraints in

            (* TODO: properly explain error *)
            if DiscreteSolver.Solution.is_unsatisfiable solution then
                Format.eprintf "Unsatisfiable solution in SymbolicTyped.completion@.";

            (* first, the return value *)
            let state, retopt = begin match fn.Cil.svar.Cil.vtype with
                | Cil.TFun (rettyp, _, _, _) when Cil.isVoidType rettyp ->
                    (state, None)

                | Cil.TFun (rettyp, _, _, _) ->
                    let (((((qtr, _), _), _), _), _) = run begin perform
                        qtf <-- lookup_var fn.Cil.svar;
                        retval qtf
                    end expState in
                    let state, retbytes = qt_to_bytes expState solution state Types.Block_type_Local rettyp qtr in
                    (state, Some retbytes)

                | _ ->
                    failwith "Impossible!"
            end in

            (* then, the globals and call stack *)
            let state = List.fold_left begin fun state frame ->
                frame_qt_to_bytes expState solution state frame Types.Block_type_Local
            end state (state.Types.global::(state.Types.formals @ state.Types.locals)) in

            k [ Types.Return (retopt, { Types.result_state=state; Types.result_history=Types.emptyHistory }) ]
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

