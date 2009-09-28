

module Interpreter (S : Config.BlockConfig) = struct

    (* Cil setup *)
    let init_cil () = ()

    let prepare_file file =
        let verbose = !Errormsg.verboseFlag in
        (* Suppress most output from the symbolic executor *)
        Executeargs.print_args.Executeargs.arg_print_reg <- verbose;
        Executeargs.print_args.Executeargs.arg_print_ifstmt <- verbose;
        Executeargs.print_args.Executeargs.arg_print_misc <- verbose;
        Executeargs.print_args.Executeargs.arg_print_stmt <- verbose;
        Executeargs.print_args.Executeargs.arg_print_func <- verbose;
        Executeargs.print_args.Executeargs.arg_print_assign <- verbose;
        Executemain.prepare_file file


    let should_delegate_call job = match job.Types.instrList with
        | [ Cil.Call(lvalopt, Cil.Lval f, args, _) ] ->
            begin match f with
                | Cil.Var v, Cil.NoOffset ->
                    if S.should_enter_block v.Cil.vattr then
                        None
                    else
                        Some (lvalopt, Cilutility.search_function v, args)
                | Cil.Mem e, Cil.NoOffset ->
                    begin match Eval.rval job.Types.state e with
                        | Types.Bytes_FunPtr (fn, _) ->
                            if S.should_enter_block fn.Cil.svar.Cil.vattr then
                                None
                            else
                                Some (lvalopt, fn, args)
                        | _ ->
                            failwith "TODO: report unsupported non-constant function pointer"
                    end
                | _, _ ->
                    failwith "Does Cil generate calls through (_, offset)?"
            end
        | _ ->
            None


    let call dispatch file job k =
        Format.eprintf "Evaluating symbolic block...@.";

        let rec symbolic_loop completed job job_pool = match should_delegate_call job with
            | Some (lvalopt, fn, args) ->
                let destopt = match lvalopt with
                    | None -> None
                    | Some lval ->
                        let lvals = Eval.lval job.Types.state lval in
                        let size = (Cil.bitsSizeOf (Cil.typeOfLval lval))/8 in
                        Some (lvals, size)
                in

                (* prepare the return continuation *)
                let return = function
                    | [ Types.Return (retvalopt, { Types.result_state=state; Types.result_history=history }) ] ->
                        let state = match destopt, retvalopt with
                            | Some dest, Some retval ->
                                MemOp.state__assign state dest retval
                            | _, _ ->
                                state
                        in
                        symbolic_loop completed { job with
                            Types.state=state;
                            Types.exHist=history;
                            Types.instrList=[];
                        } job_pool
                    | _ ->
                        failwith "TODO: handle other completion values"
                in

                (* perform the function call symbolically *)
                let args = List.map (fun e -> Eval.rval job.Types.state e) args in
                let job = { job with
                    Types.state = MemOp.state__start_fcall job.Types.state Types.Runtime fn args;
                    Types.instrList = [];
                    Types.stmt = List.hd fn.Cil.sallstmts;
                    Types.inTrackedFn = false;
                } in

                dispatch (`SymbolicBlock (file, job, return))

            | None ->
                let result, job_pool = Driver.step_job job job_pool in
                begin match result with
                    | Types.Active job ->
                        symbolic_loop completed job job_pool
                    | Types.Fork (j1, j2) ->
                        let job_pool = Driver.queue_job j1 job_pool in (* queue the true branch *)
                        symbolic_loop completed j2 job_pool (* continue the false branch *)
                    | Types.Complete completion ->
                        (* store the result and pick another job to continue, if available *)
                        let completed = completion::completed in
                        match Driver.pick_job job_pool with
                            | Some (job, job_pool) ->
                                symbolic_loop completed job job_pool
                            | None ->
                                k completed
                end
        in
        symbolic_loop [] job ([], Types.JobSet.empty)


    let exec file args =
        let job = Executemain.job_for_file file (file.Cil.fileName::args) in
        `SymbolicBlock (file, job, (fun x -> x))


    let dispatch chain dispatch = function
        | `SymbolicBlock (file, job, k) when should_delegate_call job = None ->
            call dispatch file job k
        | work ->
            chain work
end

