

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


    let call dispatch file job k =
        let fn = List.hd job.Types.state.Types.callstack in
        Format.eprintf "Evaluating symbolic block %s...@." fn.Cil.svar.Cil.vname;

        let rec symbolic_loop completed job job_queue =

            let complete_job result =
                (* store the result and pick another job to continue, if available *)
                let completed = result::completed in
                match job_queue with
                    | job::job_queue ->
                        symbolic_loop completed job job_queue
                    | [] ->
                        k completed
            in

            if S.should_enter_block (List.hd job.Types.state.Types.callstack).Cil.svar.Cil.vattr then begin
                (* execute this function *)

                let state = Driver.step_job job in
                begin match state with
                    | Types.Active job ->
                        symbolic_loop completed job job_queue
                    | Types.Fork (j1, j2) ->
                        let job_queue = j1::job_queue in (* queue the true branch *)
                        symbolic_loop completed j2 job_queue (* continue the false branch *)
                    | Types.Complete result ->
                        complete_job (result, None)
                end

            end else begin
                (* delegate this function *)

                (* prepare the completion continuation *)
                let completion = function
                    | [ (Types.Return (retopt, { Types.result_state=state; Types.result_history=history }),
                         None) as result ] ->
                        begin match List.hd state.Types.callContexts with
                            | Types.Runtime ->
                                (* occurs when main() is not symbolic, so there's nothing left to execute *)
                                complete_job result

                            | Types.Source (destopt, _, _, nextstmt) ->
                                (* end the function call and continue executing *)
                                let state = MemOp.state__end_fcall state in
                                let state = match destopt, retopt with
                                    | Some dest, Some ret ->
                                        let state, lval = Eval.lval state dest in
                                        MemOp.state__assign state lval ret
                                    | _, _ ->
                                        state
                                in
                                symbolic_loop completed { job with
                                    Types.state=state;
                                    Types.stmt=nextstmt;
                                    Types.exHist=history;
                                    (* TODO: update inTrackFn and coverage? *)
                                } job_queue

                            | Types.NoReturn _ ->
                                failwith "TODO: handle return from @noreturn"
                        end

                    | [ (Types.Abandoned _, _) as result ] ->
                        complete_job result

                    | _ ->
                        failwith "TODO: handle other completion values"
                in
                dispatch (`SymbolicBlock (file, job, completion))
            end
        in
        symbolic_loop [] job []


    let exec file args =
        let job = Executemain.job_for_file file (file.Cil.fileName::args) in

        let completion results =
            Format.eprintf "Completing symbolic block...@.";

            (* report jobs that were abandoned *)
            let abandoned = List.fold_left begin fun abandoned result -> match result with
                | Types.Abandoned (s, loc, _), block_errors ->
                    (s, loc, block_errors)::abandoned
                | _, _ ->
                    abandoned
            end [] results in

            if abandoned != [] then begin
                let printer ff abandoned = ignore begin List.iter begin fun (s, l, b) ->
                    Format.fprintf ff "@[%s:%d: %s@]@\n" l.Cil.file l.Cil.line s;
                end abandoned end in

                Format.eprintf "@.";
                Format.eprintf "Abandoned paths:@\n  @[%a@]@." printer abandoned;

                let count = List.length abandoned in
                Format.eprintf "%d path%s abandoned in SymbolicBlock.exec:@."
                    count (if count == 1 then "" else "s");
            end;

            (file, results)
        in
        `SymbolicBlock (file, job, completion)


    let dispatch chain dispatch = function
        | `SymbolicBlock (file, job, k)
                when S.should_enter_block (List.hd job.Types.state.Types.callstack).Cil.svar.Cil.vattr ->
            call dispatch file job k
        | work ->
            chain work
end

