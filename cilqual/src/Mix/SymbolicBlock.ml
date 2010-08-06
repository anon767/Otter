open Otter


module Interpreter (S : Config.BlockConfig) = struct

    (* Cil setup *)
    let init_cil () = ()

    let prepare_file file =
        let verbose = !Errormsg.verboseFlag in
        (* Suppress most output from the symbolic executor *)
        Output.arg_print_reg := verbose;
        Output.arg_print_stmt := verbose;
        Output.arg_print_func := verbose;
        Output.arg_print_assign := verbose;
        Driver.prepare_file file


    let call dispatch stack file job k =
        let fn = List.hd job.Types.state.Types.callstack in
        Format.eprintf "Evaluating symbolic block %s...@." fn.Cil.svar.Cil.vname;

        let rec symbolic_loop stack completed job job_queue =

            let next_job stack completed = function
                | job::job_queue ->
                    (* pick another job to continue, if available *)
                    symbolic_loop stack completed job job_queue
                | [] ->
                    k stack completed
            in

            if S.should_enter_block (List.hd job.Types.state.Types.callstack).Cil.svar.Cil.vattr then begin
                (* execute this function *)

                let state, job_queue = Builtin_function.interceptor job job_queue Core.step in
                let rec process_job_states completed job_queue = function
                    | Types.Active job ->
                        (completed, (job::job_queue))
                    | Types.Fork states ->
                        List.fold_left (fun (completed, job_queue) state -> process_job_states completed job_queue state) (completed, job_queue) states
                    | Types.Complete result ->
                        (((result, None)::completed), job_queue)
                    | _ ->
                        failwith "TODO: handle other results"
                in
                let completed, job_queue = process_job_states completed job_queue state in
                next_job stack completed job_queue

            end else begin
                (* delegate this function *)

                (* prepare the completion continuation *)
                let completion stack results =

                    let completed, job_queue = List.fold_left begin fun (completed, job_queue) result -> match result with
                        | (Types.Return (retopt, { Types.result_state=state; Types.result_history=history }), None) ->
                            begin match List.hd state.Types.callContexts with
                                | Types.Runtime ->
                                    (* occurs when main() is not symbolic, so there's nothing left to execute *)
                                    (result::completed, job_queue)

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
                                    let job = { job with
                                        Types.state=state;
                                        Types.stmt=nextstmt;
                                        Types.exHist=history;
                                        (* TODO: update inTrackFn and coverage? *)
                                    } in
                                    (completed, job::job_queue)

                                | Types.NoReturn _ ->
                                    failwith "TODO: handle return from @noreturn"
                            end

                        | (Types.Abandoned _, _) ->
                            (result::completed, job_queue)

                        | _ ->
                            failwith "TODO: handle other completion values"
                    end (completed, job_queue) results in

                    next_job stack completed job_queue
                in
                dispatch stack (`SymbolicBlock (file, job, completion))
            end
        in
        symbolic_loop stack [] job []


    let exec file args =
        let job = Driver.job_for_file file (file.Cil.fileName::args) in

        let completion stack results =
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


    let dispatch chain dispatch stack = function
        | `SymbolicBlock (file, job, k)
                when S.should_enter_block (List.hd job.Types.state.Types.callstack).Cil.svar.Cil.vattr ->
            call dispatch stack file job k
        | work ->
            chain stack work
end

