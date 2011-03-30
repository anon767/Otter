open DataStructures
open OcamlUtilities
open Cil
open CilUtilities
open OtterBytes
open Bytes
open BytesUtility
open State
open Job
open CoverageData


let stmtInfo_of_job job =
    { siFuncName = (List.hd job#state.callstack).svar.vname;
        siStmt = Coverage.stmtAtEndOfBlock job#stmt; }

(** Return a new exHist with
        - block coverage updated by the addition of the statement at the
            end of the block containing [job#stmt]
        - line coverage is updated if [job#stmt] is an if, return, goto,
            or loop
        - if nextStmtOpt is [Some s] and [job#stmt] is the end of a block,
            edge coverage is updated by the addition of the edge from
            [job#stmt] to the end of [s]'s block
        - whichBranch tells us which branch we are taking for condition
            coverage, which only cares about [If] statements. whichBranch is
            ignored for other types of coverage, and it is entirely ignored
            if [job#stmt] is not an [If]. (Really, it might be better if
            whichBranch were a bool option, so we could pass in [None] if
            weren't at an [If], but this is more convenient.)
        Each form of coverage is only updated if it was requested. *)
let addStmtCoverage job whichBranch nextStmtOpt = Profiler.global#call "Statement.addStmtCoverage" begin fun () ->
    { job#exHist with
            coveredLines =
                if !Executeargs.arg_line_coverage
                then match job#stmt.skind with
                        If(_,_,_,loc)
                    | Cil.Return(_,loc)
                    | Goto(_,loc)
                    | Loop(_,loc,_,_) ->
                            LineSet.add (loc.Cil.file,loc.Cil.line) job#exHist.coveredLines
                    | _ -> job#exHist.coveredLines
                else LineSet.empty;
            coveredBlocks =
                if !Executeargs.arg_block_coverage
                then StmtInfoSet.add (stmtInfo_of_job job) job#exHist.coveredBlocks
                else StmtInfoSet.empty;
            coveredEdges =
                if !Executeargs.arg_edge_coverage
                then (
                    match nextStmtOpt with
                            Some nextStmt when job#stmt == Coverage.stmtAtEndOfBlock job#stmt ->
                                    let funcName = (List.hd job#state.callstack).svar.vname in
                                    EdgeSet.add
                                        ({ siFuncName = funcName; siStmt = job#stmt; },
                                         { siFuncName = funcName;
                                             siStmt = Coverage.stmtAtEndOfBlock nextStmt; })
                                        job#exHist.coveredEdges
                        | _ -> job#exHist.coveredEdges
                ) else EdgeSet.empty;
            coveredConds =
                if !Executeargs.arg_cond_coverage
                then (
                        match job#stmt.skind with
                                If _ -> CondSet.add (stmtInfo_of_job job,whichBranch) job#exHist.coveredConds
                        | _ -> job#exHist.coveredConds
                ) else CondSet.empty;
            executionPath =
                if !Executeargs.arg_path_coverage && job#stmt == Coverage.stmtAtEndOfBlock job#stmt
                then (
                    { siFuncName = (List.hd job#state.callstack).svar.vname; siStmt = job#stmt; } :: job#exHist.executionPath
                ) else (
                    job#exHist.executionPath
                )
    }
end

let addInstrCoverage job instr = Profiler.global#call "Statement.addInstrCoverage" begin fun () ->
    let instrLoc = get_instrLoc instr in
    { job#exHist with coveredLines =
            LineSet.add (instrLoc.Cil.file,instrLoc.Cil.line) job#exHist.coveredLines; }
end

let exec_fundec job instr fundec lvalopt exps errors = Profiler.global#call "Statement.exec_fundec" begin fun () ->
    (* TODO: Profiler.start_fcall job fundec *)

    let stmt = job#stmt in

    (* evaluate the arguments *)
    let job, argvs, errors = List.fold_right begin fun exp (job, argvs, errors) ->
        let job, bytes, errors = Profiler.global#call "Statement.exec_fundec/rval" begin fun () ->
            Expression.rval job exp errors 
    end in
        (job, bytes::argvs, errors)
    end exps (job, [], errors) in

    (* [stmt] is an [Instr], so it can't have two successors. If
     [func] returns, then [stmt] has exactly one successor. If
     [func] is [exit] or has the [noreturn] attribute, [stmt]
     has no successor. *)
    let callContext = match stmt.succs with
        | []  -> NoReturn (stmt,instr)
        | [h] -> Source (lvalopt,stmt,instr,h)
        | _   -> assert false
    in
    let job = MemOp.state__start_fcall job callContext fundec argvs in

    (*
    (* If fundec is the function to be examined *)
    if Executeargs.run_args.arg_examfn = fundec.svar.vname then
        InvInput.examine state fundec;
    *)

    (* Update the state, the next stmt to execute, and whether or
     not we're in a tracked function. *)
    let job = job#with_stmt (List.hd fundec.sallstmts) in
    let job = job#with_instrList [] in
    let job = job#with_inTrackedFn (StringSet.mem fundec.svar.vname job#trackedFns) in
    (Active job, errors)
end

let exec_instr_call job instr lvalopt fexp exps errors = Profiler.global#call "Statement.exec_instr_call" begin fun () ->
    match fexp with
      | Lval(Cil.Var(varinfo), Cil.NoOffset) ->
            let fundec =
                try FindCil.fundec_by_name job#file varinfo.vname
                with Not_found -> FormatPlus.failwith "Function %s not found." varinfo.vname
            in
            let job = job#with_decision_path (DecisionPath.add (Decision.DecisionFuncall(instr, varinfo)) job#decision_path) in
            exec_fundec job instr fundec lvalopt exps errors
      | Lval(Cil.Mem _, _) ->
            FormatPlus.failwith "Function pointer in Statement.exec_instr:\n%a\nPlease use Interceptor.function_pointer_interceptor" Printcil.instr instr
      | _ ->
            FormatPlus.failwith "Bad function call:\n%a" Printcil.instr instr
end

let exec_instr job errors = Profiler.global#call "Statement.exec_instr" begin fun () ->
    assert (job#instrList <> []);
    let printInstr instr =
        Output.set_mode Output.MSG_STMT;
        Output.printf "@[%a@]@." Printcil.instr instr
    in

    let instr, tail = match job#instrList with i::tl -> (i, tl) | _ -> assert false in

    (* Within instructions, we have to update line coverage (but not
         statement or edge coverage). *)
    let job =
        if job#inTrackedFn && !Executeargs.arg_line_coverage
        then job#with_exHist (addInstrCoverage job instr)
        else job
    in

    (* Since we've used the makeCFGFeature, an [Instr] is a series of
       [Set]s and [Asm]s, possibly terminated with a [Call]. *)
    match instr with
         | Set(cil_lval, exp, loc) ->
            printInstr instr;
            let job, lval, errors = Expression.lval job cil_lval errors in
            let job, rv, errors = Expression.rval job exp errors in
            let job = MemOp.state__assign job lval rv in
            let job = job#with_stmt (if tail = [] then List.hd job#stmt.succs else job#stmt) in
            let job = job#with_instrList tail in
            (Active job, errors)
        | Call(lvalopt, fexp, exps, loc) ->
            assert (tail = []);
            printInstr instr;
            exec_instr_call job instr lvalopt fexp exps errors
        | Asm _ ->
            (Complete (Abandoned (`Failure "Cannot handle assembly", job)), errors)
end


let exec_stmt job errors = Profiler.global#call "Statement.exec_stmt" begin fun () ->
    assert (job#instrList = []);
    let stmt = job#stmt in

    let nextExHist ?(whichBranch=false) nextStmtOpt =
        if job#inTrackedFn
        then addStmtCoverage job whichBranch nextStmtOpt
        else job#exHist
    in

    (* Print "Enter function ..." if it's the first statement of the function *)
    (* FIXME: That means, if there's a label at the very beginning of the function, then
     *        every time we go back to the first statement via goto, "Enter function ..."
     *        will be printed, which can be confusing.
     *)
    begin if stmt.sid = 0 then
        let fundec = List.hd job#state.callstack in
        Output.set_mode Output.MSG_FUNC;
        Output.printf "@[Enter function %a@]@." CilPrinter.fundec fundec;
    end;

    Output.set_mode Output.MSG_STMT;
    Output.printf "@[%a@]@." CilPrinter.stmt_abbr stmt;
    match stmt.skind with
        | Instr [] ->
             let nextStmt = match stmt.succs with [x] -> x | _ -> assert false in
             (Active ((job#with_stmt nextStmt)#with_exHist (nextExHist (Some nextStmt))), errors)
        | Instr (instrs) ->
            (* We can certainly update block coverage here, but not
             * necessarily edge coverage. If instrs contains a call, we
             * don't know for certain that we'll traverse the edge from this
             * statement to its successor: the call might never return. So
             * if there is a call, we postpone (by passing None as the next
             * stmt) recording the edge until the return. (Line coverage for
             * the instructions themselves are handled in exec_instr.) *)
            let nextStmtOpt =
                if List.exists (function Call _ -> true | _ -> false) instrs
                then None
                else Some (match stmt.succs with [x] -> x | _ -> assert false)
            in
            (Active ((job#with_instrList instrs)#with_exHist (nextExHist nextStmtOpt)), errors)
        | Cil.Return (expopt, loc) ->
            begin match job#state.callContexts with
                | Runtime::_ -> (* completed symbolic execution (e.g., return from main) *)
                    let job, retval, errors = match expopt with
                        | None ->
                            (job, None, errors)
                        | Some exp ->
                            let job, retval, errors = Expression.rval job exp errors in
                            (job, Some retval, errors)
                    in
                    let job = job#with_exHist (nextExHist None) in
                    Output.set_mode Output.MSG_REPORT;
                    Output.printf "Program execution finished.@.";
                    (Complete (Return (retval, job)), errors)
                | (Source (destOpt,callStmt,_,nextStmt))::_ ->
                        let job, errors =
                            match expopt, destOpt with
                                | Some exp, Some dest ->
                                    (* evaluate the return expression in the callee frame *)
                                    let job, rv, errors = Expression.rval job exp errors in
                                    (* TODO: Profiler.end_fcall job *)
                                    let job = MemOp.state__end_fcall job in
                                    (* evaluate the assignment in the caller frame *)
                                    let job, lval, errors = Expression.lval job dest errors in
                                    (MemOp.state__assign job lval rv, errors)
                                | _, _ ->
                                     (* If we are not returning a value, or if we
                                        ignore the result, just end the call *)
                                    (* TODO: Profiler.end_fcall job *)
                                    (MemOp.state__end_fcall job, errors)
                        in

                        let callingFuncName = (List.hd job#state.callstack).svar.vname in
                        let inTrackedFn = StringSet.mem callingFuncName job#trackedFns in

                        (* When a function returns, we have to record the
                           coverage within the returning function and also the
                           *edge* in the calling function from the call
                           instruction's block to the next block. *)
                        let exHist =
                            (* First, record coverage for the returning function: *)
                            let exHist = nextExHist None in (* [None] because we don't currently track returns as edges for purposes of coverage *)

                            (* Now record the edge in the calling function. We
                                 can't use nextExHist because the edge we want
                                 is 'from the past' rather than 'into the
                                 future'; so we have to check whether we should
                                 in fact record coverage. *)
                            if inTrackedFn && !Executeargs.arg_edge_coverage then
                                let coveredEdges = EdgeSet.add
                                    ({ siFuncName = callingFuncName; siStmt = callStmt; }, (* A call ends a block, so use callStmt directly *)
                                        { siFuncName = callingFuncName; siStmt = Coverage.stmtAtEndOfBlock nextStmt })
                                    exHist.coveredEdges
                                in
                                { exHist with coveredEdges = coveredEdges; }
                            else
                                exHist
                        in

                        let job = job#with_stmt nextStmt in
                        let job = job#with_inTrackedFn inTrackedFn in
                        let job = job#with_exHist exHist in
                        (Active job, errors)

                | (NoReturn _)::_ ->
                        failwith "Return from @noreturn function"
                | [] ->
                        (* there should always be a Runtime at the start of the list *)
                        assert false
            end
        | Goto (stmtref, loc) ->
            (Active ((job#with_stmt !stmtref)#with_exHist (nextExHist (Some !stmtref))), errors)
        | If (exp, block1, block2, loc) ->
            begin
            (* try a branch *)
                let try_branch job pcopt block =
                    let job = match pcopt with
                        | Some(pc) -> MemOp.state__add_path_condition job pc true
                        | None -> job
                    in
                    let nextStmt = match stmt.succs with
                            [succ] -> succ (* This happens for 'if (...);', with nothing on either branch *)
                        | [succF;succT] -> (* The successors are in reverse order: false then true *)
                                (* Just making sure I understand this correctly *)
                                assert ((block1.bstmts = [] || List.hd block1.bstmts == succT) &&
                                                    (block2.bstmts = [] || List.hd block2.bstmts == succF));
                                if block == block1 then succT else succF
                        | _ -> assert false (* Impossible: there must be 1 or 2 successors *)
                    in
                    job#with_stmt nextStmt
                in

                let job, rv, errors = Expression.rval job exp errors in

                Output.set_mode Output.MSG_GUARD;
                Output.printf "Check if the following holds:@\n  @[%a@]@\n" BytesPrinter.bytes rv;
                Output.printf "Under the path condition:@\n";
                if PathCondition.is_empty job#state.path_condition then
                    Output.printf "  (nil)"
                else
                    Output.printf "  @[%a@]" (FormatPlus.pp_print_list BytesPrinter.bytes "@ AND ") (PathCondition.clauses job#state.path_condition);
                Output.printf "@.";

                let truth = MemOp.eval job#state.path_condition rv in
                Output.set_mode Output.MSG_REG;
                let job_state = match truth with
                    | Ternary.True ->
                        Output.printf "True@.";
                        let job = try_branch job None block1 in
                        let job = job#with_decision_path (DecisionPath.add (Decision.DecisionConditional(stmt, true)) job#decision_path) in
                        let job = job#with_exHist (nextExHist (Some job#stmt) ~whichBranch:true) in
                        Active job

                    | Ternary.False ->
                        Output.printf "False@.";
                        let job = try_branch job None block2 in
                        let job = job#with_decision_path (DecisionPath.add (Decision.DecisionConditional(stmt, false)) job#decision_path) in
                        let job = job#with_exHist  (nextExHist (Some job#stmt) ~whichBranch:false) in
                        Active job

                    | Ternary.Unknown ->
                        Output.printf "Unknown@.";

                        (* Create two jobs, one for each branch. The false branch
                           inherits the old jid, and the true job gets a new jid. *)
                        let false_job, true_job =
                            (job : #Info.t)#fork2
                                begin fun job ->
                                    (* false *)
                                    let job = try_branch job (Some (logicalNot rv)) block2 in
                                    let job = job#with_decision_path (DecisionPath.add (Decision.DecisionConditional(stmt, false)) job#decision_path) in
                                    let job = job#with_exHist (nextExHist (Some job#stmt) ~whichBranch:false) in
                                    job
                                end
                                begin fun job ->
                                    (* true *)
                                    let job = try_branch job (Some rv) block1 in
                                    let job = job#with_decision_path (DecisionPath.add (Decision.DecisionConditional(stmt, true)) job#decision_path) in
                                    let job = job#with_exHist (nextExHist (Some job#stmt) ~whichBranch:true) in
                                    job
                                end
                        in

                        Output.set_mode Output.MSG_BRANCH;
                        Output.printf "@[Branching on @[%a@]@ at @[%a@].@]@." CilPrinter.exp exp Printcil.loc loc;
                        if !Executeargs.arg_print_callstack then
                            Output.printf "Call stack:@\n  @[%a@]@." (Printer.callingContext_list "@\n") job#state.callContexts;
                        Output.printf "Job %d is the true branch and job %d is the false branch.@\n@." true_job#path_id false_job#path_id;
                        Fork [Active true_job; Active false_job]
                in
                (job_state, errors)

            end
        | Block(block)
        | Loop (block, _, _, _) ->
            (* A [Loop]'s block always has a non-empty bstmts. (See
               Cil.succpred_stmt.)
               This is not true for [Block]s, but it *does* seem to be
               true for [Block]s which are not under [If]s, so we're okay. *)
            let nextStmt = List.hd block.bstmts in
            (Active ((job#with_stmt nextStmt)#with_exHist (nextExHist (Some nextStmt))), errors)
        | _ -> failwith "Not implemented yet"
end


let errors_to_abandoned_list errors =
    List.map begin fun (job, error) ->
        Complete (Abandoned (error, job))
    end errors


let step job job_queue = Profiler.global#call "Statement.step" begin fun () ->
    try
        let job_state, errors = match job#instrList with
            | [] -> exec_stmt job []
            | _ -> exec_instr job []
        in
        let abandoned_job_states = errors_to_abandoned_list errors in
        (Fork (job_state::abandoned_job_states), job_queue)

    with
        | Failure msg -> begin
            Output.must_printf "Statement.step: failwith %s@." msg;
            if !Executeargs.arg_failfast then failwith msg;
            (Complete (Abandoned (`Failure msg, job)), job_queue)
        end
end
