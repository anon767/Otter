open DataStructures
open OcamlUtilities
open Cil
open CilUtilities
open OtterBytes
open Bytes
open BytesUtility
open Types
open Job
open Decision


let stmtInfo_of_job job =
    { siFuncName = (List.hd job.state.callstack).svar.vname;
        siStmt = Coverage.stmtAtEndOfBlock job.stmt; }

(** Return a new exHist with
        - block coverage updated by the addition of the statement at the
            end of the block containing [job.stmt]
        - line coverage is updated if [job.stmt] is an if, return, goto,
            or loop
        - if nextStmtOpt is [Some s] and [job.stmt] is the end of a block,
            edge coverage is updated by the addition of the edge from
            [job.stmt] to the end of [s]'s block
        - whichBranch tells us which branch we are taking for condition
            coverage, which only cares about [If] statements. whichBranch is
            ignored for other types of coverage, and it is entirely ignored
            if [job.stmt] is not an [If]. (Really, it might be better if
            whichBranch were a bool option, so we could pass in [None] if
            weren't at an [If], but this is more convenient.)
        Each form of coverage is only updated if it was requested. *)
let addStmtCoverage job whichBranch nextStmtOpt =
    { job.exHist with
            coveredLines =
                if !Executeargs.arg_line_coverage
                then match job.stmt.skind with
                        If(_,_,_,loc)
                    | Cil.Return(_,loc)
                    | Goto(_,loc)
                    | Loop(_,loc,_,_) ->
                            LineSet.add (loc.Cil.file,loc.Cil.line) job.exHist.coveredLines
                    | _ -> job.exHist.coveredLines
                else LineSet.empty;
            coveredBlocks =
                if !Executeargs.arg_block_coverage
                then StmtInfoSet.add (stmtInfo_of_job job) job.exHist.coveredBlocks
                else StmtInfoSet.empty;
            coveredEdges =
                if !Executeargs.arg_edge_coverage
                then (
                    match nextStmtOpt with
                            Some nextStmt when job.stmt == Coverage.stmtAtEndOfBlock job.stmt ->
                                    let funcName = (List.hd job.state.callstack).svar.vname in
                                    EdgeSet.add
                                        ({ siFuncName = funcName; siStmt = job.stmt; },
                                         { siFuncName = funcName;
                                             siStmt = Coverage.stmtAtEndOfBlock nextStmt; })
                                        job.exHist.coveredEdges
                        | _ -> job.exHist.coveredEdges
                ) else EdgeSet.empty;
            coveredConds =
                if !Executeargs.arg_cond_coverage
                then (
                        match job.stmt.skind with
                                If _ -> CondSet.add (stmtInfo_of_job job,whichBranch) job.exHist.coveredConds
                        | _ -> job.exHist.coveredConds
                ) else CondSet.empty;
            executionPath =
                if !Executeargs.arg_path_coverage && job.stmt == Coverage.stmtAtEndOfBlock job.stmt
                then (
                    { siFuncName = (List.hd job.state.callstack).svar.vname; siStmt = job.stmt; } :: job.exHist.executionPath
                ) else (
                    job.exHist.executionPath
                )
    }

let addInstrCoverage job instr =
    let instrLoc = get_instrLoc instr in
    { job.exHist with coveredLines =
            LineSet.add (instrLoc.Cil.file,instrLoc.Cil.line) job.exHist.coveredLines; }

let function_from_exp job state exp args errors =
    match exp with
        | Lval(Var(varinfo), NoOffset) ->
            begin
                try
                    ([ (state, FindCil.fundec_by_varinfo job.file varinfo) ], errors)
                with Not_found ->
                    failwith ("Function "^varinfo.vname^" not found.")
            end


        | Lval(Mem(exp2), NoOffset) ->
            let state, bytes, errors  = Expression.rval state exp2 errors in
            let rec getall fp =
                let fold_func acc pre leaf =
                    match leaf with
                        | Bytes_FunPtr(varinfo,_) ->
                            (* the varinfo should always map to a valid fundec (if the file was parsed by Cil) *)
                            let state = MemOp.state__add_path_condition state (Bytes.guard__to_bytes pre) true in
                            let fundec = FindCil.fundec_by_varinfo job.file varinfo in
                            (state, fundec)::acc
                        | _ -> acc (* should give a warning here about a non-valid function pointer*)
                in
                Bytes.conditional__fold ~test:(Stp.query_stp state.path_condition) fold_func [] fp
            in
            begin match bytes with
                | Bytes_FunPtr(varinfo,_) ->
                    (* the varinfo should always map to a valid fundec (if the file was parsed by Cil) *)
                    let fundec = FindCil.fundec_by_varinfo job.file varinfo in
                    ([ (state, fundec) ], errors)
                | Bytes_Read(bytes2, offset, len) ->
                    let fp = (BytesUtility.expand_read_to_conditional bytes2 offset len) in
                    (getall fp, errors)
                | Bytes_Conditional(c) ->
                    (getall c, errors)

                | _ ->
                    FormatPlus.failwith "Non-constant function ptr not supported :@ @[%a@]" Printer.exp exp2
            end
        | _ ->
            FormatPlus.failwith "Non-constant function ptr not supported :@ @[%a@]" Printer.exp exp

let exec_fundec job state instr fundec lvalopt exps errors =
    Output.set_mode Output.MSG_FUNC;
    Output.printf "@[Enter function %a@]@\n" Printer.fundec fundec;

    let stmt = job.stmt in

    (* evaluate the arguments *)
    let state, argvs, errors = List.fold_right begin fun exp (state, argvs, errors) ->
        let state, bytes, errors = Expression.rval state exp errors in
        (state, bytes::argvs, errors)
    end exps (state, [], errors) in

    (* [stmt] is an [Instr], so it can't have two successors. If
     [func] returns, then [stmt] has exactly one successor. If
     [func] is [exit] or has the [noreturn] attribute, [stmt]
     has no successor. *)
    let callContext = match stmt.succs with
        | []  -> NoReturn (stmt,instr)
        | [h] -> Source (lvalopt,stmt,instr,h)
        | _   -> assert false
    in
    let state = MemOp.state__start_fcall state callContext fundec argvs in

    (*
    (* If fundec is the function to be examined *)
    if Executeargs.run_args.arg_examfn = fundec.svar.vname then
        InvInput.examine state fundec;
    *)

    (* Update the state, the next stmt to execute, and whether or
     not we're in a tracked function. *)
    let job = { job with
            state = state;
            stmt = List.hd fundec.sallstmts;
            inTrackedFn = StringSet.mem fundec.svar.vname job.trackedFns;
    } in
    (Active job, errors)

let exec_instr_call job instr lvalopt fexp exps errors =
    let state, exHist = job.state, job.exHist in

    let rec process_func_list func_list errors =
        match func_list with
            | [] -> ([], errors)
            | (state, fundec)::t ->
                let job_state, errors =
                    let job = {job with
                        decisionPath = DecisionFuncall(instr, fundec)::job.decisionPath;} in
                    try
                        (exec_fundec job state instr fundec lvalopt exps errors)
                    with Failure msg ->
                        if !Executeargs.arg_failfast then failwith msg;
                        let result = {
                            result_file = job.file;
                            result_state = state;
                            result_history = exHist;
                            result_decision_path = job.decisionPath;
                        } in
                        (Complete (Abandoned (`Failure msg, Job.get_loc job, result)), errors)
                in
                let func_list, errors = process_func_list t errors in
                (job_state::func_list, errors)
    in
    let func_list, errors = function_from_exp job state fexp exps errors in
    let f, errors = process_func_list func_list errors in
    match f with
        | _::_::_ -> (Fork(f), errors)
        | [a] -> (a, errors)
        | [] -> failwith "No valid function found!"


let exec_instr job errors =
    assert (job.instrList <> []);
    let printInstr instr =
        Output.set_mode Output.MSG_STMT;
        Output.printf "%a@\n" Printcil.instr instr
    in

    let instr,tail = match job.instrList with i::tl -> i,tl | _ -> assert false in
    let job = { job with instrList = tail; } in

    (* Within instructions, we have to update line coverage (but not
         statement or edge coverage). *)
    let job =
        if job.inTrackedFn && !Executeargs.arg_line_coverage
        then { job with exHist = addInstrCoverage job instr; }
        else job
    in

    (* Since we've used the makeCFGFeature, an [Instr] is a series of
       [Set]s and [Asm]s, possibly terminated with a [Call]. *)
    match instr with
         | Set(cil_lval, exp, loc) ->
            printInstr instr;
            let state = job.state in
            let state, lval, errors = Expression.lval state cil_lval errors in
            let state, rv, errors = Expression.rval state exp errors in
            let state = MemOp.state__assign state lval rv in
            let nextStmt = if tail = [] then List.hd job.stmt.succs else job.stmt in
            (Active { job with state = state; stmt = nextStmt }, errors)
        | Call(lvalopt, fexp, exps, loc) ->
            assert (tail = []);
            printInstr instr;
            exec_instr_call job instr lvalopt fexp exps errors
        | Asm (_,_,_,_,_,loc) ->
            let result = {
                result_file = job.file;
                result_state = job.state;
                result_history = job.exHist;
                result_decision_path = job.decisionPath;
            } in
            (Complete (Abandoned (`Failure "Cannot handle assembly", loc, result)), errors)

let exec_stmt job errors =
    assert (job.instrList = []);
    let state,stmt = job.state,job.stmt in

    let nextExHist ?(whichBranch=false) nextStmtOpt =
        if job.inTrackedFn
        then addStmtCoverage job whichBranch nextStmtOpt
        else job.exHist
    in

    Output.set_mode Output.MSG_STMT;
    Output.printf "@[%a@\n@]" Printer.stmt_abbr stmt;
    match stmt.skind with
        | Instr [] ->
             let nextStmt = match stmt.succs with [x] -> x | _ -> assert false in
             (Active { job with stmt = nextStmt; exHist = nextExHist (Some nextStmt); }, errors)
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
            (Active { job with instrList = instrs; exHist = nextExHist nextStmtOpt; }, errors)
        | Cil.Return (expopt, loc) ->
            begin match state.callContexts with
                | Runtime::_ -> (* completed symbolic execution (e.g., return from main) *)
                    Output.set_mode Output.MSG_MUSTPRINT;
                    Output.printf "Program execution finished.@\n";
                    let state, retval, errors = match expopt with
                        | None ->
                            (state, None, errors)
                        | Some exp ->
                            let state, retval, errors = Expression.rval state exp errors in
                            (state, Some retval, errors)
                    in
                    let result = {
                            result_file = job.file;
                            result_state = state;
                            result_history = nextExHist None;
                            result_decision_path = job.decisionPath;
                    } in
                    (Complete (Return (retval, result)), errors)
                | (Source (destOpt,callStmt,_,nextStmt))::_ ->
                        let state, errors =
                            match expopt, destOpt with
                                | Some exp, Some dest ->
                                    (* evaluate the return expression in the callee frame *)
                                    let state, rv, errors = Expression.rval state exp errors in
                                    let state = MemOp.state__end_fcall state in
                                    (* evaluate the assignment in the caller frame *)
                                    let state, lval, errors = Expression.lval state dest errors in
                                    (MemOp.state__assign state lval rv, errors)
                                | _, _ ->
                                     (* If we are not returning a value, or if we
                                        ignore the result, just end the call *)
                                    (MemOp.state__end_fcall state, errors)
                        in

                        let callingFuncName = (List.hd state.callstack).svar.vname in
                        let inTrackedFn = StringSet.mem callingFuncName job.trackedFns in

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

                        let job = { job with
                            state = state;
                            stmt = nextStmt;
                            inTrackedFn = inTrackedFn;
                            exHist = exHist;
                        } in
                        (Active job, errors)

                | (NoReturn _)::_ ->
                        failwith "Return from @noreturn function"
                | [] ->
                        (* there should always be a Runtime at the start of the list *)
                        assert false
            end
        | Goto (stmtref, loc) ->
            (Active { job with stmt = !stmtref; exHist = nextExHist (Some !stmtref); }, errors)
        | If (exp, block1, block2, loc) ->
            begin
            (* try a branch *)
                let try_branch state pcopt block =
                    let nextState = match pcopt with
                        | Some(pc) -> MemOp.state__add_path_condition state pc true
                        | None -> state
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
                    (nextState, nextStmt)
                in

                let state, rv, errors = Expression.rval state exp errors in

                Output.set_mode Output.MSG_GUARD;
                if(Output.need_print Output.MSG_GUARD) then
                    begin
                        Output.printf "Check if the following holds:@\n  @[%a@]@\n" BytesPrinter.bytes rv;
                        Output.printf "Under the path condition:@\n";
                        if state.path_condition = [] then
                            Output.printf "  (nil)@\n"
                        else
                            Output.printf "  @[%a@]@\n" (FormatPlus.pp_print_list BytesPrinter.bytes "@ AND ") state.path_condition;
                    end;

                let state, truth = MemOp.eval_with_cache state state.path_condition rv in
                Output.set_mode Output.MSG_REG;
                let job_state = match truth with
                    | Ternary.True ->
                        Output.printf "True@\n";
                        let nextState,nextStmt = try_branch state None block1 in
                        let job' = { job with
                            state = nextState;
                            stmt = nextStmt;
                            decisionPath = (DecisionConditional(stmt, true))::job.decisionPath; }
                        in
                            Active { job' with exHist = nextExHist (Some nextStmt) ~whichBranch:true; }

                    | Ternary.False ->
                        Output.printf "False@\n";
                        let nextState,nextStmt = try_branch state None block2 in
                        let job' = { job with
                            state = nextState;
                            stmt = nextStmt;
                            decisionPath = (DecisionConditional(stmt, false))::job.decisionPath; }
                        in
                            Active { job' with exHist = nextExHist (Some nextStmt) ~whichBranch:false; }

                    | Ternary.Unknown ->
                        Output.printf "Unknown@\n";

                        let nextStateT,nextStmtT = try_branch state (Some rv) block1 in
                        let nextStateF,nextStmtF = try_branch state (Some (logicalNot rv)) block2 in

                        (* Create two jobs, one for each branch. Since we
                           continue executing the false branch immediately, let
                           that job inherit the old jid. Give the true job a new
                           jid. *)
                        let trueJob = { job with
                            state = nextStateT;
                            stmt = nextStmtT;
                            exHist = nextExHist (Some nextStmtT) ~whichBranch:true;
                            decisionPath = (DecisionConditional(stmt, true))::job.decisionPath;
                            jid = Counter.next job_counter; } in
                        let falseJob = { job with
                            state = nextStateF;
                            stmt = nextStmtF;
                            decisionPath = (DecisionConditional(stmt, false))::job.decisionPath;
                            exHist =  nextExHist (Some nextStmtF) ~whichBranch:false; } in
                            Output.set_mode Output.MSG_MUSTPRINT;
                            Output.printf "Branching on @[%a@]@ at %a.@\n"
                            Printer.exp exp
                            Printcil.loc loc;
                            if !Executeargs.arg_print_callstack then
                                Output.printf "Call stack:@\n  @[%a@]@\n" (Printer.callingContext_list "@\n") state.callContexts;
                            Output.printf "Job %d is the true branch and job %d is the false branch.@\n@\n" trueJob.jid falseJob.jid;
                            Fork [Active trueJob; Active falseJob]
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
            (Active { job with stmt = nextStmt; exHist = nextExHist (Some nextStmt); }, errors)
        | _ -> failwith "Not implemented yet"


let errors_to_abandoned_list job errors =
    List.map begin fun (state, failing_condition, error) ->
        (* assert: failing_condition is never true *)
        let result = {
            result_file = job.file;
            result_state = { state with path_condition = failing_condition::state.path_condition };
            result_history = job.exHist;
            result_decision_path = job.decisionPath;
        } in
        Complete (Abandoned (error, Job.get_loc job, result))
    end errors


let step job job_queue =
    try
        let job_state, errors = match job.instrList with
            | [] -> exec_stmt job []
            | _ -> exec_instr job []
        in
        if errors = [] then
            (job_state, job_queue)
        else
            let abandoned_job_states = errors_to_abandoned_list job errors in
            (Fork (job_state::abandoned_job_states), job_queue)

    with
        | Failure msg ->
            if !Executeargs.arg_failfast then failwith msg;
            let result = {
                result_file = job.file;
                result_state = job.state;
                result_history = job.exHist;
                result_decision_path = job.decisionPath;
            } in
                (Complete (Abandoned (`Failure msg, Job.get_loc job, result)), job_queue)
