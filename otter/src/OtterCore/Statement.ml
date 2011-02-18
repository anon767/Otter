open DataStructures
open OcamlUtilities
open Cil
open CilUtilities
open OtterBytes
open Bytes
open BytesUtility
open State
open Job
open Decision


let get_active_state parent_job job =
    let job = job#with_jid_unique (Counter.next job_counter_unique) in
    let job = job#with_jid_parent (parent_job#jid_unique) in
    Active job

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
let addStmtCoverage job whichBranch nextStmtOpt =
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

let addInstrCoverage job instr =
    let instrLoc = get_instrLoc instr in
    { job#exHist with coveredLines =
            LineSet.add (instrLoc.Cil.file,instrLoc.Cil.line) job#exHist.coveredLines; }

let function_from_exp job instr fexp errors =
    match fexp with
        | Lval(Var(varinfo), NoOffset) ->
            begin
                try
                    ([ (job, FindCil.fundec_by_varinfo job#file varinfo) ], errors)
                with Not_found ->
                    failwith ("Function "^varinfo.vname^" not found.")
            end


        | Lval(Mem(fexp), NoOffset) ->
            let job, bytes, errors  = Expression.rval job fexp errors in
            let rec getall fp =
                let fundecs, errors = Bytes.conditional__fold begin fun (fundecs, errors) pre leaf ->
                    match leaf with
                        | Bytes_FunPtr varinfo ->
                            let fundec = FindCil.fundec_by_varinfo job#file varinfo in
                            ((fundec, pre)::fundecs, errors)
                        | _ ->
                            (fundecs, (job, `Failure "Invalid function pointer")::errors)
                end ([], errors) fp in
                let jobs = (job : #Info.t)#fork begin fun job (fundec, pre) jobs ->
                    let job = MemOp.state__add_path_condition job (Bytes.guard__to_bytes pre) true in
                    (job, fundec)::jobs
                end fundecs [] in
                (jobs, errors)
            in
            begin match bytes with
                | Bytes_FunPtr varinfo ->
                    (* the varinfo should always map to a valid fundec (if the file was parsed by Cil) *)
                    let fundec = FindCil.fundec_by_varinfo job#file varinfo in
                    ([ (job, fundec) ], errors)
                | Bytes_Read(bytes2, offset, len) ->
                    let fp = (BytesUtility.expand_read_to_conditional bytes2 offset len) in
                    let (), fp = Bytes.conditional__prune ~test:(fun () pre guard -> ((), Stp.query_stp job#state.path_condition pre guard)) () fp in
                    let fp, errors = getall fp in
                    (fp, errors)
                | Bytes_Conditional(c) ->
                    let fp, errors = getall c in
                    (fp, errors)

                | _ ->
                    FormatPlus.failwith "Non-constant function ptr not supported :@ @[%a@]" CilPrinter.exp fexp
            end
        | _ ->
            FormatPlus.failwith "Non-constant function ptr not supported :@ @[%a@]" CilPrinter.exp fexp

let exec_fundec job instr fundec lvalopt exps errors =
    (* TODO: Profiler.start_fcall job fundec *)

    let stmt = job#stmt in

    (* evaluate the arguments *)
    let job, argvs, errors = List.fold_right begin fun exp (job, argvs, errors) ->
        let job, bytes, errors = Expression.rval job exp errors in
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
    let job' = job in
    let job' = job'#with_stmt (List.hd fundec.sallstmts) in
    let job' = job'#with_inTrackedFn (StringSet.mem fundec.svar.vname job#trackedFns) in
    (get_active_state job job', errors)

let exec_instr_call job instr lvalopt fexp exps errors =
    let rec process_func_list func_list errors =
        match func_list with
            | [] -> ([], errors)
            | (job, fundec)::t ->
                let job_state, errors =
                    try
                        let job = job#with_decision_path (DecisionFuncall(instr, fundec)::job#decision_path) in
                        exec_fundec job instr fundec lvalopt exps errors
                    with Failure msg ->
                        if !Executeargs.arg_failfast then failwith msg;
                        (Complete (Abandoned (`Failure msg, job)), errors)
                in
                let func_list, errors = process_func_list t errors in
                (job_state::func_list, errors)
    in
    let func_list, errors = function_from_exp job instr fexp errors in
    begin if List.length func_list > 1 then 
        Output.set_mode Output.MSG_FUNC;
        Output.printf "Symbolic function pointer encountered. Fork job %d to " job#path_id;
        List.iter (fun (job, fundec) -> Output.printf "(job %d,function %s)" job#path_id fundec.svar.vname) func_list;
        Output.printf "@\n"
    end;
    let f, errors = process_func_list func_list errors in
    match f with
        | _::_::_ -> 
            (Fork(f), errors)
        | [a] -> (a, errors)
        | [] -> failwith "No valid function found!"


let exec_instr job errors =
    assert (job#instrList <> []);
    let printInstr instr =
        Output.set_mode Output.MSG_STMT;
        Output.printf "%a@\n" Printcil.instr instr
    in

    let old_job = job in
    let instr, tail = match job#instrList with i::tl -> (i, tl) | _ -> assert false in
    let job = job#with_instrList tail in

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
            let old_job = job in
            let job, lval, errors = Expression.lval job cil_lval errors in
            let job, rv, errors = Expression.rval job exp errors in
            let job = MemOp.state__assign job lval rv in
            let job = job#with_stmt (if tail = [] then List.hd job#stmt.succs else job#stmt) in
            (get_active_state old_job job, errors)
        | Call(lvalopt, fexp, exps, loc) ->
            assert (tail = []);
            printInstr instr;
            exec_instr_call job instr lvalopt fexp exps errors
        | Asm _ ->
            (Complete (Abandoned (`Failure "Cannot handle assembly", old_job)), errors)

let exec_stmt job errors =
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
        Output.printf "@[Enter function %a@]@\n" CilPrinter.fundec fundec;
    end;

    Output.set_mode Output.MSG_STMT;
    Output.printf "@[%a@\n@]" CilPrinter.stmt_abbr stmt;
    match stmt.skind with
        | Instr [] ->
             let nextStmt = match stmt.succs with [x] -> x | _ -> assert false in
             (get_active_state job ((job#with_stmt nextStmt)#with_exHist (nextExHist (Some nextStmt))), errors)
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
            (get_active_state job ((job#with_instrList instrs)#with_exHist (nextExHist nextStmtOpt)), errors)
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
                    Output.set_mode Output.MSG_MUSTPRINT;
                    Output.printf "Program execution finished.@\n";
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

                        let old_job = job in
                        let job = job#with_stmt nextStmt in
                        let job = job#with_inTrackedFn inTrackedFn in
                        let job = job#with_exHist exHist in
                        (get_active_state old_job job, errors)

                | (NoReturn _)::_ ->
                        failwith "Return from @noreturn function"
                | [] ->
                        (* there should always be a Runtime at the start of the list *)
                        assert false
            end
        | Goto (stmtref, loc) ->
            (get_active_state job ((job#with_stmt !stmtref)#with_exHist (nextExHist (Some !stmtref))), errors)
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
                if(Output.need_print Output.MSG_GUARD) then
                    begin
                        Output.printf "Check if the following holds:@\n  @[%a@]@\n" BytesPrinter.bytes rv;
                        Output.printf "Under the path condition:@\n";
                        if job#state.path_condition = [] then
                            Output.printf "  (nil)@\n"
                        else
                            Output.printf "  @[%a@]@\n" (FormatPlus.pp_print_list BytesPrinter.bytes "@ AND ") job#state.path_condition;
                    end;

                let truth = MemOp.eval job#state.path_condition rv in
                Output.set_mode Output.MSG_REG;
                let job_state = match truth with
                    | Ternary.True ->
                        Output.printf "True@\n";
                        let old_job = job in
                        let job = try_branch job None block1 in
                        let job = job#with_decision_path ((DecisionConditional(stmt, true))::job#decision_path) in
                        let job = job#with_exHist (nextExHist (Some job#stmt) ~whichBranch:true) in
                        get_active_state old_job job

                    | Ternary.False ->
                        Output.printf "False@\n";
                        let old_job = job in
                        let job = try_branch job None block2 in
                        let job = job#with_decision_path ((DecisionConditional(stmt, false))::job#decision_path) in
                        let job = job#with_exHist  (nextExHist (Some job#stmt) ~whichBranch:false) in
                        get_active_state old_job job

                    | Ternary.Unknown ->
                        Output.printf "Unknown@\n";

                        (* Create two jobs, one for each branch. The false branch
                           inherits the old jid, and the true job gets a new jid. *)
                        let jobs = (job : #Info.t)#fork begin fun job (rv, block, branch) jobs ->
                            let job = try_branch job (Some rv) block in
                            let job = job#with_decision_path ((DecisionConditional(stmt, branch))::job#decision_path) in
                            let job = job#with_exHist (nextExHist (Some job#stmt) ~whichBranch:branch) in
                            job::jobs
                        end [ (logicalNot rv, block2, false); (rv, block1, true) ] [] in
                        let false_job, true_job = match jobs with [ true_job; false_job ] -> (true_job, false_job) | _ -> failwith "Impossible!" in

                            Output.set_mode Output.MSG_MUSTPRINT;
                            Output.printf "Branching on @[%a@]@ at %a.@\n"
                            CilPrinter.exp exp
                            Printcil.loc loc;
                            if !Executeargs.arg_print_callstack then
                                Output.printf "Call stack:@\n  @[%a@]@\n" (Printer.callingContext_list "@\n") job#state.callContexts;
                            Output.printf "Job %d is the true branch and job %d is the false branch.@\n@\n" true_job#path_id false_job#path_id;
                            Fork [get_active_state job true_job; get_active_state job false_job]
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
            (get_active_state job ((job#with_stmt nextStmt)#with_exHist (nextExHist (Some nextStmt))), errors)
        | _ -> failwith "Not implemented yet"


let errors_to_abandoned_list job errors =
    List.map begin fun (job, error) ->
        Complete (Abandoned (error, job))
    end errors


let step job job_queue =
    try
        let job_state, errors = match job#instrList with
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
            (Complete (Abandoned (`Failure msg, job)), job_queue)
