open DataStructures
open OcamlUtilities
open CilUtilities
open OtterBytes
open OtterCore
open OtterQueue
open OtterReporter
open OtterDriver
open OtterGraph
open Graph
open Bytes
open Types
open Job
open Decision
open Cil

module Reporter = BasicReporter.Make (BackOtterErrors)

type failing_predicate =
    | FailingCondition of state * Bytes.bytes (* TODO (martin): make the condition a list of bytes *)
    | FailingPaths of Decision.t list list (* most recent decision first *)

type target = {
  target_func: Cil.fundec;
  target_predicate: failing_predicate;
}

(* Cil feature for call-chain backwards Otter *)
let arg_assertfn = ref "__FAILURE"

let otter_failure_interceptor job job_queue interceptor =
	match job.instrList with
		| Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc)::_ when varinfo.Cil.vname = (!arg_assertfn) ->
            let job_result = {
                result_file = job.Job.file;
                result_state = job.Job.state;
                result_history = job.Job.exHist;
                result_decision_path = job.Job.decisionPath;
            } in
            let loc = Job.get_loc job in
            let job_state = Complete (Abandoned (`FailureReached, loc, job_result)) in
                job_state, job_queue
		| _ ->
			interceptor job job_queue


let run ?interceptor ?queue job =
    let (>>>) = Interceptor.(>>>) in
    let default_interceptor =
        Interceptor.set_output_formatter_interceptor
        >>> otter_failure_interceptor
        >>> BuiltinFunctions.libc_interceptor
        >>> BuiltinFunctions.interceptor
    in
    let integrated_interceptor = match interceptor with
    | None -> default_interceptor
    | Some (interceptor) -> default_interceptor >>> interceptor
    in
    (Driver.main_loop ~interceptor:integrated_interceptor ?queue (new Reporter.t ()) job)#completed


let distance_to_targets_prioritizer callstack target_fundecs job =
    if (List.length job.state.callstack) = (List.length callstack) then
      let graph,root = make_graph (List.hd job.state.callstack) in
      let target_nodes =
        Graph.filter_nodes graph
          begin
            fun node -> match node.obj with
              | InstrStmt.Instr((Call(_,Lval(Var(varinfo),_),exps,_))::_,_)  ->
                  List.fold_left (fun b t -> if t.svar == varinfo then true else b) false target_fundecs
              | _ -> false
          end
      in
      let get_predicate job node =
        match job.instrList,job.stmt,node.obj with
          | [],stmt,InstrStmt.Stmt(stmt') when stmt==stmt' -> true
          | [],_,_ -> false
          | instrs,_,InstrStmt.Instr(instrs',_) when instrs==instrs' -> true
          | _ -> false
      in
      let sources = Graph.filter_nodes graph (get_predicate job) in
      assert(List.length sources = 1);
      let source = List.hd sources in
      Graph.set_color graph max_int;
      let backward_distance_from_targets =
          List.fold_left (fun d tar ->
              let d' = Graph.backward_distance graph source tar in
                  min d d'
          ) max_int target_nodes
      in
          if backward_distance_from_targets = max_int then
              BestFirstQueue.Drop (Printf.sprintf "Warning: job %d dropped" job.jid)
          else
              BestFirstQueue.Rank (float_of_int backward_distance_from_targets)
    else
        (* If not in entry function (through function calls), avoid running it early.
         * TODO (martin): give a rank equal to the rank of the function call in the entry function?
         *)
        BestFirstQueue.Rank (max_float)

let print_decisions ff decisions =
    let print_decision ff decision =
        match decision with
        | DecisionConditional(stmt,truth) ->
                Format.fprintf ff "Decision: @[%a@]: %s@\n" Printer.stmt_abbr stmt (if truth then "true" else "false")
        | DecisionFuncall(instr,fundec) ->
                Format.fprintf ff "Decision: @[%a@]@\n" Printer.fundec fundec
        | DecisionEnd ->
                Format.fprintf ff "Decision: END@\n"
    in
        if decisions = [] then
            Format.fprintf ff "Decision: (none)@\n"
        else
            List.iter (print_decision ff) decisions

let print_job_completion ff job_completion =
    match job_completion with
    | Job.Return(_,_) -> Format.fprintf ff "Return@\n"
    | Job.Exit(_,_) -> Format.fprintf ff "Exit@\n"
    | Job.Abandoned(`FailingPaths(_),_,_) -> Format.fprintf ff "Abandoned(FailingPaths)@\n"
    | Job.Abandoned(_,_,_) -> Format.fprintf ff "Abandoned@\n"

let print_job ff job =
    Format.fprintf ff "Job (first statement):@\n";
    match job.instrList with
    | instr::_ -> Format.fprintf ff "@[%a@]@\n" Printcil.instr instr
    | [] -> Format.fprintf ff "@[%a@]@\n" Printcil.stmt job.stmt

let print_list name print_function lst =
    Format.printf "-----------------------------------------@\n";
    Format.printf "List of %s(s) (length %d)@\n" name (List.length lst);
    List.iter (Format.printf "Element: @[%a@]@\n" print_function) lst;
    Format.printf "-----------------------------------------@\n"


(* TODO (martin): test if this abandoned path is due to a call to
 * __FAILURE(), or something else. We might want to discard the latter.
 *)
let test_job_at_targets targets job =
    (* if job meets one of the targets, do checking *)
    (* if fails, return Some (Complete Abandoned) *)
    match job.instrList with
        | (Call(lvalopt,fexp,exps,loc) as instr)::tail ->
            (* Advance the current instruction to the next *)
            let job = { job with instrList = tail; } in
            let errors = [] in
            let job_state, errors = Statement.exec_instr_call job instr lvalopt fexp exps errors in
            List.iter (fun (_,_,error) -> Format.printf "An error caught in test_job_at_targets when initializing the function call: %a @\n" Errors.printer error) errors;
            let active_jobs =
              let rec get_active_jobs job_state =
                  match job_state with
                  | Active(job) -> [job]
                  | Fork(job_states) ->
                      List.fold_left (fun active_jobs job_state ->
                        let active_jobs' = get_active_jobs job_state in
                          List.rev_append active_jobs' active_jobs
                      ) [] job_states
                  | _ -> []
              in
                get_active_jobs job_state
            in
            print_list "Active Job" print_job active_jobs;
            (* Match jobs with their corresponding targets *)
            let targetted_jobs : (Job.job * target) list =
                List.fold_left (fun targetted_jobs job ->
                    let fundec = List.hd job.state.callstack in
                    let matched_targets = List.filter (fun target -> fundec == target.target_func) targets in
                    assert (List.length matched_targets <= 1);
                    if List.length matched_targets = 0 then
                        targetted_jobs
                    else
                        (job, List.hd matched_targets)::targetted_jobs
                ) [] active_jobs
            in
            print_list "Targetted Job"
              (fun ff (job,target) -> print_job ff job) targetted_jobs;
            (*
             * Forward SE test.
             * See if the current job (state) can follow the bounding paths
             *)
            let forward_otter_bounded_by_paths job paths =
                Format.printf "@\nBegin forward_otter_bounded_by_paths@\n";
                let subset_bounding_paths_interceptor job job_queue interceptor =
                    (* The decision path has the prefix (things happened before the checking function) taken out *)
                    Format.printf "Decision path:@\n";
                    Format.printf "@[%a@]@\n" print_decisions job.decisionPath;

                    let bounding_paths = match job.boundingPaths with
                    | None -> []
                    | Some(boundingPaths) -> boundingPaths
                    in
                        print_list "Bounding Path" print_decisions bounding_paths;

                    (* A bounding path agrees with the decision path if the decision path is a prefix of it
                     * Since paths have most recent decision first, prefix becomes suffix *)
                    let paths_agree decision_path bounding_path =
                        let rec is_prefix eq pre lst =
                            match pre, lst with
                            | d1::pre', d2::lst' -> if eq d1 d2 then is_prefix eq pre' lst' else false
                            | [], _ -> true
                            | _, _ -> false
                        in
                            is_prefix Decision.equals (List.rev decision_path) (List.rev bounding_path)
                    in
                    let agreed_bounding_paths = List.filter (paths_agree job.decisionPath) bounding_paths in
                        print_list "Agreed Bounding Path" print_decisions agreed_bounding_paths;
                        if agreed_bounding_paths = [] then
                            (* stop this job *)
                            let job_result = {
                                result_file = job.Job.file;
                                result_state = job.Job.state;
                                result_history = job.Job.exHist;
                                result_decision_path = job.Job.decisionPath;
                            } in
                              (Complete (Exit (None, job_result)), job_queue)
                        else
                            let job' = {job with boundingPaths = Some(agreed_bounding_paths);} in
                            interceptor job' job_queue
                in
                (* Cut out the prefix of the decision path, so that we can compare the decision
                 * path with the bounding paths side by side. Put the prefix back when done *)
                let job' = {job with
                    boundingPaths = Some paths;
                    decisionPath = [];
                } in
                let interceptor = subset_bounding_paths_interceptor in
                let return = run ~interceptor job' in
                Format.printf "@\nEnd forward_otter_bounded_by_paths@\n";
                let add_prefix job_result =
                    {job_result with
                        result_decision_path = List.append job_result.result_decision_path job.decisionPath;
                    }
                in
                List.map (function
                    | Job.Return (bytes_opt, job_result) -> Job.Return (bytes_opt, add_prefix job_result)
                    | Exit (bytes_opt, job_result) -> Exit (bytes_opt, add_prefix job_result)
                    | Abandoned (reason, loc, job_result) -> Abandoned (reason, loc, add_prefix job_result)
                ) return
            in

            let failing_paths =
                List.fold_left (
                    fun failing_paths (job,target) ->
                        Format.printf "Test job %d at target function @[%a@]@\n" job.jid Printer.fundec target.target_func;
                        let partial_failing_paths =
                          match target.target_predicate with
                          | FailingCondition (_,_) -> failwith "Not implemented yet"
                          | FailingPaths (paths) ->
                              (* add an DecisionEnd to mark the end of a bounding path *)
                              let bounding_paths = List.map (fun path -> DecisionEnd::path) paths in
                              let job_completions : 'reason job_completion list = forward_otter_bounded_by_paths job bounding_paths in
                                List.fold_left (fun failing_paths job_completion ->
                                    match job_completion with
                                    | Abandoned (`Failure(_),_,result)
                                    | Abandoned (`FailureReached, _, result) -> result.result_decision_path::failing_paths
                                    | _ -> failing_paths
                                ) [] job_completions
                        in
                          List.rev_append partial_failing_paths failing_paths
                ) [] targetted_jobs
            in
            if List.length failing_paths = 0 then
                None
            else
                let result = {
                    result_file = job.Job.file;
                    result_state = job.state;  (* Doesn't matter what state is; won't be used *)
                    result_history = job.exHist;
                    result_decision_path = job.decisionPath; }
                in
                Some (Complete (Abandoned (`FailingPaths failing_paths, loc, result)))
        | _ ->
            None

let test_job_at_targets_interceptor targets job job_queue interceptor =
	match test_job_at_targets targets job with
	    | Some (Complete (Abandoned (`FailingPaths _, _, _)) as job_state) ->
            (* TODO (martin) the SE is incomplete if we stop here. *)
			(job_state, job_queue)
		| None ->
			interceptor job job_queue
        | _ -> failwith "test_job_at_targets: unreachable program point"

let callchain_backward_se_legacy file : _ job_completion list =
  let job = OtterJob.Job.get_default file in
  let entryfn = List.hd job.state.callstack in
  let assertfn =
    let fname = !arg_assertfn in
    try FindCil.fundec_by_name file fname
    with Not_found -> FormatPlus.failwith "Assertion function %s not found" fname
  in

  let print_targets fn ts =
	Format.printf "@\nStart forward SE on function %s with target(s): %s@\n@\n"
	  (fn.svar.vname)
      (let s=(String.concat "," (List.map (fun t -> t.target_func.svar.vname) ts)) in if s="" then "(none)" else s)
  in

  (* Run forward SE on the examining function (job), given the targets. *)
  let call_Otter_main_loop targets job =
	  let queue =
          if (!Executeargs.arg_cfg_pruning) then
              new BestFirstQueue.t (distance_to_targets_prioritizer job.state.callstack (assertfn::(List.map (fun t -> t.target_func) targets)))
          else
              Queue.get_default ()
      in
      (* TODO (martin): test_job_at_targets_interceptor will be invoked AFTER BuiltinFunctions.interceptor.
       *                Is this what we want? *)
      let interceptor = test_job_at_targets_interceptor targets in
      run ~interceptor ~queue job
  in

  (* The implementation of main loop *)
  let rec callchain_backward_main_loop job targets : 'reason job_completion list =
	(* Assume we start at f *)
	let f = List.hd job.state.callstack in
	(* Run forward SE based on the targets *)
	let results : 'reason job_completion list = call_Otter_main_loop targets job in
	  if f == entryfn then
		(* If f is main(), we are done *)
        let _ = List.iter ( function
            | Abandoned(`FailingPaths(paths),_,_) ->
                print_list "Final failing path" print_decisions paths
            | _ -> ()
        ) results in
		    results
	  else
        let _ = print_list "Completed Job" print_job_completion results in
        let join_paths prefix suffixes =
            (* All paths have most recent decision first *)
            (* If suffixes is empty, that means no failing paths are satisfied. *)
            List.map (fun suffix -> List.append suffix prefix) suffixes
        in
        let failing_paths = List.fold_left
          ( fun all_paths job_completion ->
              match job_completion with
              | Abandoned (`FailingPaths(paths),_,job_result) ->
                      let joined_paths = join_paths job_result.result_decision_path paths in
                        List.rev_append joined_paths all_paths
              | Abandoned (`FailureReached,_,job_result) ->
                      job_result.result_decision_path::all_paths
              | Abandoned (`Failure(_),_,job_result) when (not !BackOtterReporter.arg_no_exceptions_as_failures) ->
                      job_result.result_decision_path::all_paths
              | _ -> all_paths
          ) [] results
        in
        print_list "Failing path" print_decisions failing_paths;
        (* TODO (martin): based on the properties of the function, and the failing_paths,
         *                decide if the failing predicate is a condition or paths
         *)
        if failing_paths = [] then
            let _ = Format.printf "No failing paths from calling function %s.@\n" f.svar.vname in
            []
        else
		    let new_target = {
		      target_func = f;
              target_predicate = FailingPaths(failing_paths);
		    } in
            Format.printf "Create new target for function %s with failing paths:@\n" f.svar.vname;
            print_list "Failing Path" print_decisions failing_paths;
		    let callers = CilCallgraph.find_callers file f in
              print_list (Format.sprintf "Function %s's caller" f.svar.vname) Printer.fundec callers;
		      List.fold_left
		      (
		    	fun lst caller ->
		    	  let targets' = (new_target::targets) in
                  let _ = print_targets caller targets' in
		    	  let job' = OtterJob.FunctionJob.make file caller in
		    	  let lst' = callchain_backward_main_loop job' targets'  in
		    		List.rev_append lst' lst
		      )
		      [] callers
  in

  let assert_target = {
      target_func = assertfn;
      target_predicate = FailingPaths([]);
  } in
  let callers = CilCallgraph.find_callers file assertfn in
	List.fold_left
	  (fun results caller ->
          (* let targets = [assert_target] in *)
          let _ = assert_target in
          let targets = [] in
          let _ = print_targets caller targets in
		  let job = OtterJob.FunctionJob.make file caller in
		  let new_result = callchain_backward_main_loop job targets in
		  List.rev_append new_result results
	  ) [] callers


let prepare_file file =
	Executeargs.arg_cfg_pruning := true;
	Core.prepare_file file

let doit file =

    Format.printf "@\n@\nCall-chain backward Symbolic Execution@\n@\n";
	let startTime = Unix.gettimeofday () in
	(* Set signal handlers to catch timeouts and interrupts *)
	let old_ALRM_handler =
		Sys.signal Sys.sigalrm
			(Sys.Signal_handle (fun _ -> raise (SignalException "Timed out!")))
	and old_INT_handler =
		Sys.signal Sys.sigint
			(Sys.Signal_handle (fun _ -> raise (SignalException "User interrupt!")))
	in
	(* Set a timer *)
	ignore (Unix.alarm !Executeargs.arg_timeout);

	prepare_file file;
	let results = callchain_backward_se_legacy file in

	(* Turn off the alarm and reset the signal handlers *)
	ignore (Unix.alarm 0);
	Sys.set_signal Sys.sigalrm old_ALRM_handler;
	Sys.set_signal Sys.sigint old_INT_handler;

	(* print the results *)
	Output.set_formatter (new Output.plain);
	Output.printf "%s@\n@\n" (Executedebug.get_log ());
	Output.printf "\nSTP was invoked %d times. (%d cache hits; %d misses)\n" !Stp.stp_count !Stp.cacheHits !Stp.cacheMisses;

	let executionTime = (Unix.gettimeofday ()) -. startTime
	and stpTime = Stats.lookupTime "STP" in
	Output.printf "It ran for %.2f s, which is %.2f%% of the total %.2f s execution.\n"
		stpTime (100. *. stpTime /. executionTime) executionTime;
	Output.printf "  It took %.2f s to construct the formulas for the expressions inside 'if(...)'s,
  %.2f s to construct and %.2f s to assert the path conditions,
  and %.2f s to solve the resulting formulas.\n\n"
		(Stats.lookupTime "convert conditional")
		(Stats.lookupTime "STP construct")
		(Stats.lookupTime "STP doassert")
		(Stats.lookupTime "STP query");
   (if !Executeargs.arg_simplify_path_condition then
      Output.printf "It took %.2f s to simplify path conditions.\n"
         (Stats.lookupTime "Simplify PC")
    else ());

    Output.printf "Hash-consing: hits=%d misses=%d\n" (!Bytes.hash_consing_bytes_hits) (!Bytes.hash_consing_bytes_misses);
    Output.printf "Bytes eval caching: hits=%d misses=%d\n\n" (!MemOp.bytes_eval_cache_hits) (!MemOp.bytes_eval_cache_misses);
    if (!Stp.print_stp_queries) then (
        Format.printf "Stp queries: @\n";
        List.iter (fun (pc, pre, guard, truth_value, time) ->
            List.iter (Format.printf "PC: @[%a@]@\n" BytesPrinter.bytes) pc;
            Format.printf "PRE: @[%a@]@\n" BytesPrinter.guard pre;
            Format.printf "QUERY: @[%a@]@\n" BytesPrinter.guard guard;
            Format.printf "TRUTH: @[%s@]@\n" (if truth_value then "True" else "False");
            Format.printf "TIME: @[%.2f@]@\n" time;
            Format.printf "--------------------------------------------------@\n"
        ) (!Stp.stp_queries)
    );
    Report.print_report results


let feature = {
	Cil.fd_name = "legacybackotter";
	Cil.fd_enabled = ref false;
	Cil.fd_description = "Call-chain backwards symbolic executor for C";
	Cil.fd_extraopt = [
		("--assertfn",
		Arg.Set_string arg_assertfn,
		"<fname> Assertion function to look for in the call-chain-backward mode (default: __FAILURE)");
	];
	Cil.fd_post_check = true;
	Cil.fd_doit = doit
}
