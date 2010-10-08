open DataStructures
open OcamlUtilities
open OcamlBase
open CilUtilities
open OtterBytes
open OtterCore
open OtterJob
open OtterQueue
open OtterReporter
open OtterDriver
open OtterGraph
open Graph
open Bytes
open Types
open Job
open Cil

type failing_predicate =
    | FailingCondition of state * Bytes.bytes (* TODO (martin): make the condition a list of bytes *)
    | FailingPaths of fork_decision list list

type target = {
  target_func: Cil.fundec;
  target_predicate: failing_predicate;
}

(* Cil feature for call-chain backwards Otter *)
let arg_assertfn = ref "__ASSERT"

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
        | ForkConditional(stmt,truth) ->
                Format.fprintf ff "Decision: @[%a@]: %s@\n" Printer.stmt_abbr stmt (if truth then "true" else "false")
        | ForkFunptr(instr,fundec) ->
                Format.fprintf ff "Decision: @[%a@]@\n" Printer.fundec fundec
        | ForkEnd ->
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
    | Job.Truncated(_,_) -> Format.fprintf ff "Truncated@\n"

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


let test_job_at_targets targets job =
	(* if job meets one of the targets, do checking *)
	(* if fails, return Some (Complete Abandoned) *)
	match job.instrList with
		| (Call(lvalopt,fexp,exps,loc) as instr)::tail ->
            (* Advance the current instruction to the next *)
	        let job = { job with instrList = tail; } in
            let job_state = Statement.exec_instr_call job instr lvalopt fexp exps in
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
                            is_prefix decision_equals (List.rev decision_path) (List.rev bounding_path)
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
                    let (>>>) = Interceptor.(>>>) in
                    let interceptor =
                        Interceptor.set_output_formatter_interceptor
                        >>> BuiltinFunctions.interceptor
                        >>> subset_bounding_paths_interceptor
                    in
                    (* Cut out the prefix of the decision path, so that we can compare the decision
                     * path with the bounding paths side by side. Put the prefix back when done *)
                    let job' = {job with
                        boundingPaths = Some paths;
                        decisionPath = [];
                    } in
                    let return = Driver.run ~interceptor job' in
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
                        | Truncated (job_result1, job_result2) -> Truncated (add_prefix job_result1, add_prefix job_result2)
                    ) return
            in

            let failing_paths : fork_decision list list =
                List.fold_left (
                    fun failing_paths (job,target) ->
                        Format.printf "Test job %d at target function @[%a@]@\n" job.jid Printer.fundec target.target_func;
                        let partial_failing_paths =
                          match target.target_predicate with
                          | FailingCondition (_,_) -> failwith "Not implemented yet"
                          | FailingPaths (paths) ->
                              (* add an ForkEnd to mark the end of a bounding path *)
                              let bounding_paths = List.map (fun path -> ForkEnd::path) paths in
                              let job_completions : 'reason job_completion list = forward_otter_bounded_by_paths job bounding_paths in
                                List.fold_left (fun failing_paths job_completion ->
                                    match job_completion with
                                    | Abandoned (`Failure(_),_,result) -> result.result_decision_path::failing_paths
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
			  		result_decision_path = job.decisionPath;
                } in
			  	  Some (Complete (Abandoned (`FailingPaths failing_paths, loc, result)))
		| _ ->
			None

let test_job_at_targets_interceptor targets job job_queue interceptor =
	match test_job_at_targets targets job with
	    | Some (Complete (Abandoned (`FailingPaths _, _, _)) as job_state) ->
			(job_state, job_queue)
		| None ->
			interceptor job job_queue
        | _ -> failwith "test_job_at_targets: unreachable program point"

let callchain_backward_se file entryfn assertfn job_init : _ job_completion list list =

  let print_targets fn ts =
	Format.printf "@\nStart forward SE on function %s with target(s): %s@\n@\n"
	  (fn.svar.vname)
      (let s=(String.concat "," (List.map (fun t -> t.target_func.svar.vname) ts)) in if s="" then "(none)" else s)
  in

  (* Run forward SE on the examining function (job), given the targets. *)
  let call_Otter_main_loop targets job =
      let (>>>) = Interceptor.(>>>) in
      let interceptor =
          Interceptor.set_output_formatter_interceptor
          >>> (test_job_at_targets_interceptor targets)
          >>> BuiltinFunctions.interceptor in
	let queue =
        if (!Executeargs.arg_cfg_pruning) then
            BestFirstQueue.make (distance_to_targets_prioritizer job.state.callstack (assertfn::(List.map (fun t -> t.target_func) targets)))
        else
            Queue.get_default ()
    in
        Driver.run ~interceptor ~queue job
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
            Abandoned(`FailingPaths(paths),_,_) ->
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
                      (* TODO (martin): this join may be unecessary, because forward_otter_bounded_by_paths already returns joined paths *)
                      let joined_paths = join_paths job_result.result_decision_path paths in
                        List.rev_append joined_paths all_paths
              | Abandoned (_,_,job_result) ->
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
		    	  let job' = job_init caller in
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
		  let job = job_init caller in
		  let new_result = callchain_backward_main_loop job targets in
		    new_result::results
	  ) [] callers

let prepare_file file =
	Executeargs.arg_cfg_pruning := true;
	Core.prepare_file file

let doit file =
	(* TODO: do something about signal handlers/run statistics from Executemain.doExecute *)

    Format.printf "@\n@\nCall-chain backward Symbolic Execution@\n@\n";
	prepare_file file;
	let entryfn = Driver.find_entryfn file in
	let assertfn =
		let fname = !arg_assertfn in
		try FindCil.fundec_by_name file fname
		with Not_found -> FormatPlus.failwith "Assertion function %s not found" fname
	in
	let job_init = fun entryfn -> FunctionJob.make file entryfn in
	let results = callchain_backward_se file entryfn assertfn job_init in

	(* print the results *)
	Output.set_formatter (new Output.plain);
	Output.printf "%s@\n@\n" (Executedebug.get_log ());
	List.iter (fun result -> Report.print_report result) results

let feature = {
	Cil.fd_name = "backotter";
	Cil.fd_enabled = ref false;
	Cil.fd_description = "Call-chain backwards symbolic executor for C";
	Cil.fd_extraopt = [
		("--assertfn",
		Arg.Set_string arg_assertfn,
		"<fname> Assertion function to look for in the call-chain-backward mode (default: __ASSERT) @\n");
	];
	Cil.fd_post_check = true;
	Cil.fd_doit = doit
}
