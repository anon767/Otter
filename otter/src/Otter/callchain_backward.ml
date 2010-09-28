open DataStructures
open OcamlUtilities
open OcamlBase
open CilUtilities
open OtterBytes
open OtterCore
open OtterJob
open OtterDriver
open Bytes
open Types
open Job
open Cil


class prioritized_job_queue assertfn targets = object (self)
	val get_priority = Prioritizer.prioritize assertfn targets
	val queue = PriorityQueue.make (fun j1 j2 -> j1#priority >= j2#priority)

	method get =
		try
			let first = PriorityQueue.first queue in
			PriorityQueue.remove_first queue;
			Some (first#job, self)
		with Failure "PriorityQueue.first: empty queue" ->
			None

	method queue job =
		let priority = get_priority job in
		if priority < -.(float_of_int (max_int - 1)) then
			Output.printf "Warning: job %d not continued\n" job.jid
		else
			(* Output.printf "Add Job %d with priority %0.1f\n%!" job.jid priority; *)
			PriorityQueue.add queue (object method job=job method priority=priority end)
end

let rec process_result result completed job_queue =
	match result with
		| Active job ->
			job_queue#queue job;
			(completed, job_queue)

		| Fork states ->
			List.fold_left (fun (completed, job_queue) state -> process_result state completed job_queue) (completed, job_queue) states

		| Complete completion ->
			Driver.output_completion_info completion;
			((completion::completed), job_queue)

		| _ ->
			(completed, job_queue)

let test_job_at_targets targets job =
	(* if job meets one of the targets, do checking *)
	(* if fails, return Some (Complete Abandoned) *)
	match job.instrList with
		| (Call(lvalopt,fexp,exps,loc) as instr)::_ ->
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
            Format.printf "Number of active_jobs = %d\n%!" (List.length active_jobs);
            let targetted_jobs : (Job.job * Prioritizer.target) list = 
                List.fold_left (fun targetted_jobs job ->
                    let fundec = List.hd job.state.callstack in
                    let matched_targets = List.filter (fun target -> fundec == target.Prioritizer.target_func) targets in
                    assert (List.length matched_targets <= 1);
                    if List.length matched_targets = 0 then
                        targetted_jobs
                    else
                        (job, List.hd matched_targets)::targetted_jobs
                ) [] active_jobs
            in
            Format.printf "Number of targetted_jobs = %d\n%!" (List.length targetted_jobs);
            let forward_otter_bounded_by_paths job paths =
              let bounding_paths_interceptor job job_queue interceptor =
                (* TODO (martin): implement *)
                interceptor job job_queue
              in
              let (@@) = Interceptor.(@@) in
              Driver.main_loop
                Driver.get_job_list
                (
              	  Interceptor.set_output_formatter_interceptor @@
                  bounding_paths_interceptor @@
              	  BuiltinFunctions.interceptor @@
              	  Statement.step
                )
                Driver.process_result
                [{job with boundingPaths = Some paths;}]
            in
            let failing_paths : fork_decision list list = 
                List.fold_left (
                    fun failing_paths (job,target) ->
                        let partial_failing_paths = 
                          match target.Prioritizer.target_predicate with
                          | Prioritizer.FailingCondition (_,_) -> failwith "Not implemented yet"
                          | Prioritizer.FailingPaths (paths) ->  
                              let results : 'reason job_completion list = forward_otter_bounded_by_paths job paths in
                                List.fold_left (fun failing_paths result ->
                                    match result with 
                                    | Abandoned (`FailingPaths(paths), _, _) ->
                                        (* Insert a "function call decision" for function call *)
                                        let paths = 
                                            List.map (fun path -> ForkFunptr(instr,target.Prioritizer.target_func)::path) paths
                                        in List.rev_append paths failing_paths
                                    | _ -> failing_paths
                                ) [] results
                        in
                          List.rev_append partial_failing_paths failing_paths
                ) [] targetted_jobs
            in
			  if List.length failing_paths = 0 then
			  	None
			  else 
			  	let result = { 
			  		result_file = job.Job.file; 
			  		result_state = job.state;  (* TODO (martin): state before or after evaluating exps? *)
			  		result_history = job.exHist; 
			  		result_decision_path = job.decisionPath; 
                    (* joining decisionPath with failing_paths forms the failng paths of the new target *)
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
	Output.banner_printf 1 "Start forward SE on function %s with target(s)\n%s\n%!"
	  (fn.svar.vname) 
      (let s=(String.concat "," (List.map (fun t -> t.Prioritizer.target_func.svar.vname) ts)) in if s="" then "(none)" else s)
  in

  (* Run forward SE on the examining function (job), given the targets. *)
  let call_Otter_main_loop targets job =
    let (@@) = Interceptor.(@@) in
	let jobs = new prioritized_job_queue assertfn targets in
	jobs#queue job;
	Driver.main_loop
	  (fun jobs -> jobs#get)
	  (
		Interceptor.set_output_formatter_interceptor @@
		(test_job_at_targets_interceptor targets) @@
		BuiltinFunctions.interceptor @@
		Statement.step
	  )
	  process_result
	  jobs
  in

  (* compute call graph and provide a helper function for finding callers *)
  (* TODO (martin): move this to its own helper module *)
  let callgraph = Callgraph.computeGraph file in
  let get_callers f =
    let function_node = Hashtbl.find callgraph f.svar.vname in
    let callers_hash = function_node.Callgraph.cnCallers in
    let caller_nodes = Inthash.tolist callers_hash in
    List.fold_left begin fun callers (_, caller) -> match caller.Callgraph.cnInfo with
        | Callgraph.NIVar (v, defined) when !defined -> (FindCil.fundec_by_varinfo file v)::callers
        | _ -> callers
    end [] caller_nodes
  in

  (* The implementation of main loop *)
  let rec callchain_backward_main_loop job targets =
	(* Assume we start at f *)
	let f = List.hd job.state.callstack in
	(* Run forward SE based on the targets *)
	let results : 'reason job_completion list = call_Otter_main_loop targets job in
	  if f == entryfn then 
		(* If f is main(), we are done *)
		results
	  else
        let join_paths prefix suffixes =
            List.map (fun suffix -> List.append prefix suffix) suffixes
        in
        let failing_paths = List.fold_left 
          ( fun all_paths job_completion ->
              match job_completion with
              | Abandoned (`FailingPaths(paths),_,job_result) -> 
                      let joined_paths = join_paths job_result.result_decision_path paths in
                        List.rev_append joined_paths all_paths
              | _ -> all_paths
          ) [] results
        in
        (* TODO (martin): based on the properties of the function, and the failing_paths,
         *                decide if the failing predicate is a condition or paths
         *)
		let new_target = {
		  Prioritizer.target_func = f;
          Prioritizer.target_predicate = Prioritizer.FailingPaths(failing_paths);
		} in
		let callers = get_callers f in
		  Output.banner_printf 1 "Function %s's caller(s): " f.svar.vname;
		  List.iter (fun caller -> Output.banner_printf 1 " %s\n" caller.svar.vname) callers;
		  Output.banner_printf 1 "%!";
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
      Prioritizer.target_func = assertfn;
      Prioritizer.target_predicate = Prioritizer.FailingPaths([]);
  } in
  let callers = get_callers assertfn in
	List.fold_left 
	  (fun results caller ->
		 Output.banner_printf 2 "Call-chain backward Symbolic Execution of target function %s\n%!" caller.svar.vname;
         let targets = [assert_target] in
         let _ = print_targets caller targets in
		 let job = job_init caller in 
		 let new_result = callchain_backward_main_loop job targets in
		   new_result::results
	  ) [] callers



(* Cil feature for call-chain backwards Otter *)
let arg_assertfn = ref "__ASSERT"

let prepare_file file =
	Executeargs.arg_cfg_pruning := true;
	Driver.prepare_file file

let doit file =
	(* TODO: do something about signal handlers/run statistics from Executemain.doExecute *)

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
	Output.printf "%s@\n" (Executedebug.get_log ());
	List.iter (fun result -> Report.print_report result) results


let feature = {
	Cil.fd_name = "backotter";
	Cil.fd_enabled = ref false;
	Cil.fd_description = "Call-chain backwards symbolic executor for C";
	Cil.fd_extraopt = [
		("--assertfn",
		Arg.Set_string arg_assertfn,
		"<fname> Assertion function to look for in the call-chain-backward mode (default: __ASSERT) \n");
	];
	Cil.fd_post_check = true;
	Cil.fd_doit = doit
}
