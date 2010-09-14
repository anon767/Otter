open DataStructures
open OcamlUtilities
open OcamlBase
open CilUtilities
open OtterBytes
open OtterCore
open Bytes
open Types
open Cil
open Executeargs


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


let pass_targets targets job fexp exps =
  (* convert fexp to fundec *) 
  let state = job.state in
  let hist = job.exHist in
  let fundecs = 
	List.fold_left 
	  ( fun lst (_,ft) -> ft::lst
	  ) [] (Statement.function_from_exp job state fexp exps) in
  (* convert args to bytes. argvs are from left to right *)
  let _, argvs = 
	List.fold_right 
	  ( fun exp (state, argvs) ->
		  let state, bytes = Expression.rval state exp in
			(state, bytes::argvs)
	  ) exps (state, []) 
  in
  (* check if all fundecs pass target *)
  let pass_target target : bool*bytes =
	List.fold_left 
	  ( fun (b,fc) fundec -> 
		  if fundec != target.Prioritizer.func then true,Bytes.bytes__zero
		  else
			(
			  (* caller's input values to callee: argvs
			   * callee's input values from caller: target.state 
			   * at the end, use caller's state as the state of quirying 
			   *)
			  Output.banner_printf 0 "Check if the failing condition is hit\n%!";
			  (* TODO: add globals *)
			  let connecting_bytes = 
				List.fold_left2
				  ( fun b argv formal ->
					  let _,fargv = Expression.rval target.Prioritizer.entry_state (Lval(Var(formal),NoOffset)) in
					  let equation = Operator.eq [(fargv,formal.vtype);(argv,formal.vtype)] in
						Operator.bytes__land equation b 
				  ) bytes__one argvs fundec.sformals
			  in
                (Format.printf "Failing condition: %a\n%!" BytesPrinter.bytes target.Prioritizer.failing_condition);
                (Format.printf "Path condition: %!");
                (List.iter (Format.printf "%a && %!" BytesPrinter.bytes) state.path_condition);
                (Format.printf "\n%!");
                (Format.printf "Connection : %a\n%!" BytesPrinter.bytes connecting_bytes);
			  let _, truth = MemOp.eval_with_cache state (connecting_bytes::state.path_condition)  (Operator.bytes__not target.Prioritizer.failing_condition) in
			  let total_failing_condition = Operator.bytes__land target.Prioritizer.failing_condition connecting_bytes in
			  let total_failing_condition = Operator.bytes__lor total_failing_condition fc in

			  let print_failed_assertion isUnknown =
				let _ = Output.set_mode Output.MSG_MUSTPRINT in
				let caller = List.hd state.callstack in
				let mustmay = (if isUnknown then "may" else "must") in
				let log format = FormatPlus.ksprintf Executedebug.log format in
				let _ = Output.banner_printf 1  "Failing condition %s be hit (see error log).\n%!" mustmay in
				let _ = log "(****************************@\n" in
				let _ = log "The following failure %s happen in function %s:@\n" mustmay caller.svar.vname in
				let _ = log "Failing condition:@;<1 2>@[%a@]@\n" BytesPrinter.bytes target.Prioritizer.failing_condition in
				let _ = log "Path condition:@;<1 2>@[  %a@]@\n" (FormatPlus.pp_print_list BytesPrinter.bytes "@\nAND@\n  ") state.path_condition in
				let _ = log "Connection:@;<1 2>@[%a@]@\n" BytesPrinter.bytes connecting_bytes in
				let _ = log "Consult STP for an example...@\n" in
				let valuesForSymbols = Stp.getAllValues (target.Prioritizer.failing_condition::connecting_bytes::state.path_condition) in
				let getVal = function
				  | Bytes_ByteArray bytArr ->
					  let byteOptArray =
						ImmutableArray.map
						  (function
							 | Byte_Symbolic s ->
								 (try
									let valueForS = List.assq s valuesForSymbols in
									  Some (make_Byte_Concrete valueForS)
								  with Not_found -> None
								 )
							 | _ -> failwith "Impossible: tracked symbolic value must be fully symbolic"
						  )
						  bytArr
					  in
						if ImmutableArray.exists (* Check if any byte is constrained *)
							 (function Some _ -> true | _ -> false)
							 byteOptArray
						then (Some (make_Bytes_ByteArray
								  (ImmutableArray.map
									 (function Some b -> b | None -> byte__zero)
									 byteOptArray))
						) 
						else None
				  | _ -> failwith "Impossible: symbolic bytes must be a ByteArray"
				in
				let _ = List.iter 
						  ( fun (bytes,varinf) -> 
							  match getVal bytes with 
								| None -> () 
								| Some concreteByteArray -> 
									(
									  match bytes_to_constant concreteByteArray varinf.vtype with
										| CInt64 (n,_,_) ->
											log "%s=%Ld@\n" varinf.vname n
										| _ -> failwith "Unimplemented: non-integer symbolic"
									)
						  )
						  hist.bytesToVars
				in
				let _ = log "(****************************@\n" in
				  ()
			  in
				match truth with 
				  | Ternary.True -> true,Bytes.bytes__zero
				  | Ternary.Unknown -> 
					  print_failed_assertion true; false,total_failing_condition
				  | Ternary.False -> 
					  print_failed_assertion false; false,total_failing_condition
			)
	  ) (true,Bytes.bytes__zero) fundecs
  in
  let rec pass_targets_impl targets =
	match targets with 
	  | [] -> true,Bytes.bytes__zero
	  | t::ts -> 
		  let truth,failing_condition = pass_target t in
		  if truth then pass_targets_impl ts 
		  else false,failing_condition (* TODO: can proceed, to find more failing targets *)
  in
	pass_targets_impl targets

let terminate_job_at_targets targets job =
	(* if job meets one of the targets, do checking *)
	(* if fails, return Some (Complete Abandoned) *)
	match job.instrList with
		| Call(_,fexp,exps,loc)::_ ->
			let truth, failing_condition = pass_targets targets job fexp exps in
			if truth then
				None
			else begin
				let msg = Printf.sprintf "Job %d hits the failing condition" job.jid in
				if run_args.arg_failfast then failwith msg;
				let state = { job.state with path_condition = failing_condition::job.state.path_condition } in
				let result = { result_file = job.Types.file; result_state = state; result_history = job.exHist } in
				Some (Complete (Types.Abandoned (`Failure msg, loc, result)))
			end
		| _ ->
			None

let terminate_job_at_targets_interceptor targets job job_queue interceptor =
	match terminate_job_at_targets targets job with
		| Some result ->
			(result, job_queue)
		| None ->
			interceptor job job_queue

let callchain_backward_se file entryfn assertfn job_init : _ job_completion list list =
  let job_init fn ts =
	let _ = Output.banner_printf 1 "Start forward SE on function %s with target(s)\n%s\n%!"
			(fn.svar.vname) (let s=(String.concat "," (List.map (fun t -> t.Prioritizer.func.svar.vname) ts)) in if s="" then "(none)" else s)
	in
	job_init fn
  in
  let get_failing_condition result = 
	List.fold_left 
	  ( fun b job_completion ->
		  match job_completion with
			| Abandoned (_,_,job_result) ->
				let this_fc = List.fold_left Operator.bytes__land Bytes.bytes__one job_result.result_state.path_condition in
				  Operator.bytes__lor b this_fc 
			| _ -> b
	  )
	  Bytes.bytes__zero result
  in

  let (@@) = Interceptor.(@@) in
  let call_Otter_main_loop targets job =
	let jobs = new prioritized_job_queue assertfn targets in
	jobs#queue job;
	Driver.main_loop
	  (fun jobs -> jobs#get)
	  (
		Interceptor.set_output_formatter_interceptor @@
		(terminate_job_at_targets_interceptor targets) @@
		BuiltinFunctions.interceptor @@
		Statement.step
	  )
	  process_result
	  jobs
  in

  (* compute call graph and provide a helper function for finding callers *)
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
	let result = call_Otter_main_loop targets job in
	(* result is a (may not be completed) list of finished jobs.
	 * A job is either successful, if no assertion failure, or unsuccessful.
	 *)
	(* Get a failing condition *)
	let failing_condition = get_failing_condition result in
	  if f == entryfn then 
		(* If f is main(), we are done *)
		result 
	  else
		let new_target = {
		  Prioritizer.func = f;
		  Prioritizer.entry_state = job.state;
		  Prioritizer.failing_condition = failing_condition;
		} in
		let callers = get_callers f in
		  Output.banner_printf 1 "Function %s's caller(s): " f.svar.vname;
		  List.iter (fun caller -> Output.banner_printf 1 " %s\n" caller.svar.vname) callers;
		  Output.banner_printf 1 "%!";
		  List.fold_left 
		  (
			fun lst caller -> 
			  let targets = (new_target::targets) in
			  let newjob = job_init caller targets in
			  let newlst = callchain_backward_main_loop newjob targets  in
				List.rev_append newlst lst
		  ) 
		  [] callers
  in
  let callers = get_callers assertfn in
	List.fold_left 
	  (fun results caller ->
		 Output.banner_printf 2 "Call-chain backward Symbolic Execution of target function %s\n%!" caller.svar.vname;
		 let job = job_init caller [] in 
		 let new_result = callchain_backward_main_loop job [] in
		   new_result::results
	  ) [] callers



(* Cil feature for call-chain backwards Otter *)

let arg_assertfn = ref "__ASSERT"

let prepare_file file =
	run_args.arg_cfg_pruning <- true;
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
	let job_init = fun entryfn -> Driver.job_for_middle file entryfn in
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
