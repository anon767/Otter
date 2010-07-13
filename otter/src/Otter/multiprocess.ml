
(* TODO: implement shared memory by one of the following:
 * - move block_to_bytes into shared_state, and add some sort of copy-on-write/indirection mechanism
 * - add a table to shared_state to keep track of shared locations, and copy them between processes at each step
 *)

type program_counter = {
	instrList : Cil.instr list;
	stmt : Cil.stmt;
}

type local_state = {
	global : Types.memory_frame;
	formals : Types.memory_frame list;
	locals : Types.memory_frame list;
	callstack : Cil.fundec list;
	callContexts : Types.callingContext list;
	va_arg : Bytes.bytes list list;
	va_arg_map : Bytes.bytes list Types.VargsMap.t;
	block_to_bytes : Bytes.bytes Types.deferred Types.MemoryBlockMap.t;
}

type shared_state = {
	path_condition : Bytes.bytes list;
}

type multijob = {
	processes : (program_counter * local_state) list;
	shared : shared_state;
	jid : int;
}


(* put a job back into the multijob and update the shared state *)
let put_job job multijob =
	let program_counter = {
		instrList = job.Types.instrList;
		stmt = job.Types.stmt;
	} in
	let process = {
		global = job.Types.state.Types.global;
		formals = job.Types.state.Types.formals;
		locals = job.Types.state.Types.locals;
		callstack = job.Types.state.Types.callstack;
		callContexts = job.Types.state.Types.callContexts;
		va_arg = job.Types.state.Types.va_arg;
		va_arg_map = job.Types.state.Types.va_arg_map;
		block_to_bytes = job.Types.state.Types.block_to_bytes;
	} in
	let shared = {
		path_condition = job.Types.state.Types.path_condition;
	} in
	{
		processes = List.append multijob.processes [ (program_counter, process) ];
		shared = shared;
		jid = job.Types.jid;
	}


(* update the multijob with a completed job *)
let put_completion completion multijob = match completion with
	| Types.Return (_, job_result)
	| Types.Exit (_, job_result)
	| Types.Abandoned (_, _, job_result) ->
		let shared = {
			path_condition = job_result.Types.result_state.Types.path_condition;
		} in
		{ multijob with
			shared = shared;
		}
	| Types.Truncated _ ->
		failwith "TODO"


(* get a job from a multijob *)
let get_job multijob = match multijob.processes with
	| [] ->
		None
	| (program_counter, process)::processes ->
		(* extract the first job from a multijob *)
		let state = {
			Types.global = process.global;
			Types.formals = process.formals;
			Types.locals = process.locals;
			Types.callstack = process.callstack;
			Types.callContexts = process.callContexts;
			Types.va_arg = process.va_arg;
			Types.va_arg_map = process.va_arg_map;
			Types.block_to_bytes = process.block_to_bytes;
			Types.path_condition = multijob.shared.path_condition;
			(* TODO *)
			Types.extra = Types.VarinfoMap.empty;
			Types.malloc = Types.VarinfoMap.empty;
			Types.path_condition_tracked = [];
			Types.loc_map = Types.LocMap.empty;
			Types.bytes_eval_cache = Types.BytesMap.empty;
		} in
		let job = {
			Types.state = state;
			Types.exHist = Types.emptyHistory;
			Types.instrList = program_counter.instrList;
			Types.stmt = program_counter.stmt;
			Types.jid = multijob.jid;
			(* TODO *)
			Types.inTrackedFn = false;
			Types.mergePoints = Types.StmtInfoSet.empty;
		} in
		let multijob = { multijob with
			processes = processes;
		} in
		Some (job, multijob)

let intercept_fork job job_queue interceptor =
	let job, multijob = job in
	match job.Types.instrList with
		| Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), _, _)::_ when varinfo.Cil.vname = "fork" -> 
			(* update the program counter *)
			assert(List.length job.Types.instrList = 1);
			let next_stmt = match job.Types.stmt.Cil.succs with
				| [ h ] -> h
				| _ -> failwith "Impossible!"
			in
			let job = { job with Types.instrList = []; Types.stmt = next_stmt; } in

			(* TODO:
			 * - keep track of pids symbolically
			 * - be careful to aquire the pid *before* cloning the job, so that the states are consistent
			 *)

			(* clone the job *)
			let job, child_job = match retopt with
				| None ->
					(job, job)
				| Some cil_lval ->
					let child_job = { job with
						(* TODO: make the pid symbolic *)
						Types.state =
							let state, lval = Eval.lval job.Types.state cil_lval in
							MemOp.state__assign state lval (Bytes.bytes__one)
					} in
					let job = { job with
						Types.state =
							let state, lval = Eval.lval job.Types.state cil_lval in
							MemOp.state__assign state lval (Bytes.bytes__zero)
					} in
					(job, child_job)
			in
			let multijob = (put_job child_job multijob) in
			interceptor (job, multijob) job_queue
		| _ -> 
			interceptor (job, multijob) job_queue

let rec get_job_multijob job_queue = 
	match job_queue with
		| [] -> None
		| multijob::t ->
			match get_job multijob with
				| None -> get_job_multijob t
				| Some job -> Some (job, t)

(* process the results *)
let rec process_job_states result multijob completed multijob_queue =
	match result with
		| Types.Active job ->
			(* put the job back into the multijob and queue it *)
			let multijob = put_job job multijob in
			(completed, (multijob::multijob_queue))
		| Types.Big_Fork states ->
			(* process all forks *)
			List.fold_left begin fun (completed, multijob_queue) state ->
				process_job_states state multijob completed multijob_queue
			end (completed, multijob_queue) states
		| Types.Complete completion ->
			(* store the results *)
			let multijob = put_completion completion multijob in
			let completion = match completion with
				| Types.Abandoned (msg, loc, job_result) ->
					Output.set_mode Output.MSG_MUSTPRINT;
					(Output.printf 
						"Error \"%s\" occurs at %s\nAbandoning path\n"
						msg (To_string.location loc));
					Types.Abandoned (msg ^ (Printexc.get_backtrace ()), loc, job_result)
				| _ ->
					completion
			in
			((completion::completed), (multijob::multijob_queue))

		| _ ->
			(completed, multijob_queue)

let repack_job_interceptor job job_queue interceptor =
	let job, multijob = job in
	interceptor job (multijob, job_queue)

let process_result result completed job_queue =
	let multijob, multijob_queue = job_queue in
	process_job_states result multijob completed multijob_queue

let (@@) i1 i2 = 
	fun a b -> i1 a b i2

let init job = 
	let multijob = {
		processes = [];
		shared = {
			path_condition = [];
		};
		jid = job.Types.jid;
	} in
	let multijob = put_job job multijob in

	(* start executing *)
	Driver.main_loop 
		get_job_multijob
		(
			intercept_fork @@
			repack_job_interceptor @@
			Driver.intercept_extended_otter_functions @@ 
			Driver.otter_core_interceptor
		)
		process_result
		[ multijob ]

let doit file =
	(* TODO: do something about signal handlers/run statistics from Executemain.doExecute *)

	Executemain.prepare_file file;
	let entryfn = Executemain.find_entryfn file in
	let job =
		if Executeargs.run_args.Executeargs.arg_entryfn = "main" then
			(* create a job for the file, with the commandline arguments set to the file name
			 * and the arguments from the '--arg' option *)
			Executemain.job_for_file file (file.Cil.fileName::Executeargs.run_args.Executeargs.arg_cmdline_argvs)
		else
			(* create a job to start in the middle of entryfn *)
			Executemain.job_for_middle file entryfn Executeargs.run_args.Executeargs.arg_yaml
	in

	Cilutility.init_funt_table file;
	(* run the job *)
	let result = init job in

	(* print the results *)
	Output.print_endline (Executedebug.get_log ());
	Report.print_report result


(* Cil feature for multi-process Otter *)
let feature = {
	Cil.fd_name = "multiotter";
	Cil.fd_enabled = ref false;
	Cil.fd_description = "Multi-process symbolic executor for C";
	Cil.fd_extraopt = [];
	Cil.fd_post_check = true;
	Cil.fd_doit = doit
}
