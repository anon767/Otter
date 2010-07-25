
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
	stmtPtrs : Types.callingContext Utility.IndexMap.t;
	va_arg : Bytes.bytes list list;
	va_arg_map : Bytes.bytes list Types.VargsMap.t;
	block_to_bytes : Bytes.bytes Types.deferred Types.MemoryBlockMap.t;
	pid : int;
}

type shared_state = {
	path_condition : Bytes.bytes list;
	shared_block_to_bytes : Bytes.bytes Types.deferred Types.MemoryBlockMap.t;
}

type multijob = {
	file : Cil.file;
	processes : (program_counter * local_state) list;
	shared : shared_state;
	jid : int;
	next_pid : int;
	current_pid : int;
}

let update_to_shared_memory shared_block_to_bytes local_block_to_bytes =
	let map_func key value =
		if Types.MemoryBlockMap.mem key local_block_to_bytes then
			Types.MemoryBlockMap.find key local_block_to_bytes
		else
			value (* process lost the shared memory binding *)
	in
	Types.MemoryBlockMap.mapi map_func shared_block_to_bytes

let update_from_shared_memory shared_block_to_bytes local_block_to_bytes =
	let map_func key value =
		if Types.MemoryBlockMap.mem key shared_block_to_bytes then
			Types.MemoryBlockMap.find key shared_block_to_bytes
		else
			value (* not shared memory *)
	in
	Types.MemoryBlockMap.mapi map_func local_block_to_bytes

(* put a job back into the multijob and update the shared state *)
let put_job job multijob pid =
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
		stmtPtrs = job.Types.state.Types.stmtPtrs;
		va_arg = job.Types.state.Types.va_arg;
		va_arg_map = job.Types.state.Types.va_arg_map;
		block_to_bytes = job.Types.state.Types.block_to_bytes;
		pid = pid;
	} in
	let shared = {
		path_condition = job.Types.state.Types.path_condition;
		shared_block_to_bytes = update_to_shared_memory multijob.shared.shared_block_to_bytes job.Types.state.Types.block_to_bytes;
	} in
	{
		file = job.Types.file;
		processes = List.append multijob.processes [ (program_counter, process) ];
		shared = shared;
		jid = job.Types.jid;
		next_pid = multijob.next_pid;
		current_pid = multijob.current_pid;
	}


(* update the multijob with a completed job *)
let put_completion completion multijob = match completion with
	| Types.Return (_, job_result)
	| Types.Exit (_, job_result)
	| Types.Abandoned (_, _, job_result) ->
		let shared = {
			path_condition = job_result.Types.result_state.Types.path_condition;
			shared_block_to_bytes = update_to_shared_memory 
				multijob.shared.shared_block_to_bytes 
				job_result.Types.result_state.Types.block_to_bytes;
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
			Types.stmtPtrs = process.stmtPtrs;
			Types.va_arg = process.va_arg;
			Types.va_arg_map = process.va_arg_map;
			Types.block_to_bytes = update_from_shared_memory multijob.shared.shared_block_to_bytes process.block_to_bytes;
			Types.path_condition = multijob.shared.path_condition;
			(* TODO *)
			Types.extra = Types.VarinfoMap.empty;
			Types.malloc = Types.VarinfoMap.empty;
			Types.path_condition_tracked = [];
			Types.bytes_eval_cache = Types.BytesMap.empty;
		} in
		let job = {
			Types.file = multijob.file;
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
			current_pid = process.pid;
		} in
		Some (job, multijob)


class multiprocess_formatter = fun jid pid cur_loc ->
	object (this)
		inherit Output.formatter_base

		val jid : int = jid
		val pid : int = pid
		val cur_loc : Cil.location = cur_loc

		method private print_loc loc = 
			if loc==Cil.locUnknown then "" else
			loc.Cil.file^":"^(string_of_int loc.Cil.line)^" : "
		method private label () = 
			Format.sprintf "[jid: %d, pid: %d] %s" jid pid (this#print_loc (cur_loc))
		method format_str str = 
			let rec impl str = 
				if String.length str = 0 then
					""
				else if String.contains str '\n' then
				  	let i = String.index str '\n' in
				  	let s1 = String.sub str 0 i in
				  	let s2 = String.sub str (i+1) ((String.length str) - i - 1) in
				    	(this#label())^s1^"\n"^(impl s2)
				else
					(this#label())^str
			in
			impl str
	end

let multi_set_output_formatter_interceptor job job_queue interceptor = 
	let j, m = job in
	Output.formatter := ((new multiprocess_formatter m.jid m.current_pid (Core.get_job_loc j)) 
		:> Output.formatter_base);
	interceptor job job_queue

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

			Output.set_mode Output.MSG_REG;
			Output.print_endline (Format.sprintf "fork(): parent: %d, child: %d" multijob.current_pid multijob.next_pid);

			(* clone the job *)
			let job, child_job = match retopt with
				| None ->
					(job, job)
				| Some cil_lval ->

					let child_job = { job with
						(* TODO: make the pid symbolic *)
						Types.state =
							let state, lval = Eval.lval job.Types.state cil_lval in
							MemOp.state__assign state lval (Bytes.int_to_bytes multijob.next_pid)
					} in
					let job = { job with
						Types.state =
							let state, lval = Eval.lval job.Types.state cil_lval in
							MemOp.state__assign state lval (Bytes.bytes__zero)
					} in
					(job, child_job)
			in
			let multijob = (put_job child_job multijob multijob.next_pid) in
			let multijob = {multijob with next_pid = multijob.next_pid + 1 } in
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
			let multijob = put_job job multijob multijob.current_pid in
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

let init job = 
	let multijob = {
		file = job.Types.file;
		processes = [];
		shared = {
			path_condition = [];
			shared_block_to_bytes = Types.MemoryBlockMap.empty;
		};
		jid = job.Types.jid;
		next_pid = 1;
		current_pid = 0;
	} in
	let multijob = put_job job multijob 0 in

	(* start executing *)
	let (@@) = Interceptors.(@@) in
	Driver.main_loop 
		get_job_multijob
		(
			multi_set_output_formatter_interceptor @@
			intercept_fork @@
			repack_job_interceptor @@
			Builtin_function.interceptor @@ 
			Core.step
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
