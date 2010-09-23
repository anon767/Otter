open DataStructures
open OcamlUtilities
open OtterBytes
open OtterCore
open Types

let (@@) = Interceptor.(@@)
let (@@@) i1 i2 = fun a b c -> i1 a b c i2

let intercept_multi_function_by_name_internal target_name replace_func job multijob job_queue interceptor =
	(* Replace a C function with Otter code *)
	(* replace_func retopt exps loc job job_queue *)
	match job.instrList with
		| Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc)::_ when varinfo.Cil.vname = target_name ->
			let (job_result, multijob) = replace_func job multijob retopt exps in
				(job_result, (multijob, job_queue))
		| _ -> 
			interceptor job multijob job_queue

type program_counter = {
	instrList : Cil.instr list;
	stmt : Cil.stmt;
}

type local_state = {
	global : memory_frame;
	formals : memory_frame list;
	locals : memory_frame list;
	callstack : Cil.fundec list;
	callContexts : callingContext list;
	stmtPtrs : callingContext IndexMap.t;
	va_arg : Bytes.bytes list list;
	va_arg_map : Bytes.bytes list VargsMap.t;
	block_to_bytes : (state, Bytes.bytes) Deferred.t MemoryBlockMap.t;
	pid : int;
}

type shared_state = {
	path_condition : Bytes.bytes list;
	shared_block_to_bytes : (state, Bytes.bytes) Deferred.t MemoryBlockMap.t;
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
		if MemoryBlockMap.mem key local_block_to_bytes then
			MemoryBlockMap.find key local_block_to_bytes
		else
			value (* process lost the shared memory binding *)
	in
	MemoryBlockMap.mapi map_func shared_block_to_bytes

let update_from_shared_memory shared_block_to_bytes local_block_to_bytes =
	let map_func key value =
		if MemoryBlockMap.mem key shared_block_to_bytes then
			MemoryBlockMap.find key shared_block_to_bytes
		else
			value (* not shared memory *)
	in
	MemoryBlockMap.mapi map_func local_block_to_bytes

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
	| Return (_, job_result)
	| Exit (_, job_result)
	| Abandoned (_, _, job_result) ->
		let shared = {
			path_condition = job_result.Types.result_state.Types.path_condition;
			shared_block_to_bytes = update_to_shared_memory 
				multijob.shared.shared_block_to_bytes 
				job_result.Types.result_state.Types.block_to_bytes;
		} in
		{ multijob with
			shared = shared;
		}
	| Truncated _ ->
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
			Types.aliases = VarinfoMap.empty;
			Types.mallocs = MallocMap.empty;
			Types.path_condition_tracked = [];
			Types.bytes_eval_cache = BytesMap.empty;
		} in
		let job = {
			Types.file = multijob.file;
			Types.state = state;
			Types.exHist = emptyHistory;
            Types.decisionPath = [];
			Types.instrList = program_counter.instrList;
			Types.stmt = program_counter.stmt;
			Types.jid = multijob.jid;
			(* TODO *)
			Types.trackedFns = StringSet.empty;
			Types.inTrackedFn = false;
		} in
		let multijob = { multijob with
			processes = processes;
			current_pid = process.pid;
		} in
		Some (job, multijob)


let multi_set_output_formatter_interceptor job multijob job_queue interceptor = 
	let loc = Statement.get_job_loc job in
	let label =
		if loc = Cil.locUnknown then
			Format.sprintf "[jid: %d, pid: %d] : " multijob.jid multijob.current_pid
		else
			Format.sprintf "[jid: %d, pid: %d] %s:%d : " multijob.jid multijob.current_pid loc.Cil.file loc.Cil.line
	in
	Output.set_formatter (new Output.labeled label);
	interceptor job multijob job_queue

let libc_fork job multijob retopt exps =
	(* update instruction pointer, history, and such *)
	let job = BuiltinFunctions.end_function_call job in

	Output.set_mode Output.MSG_REG;
	Output.printf "fork(): parent: %d, child: %d@\n" multijob.current_pid multijob.next_pid;

	(* clone the job *)
	let job, child_job = match retopt with
		| None ->
			(job, job)
		| Some cil_lval ->

			let child_job = { job with
				(* TODO: make the pid symbolic *)
				state =
					let state, lval = Expression.lval job.state cil_lval in
					MemOp.state__assign state lval (Bytes.int_to_bytes multijob.next_pid)
			} in
			let job = { job with
				state =
					let state, lval = Expression.lval job.state cil_lval in
					MemOp.state__assign state lval (Bytes.bytes__zero)
			} in
			(job, child_job)
	in
	let multijob = (put_job child_job multijob multijob.next_pid) in
	let multijob = {multijob with next_pid = multijob.next_pid + 1 } in
	(Active job, multijob)

(* allocates on the global heap *)
let otter_gmalloc_size (state:Types.state) size bytes loc =
	let name = FormatPlus.sprintf "%s(%d)#%d/%a%s"
		(List.hd state.Types.callstack).Cil.svar.Cil.vname
		size
		(DataStructures.Counter.next BuiltinFunctions.libc___builtin_alloca__id)
		Printcil.loc loc
		(MemOp.state__trace state)
	in
	let block = Bytes.block__make name size Bytes.Block_type_Heap in
	let addrof_block = Bytes.make_Bytes_Address (block, Bytes.bytes__zero) in
	let state = MemOp.state__add_block state block bytes in
	(state, block, addrof_block)

let otter_gmalloc job multijob retopt exps =
	let state, b_size = Expression.rval job.Types.state (List.hd exps) in
	let size =
		if Bytes.isConcrete_bytes b_size then
			Bytes.bytes_to_int_auto b_size (*safe to use bytes_to_int as arg should be small *)
		else
			1 (* currently bytearray have unbounded length *)
	in
	let bytes =
	  if !Executeargs.arg_init_malloc_zero
	  then Bytes.bytes__make size (* initially zero, as though malloc were calloc *)
	  else Bytes.bytes__make_default size Bytes.byte__undef (* initially the symbolic 'undef' byte *)
	in
	let state, block, bytes = otter_gmalloc_size state size bytes (Statement.get_job_loc job) in
	let job = BuiltinFunctions.end_function_call { job with state = BuiltinFunctions.set_return_value state retopt bytes } in

	let rec push_to_all procs block bytes =
		match procs with
			| [] -> []
			| (pc, ls)::t -> 
				let ls = { ls with block_to_bytes = MemoryBlockMap.add block (Deferred.Immediate bytes) ls.block_to_bytes; } in
				(pc, ls)::(push_to_all t block bytes)
	in
	let multijob = 
		{multijob with
			shared = 
				{multijob.shared with
					shared_block_to_bytes = MemoryBlockMap.add block (Deferred.Immediate bytes) multijob.shared.shared_block_to_bytes;
				};
			processes = push_to_all multijob.processes block bytes;
		}
	in

	(Active job, multijob)

let rec get_job_multijob job_queue = 
	match job_queue with
		| [] -> None
		| multijob::t ->
			match get_job multijob with
				| None -> get_job_multijob t
				| Some (job, multijob) -> Some (job, (multijob, t))

(* process the results *)
let rec process_job_states result multijob completed multijob_queue =
	match result with
		| Active job ->
			(* put the job back into the multijob and queue it *)
			let multijob = put_job job multijob multijob.current_pid in
			(completed, (multijob::multijob_queue))
		| Fork states ->
			(* process all forks *)
			List.fold_left begin fun (completed, multijob_queue) state ->
				process_job_states state multijob completed multijob_queue
			end (completed, multijob_queue) states
		| Complete completion ->
			(* store the results *)
			let multijob = put_completion completion multijob in
			begin match completion with
				| Abandoned (reason, loc, job_result) ->
					Output.set_mode Output.MSG_MUSTPRINT;
					Output.printf
						"Error \"%a\" occurs at %a.@\nAbandoning path.@\n"
						Report.abandoned_reason reason Printcil.loc loc
				| _ ->
					()
			end;
			((completion::completed), (multijob::multijob_queue))

		| _ ->
			(completed, multijob_queue)

let unpack_job_interceptor job job_queue interceptor =
	let multijob, job_queue = job_queue in
	interceptor job multijob job_queue

let repack_job_interceptor job multijob job_queue interceptor =
	interceptor job (multijob, job_queue)

let process_result result completed job_queue =
	let multijob, multijob_queue = job_queue in
	process_job_states result multijob completed multijob_queue

let run job = 
	let multijob = {
		file = job.Types.file;
		processes = [];
		shared = {
			path_condition = [];
			shared_block_to_bytes = MemoryBlockMap.empty;
		};
		jid = job.Types.jid;
		next_pid = 1;
		current_pid = 0;
	} in
	let multijob = put_job job multijob 0 in

	(* start executing *)
	Driver.main_loop 
		get_job_multijob
		(
			unpack_job_interceptor @@
			multi_set_output_formatter_interceptor @@@
			(intercept_multi_function_by_name_internal "fork"                       libc_fork) @@@
			(intercept_multi_function_by_name_internal "__otter_multi_gmalloc"      otter_gmalloc) @@@
			repack_job_interceptor @@@
			BuiltinFunctions.interceptor @@ 
			BuiltinFunctions.libc_interceptor @@
			Statement.step
		)
		process_result
		[ multijob ]

let doit file =
	(* TODO: do something about signal handlers/run statistics from Executemain.doExecute *)

	Driver.prepare_file file;
	let entryfn = Driver.find_entryfn file in
	let job =
		if !Executeargs.arg_entryfn = "main" then
			(* create a job for the file, with the commandline arguments set to the file name
			 * and the arguments from the '--arg' option *)
			Driver.job_for_file file (file.Cil.fileName::!Executeargs.arg_cmdline_argvs)
		else
			(* create a job to start in the middle of entryfn *)
			Driver.job_for_middle file entryfn
	in

	(* run the job *)
	let result = run job in

	(* print the results *)
	Output.printf "%s@\n" (Executedebug.get_log ());
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
