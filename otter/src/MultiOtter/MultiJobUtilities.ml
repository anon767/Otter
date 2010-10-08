open OtterCore
open OtterBytes
open OtterJob
open MultiTypes
open Bytes
open Types
open Job

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
		MultiTypes.instrList = job.Job.instrList;
		stmt = job.Job.stmt;
	} in
	let process = {
		MultiTypes.global = job.Job.state.Types.global;
		formals = job.Job.state.Types.formals;
		locals = job.Job.state.Types.locals;
		callstack = job.Job.state.Types.callstack;
		callContexts = job.Job.state.Types.callContexts;
		stmtPtrs = job.Job.state.Types.stmtPtrs;
		va_arg = job.Job.state.Types.va_arg;
		va_arg_map = job.Job.state.Types.va_arg_map;
		block_to_bytes = job.Job.state.Types.block_to_bytes;
		pid = pid;
	} in
	let shared = {
		MultiTypes.path_condition = job.Job.state.Types.path_condition;
		shared_block_to_bytes = update_to_shared_memory multijob.shared.shared_block_to_bytes job.Job.state.Types.block_to_bytes;
	} in
	{
		MultiTypes.file = job.Job.file;
		processes = List.append multijob.processes [ (program_counter, process) ];
		shared = shared;
		jid = job.Job.jid;
		next_pid = multijob.next_pid;
		current_pid = multijob.current_pid;
	}


(* update the multijob with a completed job *)
let put_completion completion multijob = match completion with
	| Return (_, job_result)
	| Exit (_, job_result)
	| Abandoned (_, _, job_result) ->
		let shared = {
			MultiTypes.path_condition = job_result.Job.result_state.Types.path_condition;
			shared_block_to_bytes = update_to_shared_memory 
				multijob.shared.shared_block_to_bytes 
				job_result.Job.result_state.Types.block_to_bytes;
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
			Types.global = process.MultiTypes.global;
			Types.formals = process.MultiTypes.formals;
			Types.locals = process.MultiTypes.locals;
			Types.callstack = process.MultiTypes.callstack;
			Types.callContexts = process.MultiTypes.callContexts;
			Types.stmtPtrs = process.MultiTypes.stmtPtrs;
			Types.va_arg = process.MultiTypes.va_arg;
			Types.va_arg_map = process.MultiTypes.va_arg_map;
			Types.block_to_bytes = update_from_shared_memory multijob.shared.shared_block_to_bytes process.MultiTypes.block_to_bytes;
			Types.path_condition = multijob.MultiTypes.shared.MultiTypes.path_condition;
			(* TODO *)
			Types.aliases = VarinfoMap.empty;
			Types.mallocs = MallocMap.empty;
			Types.path_condition_tracked = [];
			Types.bytes_eval_cache = BytesMap.empty;
		} in
		let job = {
			Job.file = multijob.MultiTypes.file;
			Job.state = state;
			Job.instrList = program_counter.MultiTypes.instrList;
			Job.stmt = program_counter.MultiTypes.stmt;
			Job.jid = multijob.MultiTypes.jid;
			(* TODO *)
			Job.trackedFns = StringSet.empty;
			Job.inTrackedFn = false;
			Job.exHist = emptyHistory;
			Job.decisionPath = [];
			Job.boundingPaths = None;
		} in
		let multijob = { multijob with
			processes = processes;
			current_pid = process.MultiTypes.pid;
		} in
		Some (job, multijob)
