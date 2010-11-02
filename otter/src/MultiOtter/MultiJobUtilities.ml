open OtterCore
open OtterBytes
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
let put_job job multijob metadata =
	let program_counter = {
		MultiTypes.instrList = job.Job.instrList;
		stmt = job.Job.stmt;
	} in
	let local_state = { job.state with path_condition = []; } in
	let shared = {
		shared_path_condition = job.state.path_condition;
		shared_block_to_bytes = update_to_shared_memory multijob.shared.shared_block_to_bytes job.state.block_to_bytes;
	} in
	{
		MultiTypes.file = job.Job.file;
		processes = 
			(match metadata.priority with
				| Atomic -> (program_counter, local_state, metadata)::multijob.processes (* save time sorting by putting an atomic process on the front *)
				| _ -> List.append multijob.processes [ (program_counter, local_state, metadata) ])
			;
		shared = shared;
		jid = job.Job.jid;
		next_pid = multijob.next_pid;
		current_metadata = multijob.current_metadata;
	}


(* update the multijob with a completed job *)
let put_completion completion multijob = match completion with
	| Return (_, job_result)
	| Exit (_, job_result)
	| Abandoned (_, _, job_result)
	| Truncated (_, job_result) ->
		let shared = {
			shared_path_condition = job_result.result_state.path_condition;
			shared_block_to_bytes = update_to_shared_memory 
				multijob.shared.shared_block_to_bytes 
				job_result.result_state.block_to_bytes;
		} in
		{ multijob with
			shared = shared;
		}

let schedule_process_list multijob =
	let rec update_process_list processes =
		match processes with
		| [] -> []
		| (pc, ls, md)::t ->
			match md.priority with
				| Atomic
				| Running -> (pc, ls, md)::(update_process_list t)
				| TimeWait n ->
					if n <= 0 then
						(pc, ls, { md with priority = Running; })::(update_process_list t)
					else
						(pc, ls, { md with priority = TimeWait (n-1); })::(update_process_list t)
				| IOBlock io_block_to_bytes -> (* look for changed blocks *)
					let fold_func key value prev =
						prev || (* stop checking if one changed *)
						try
							let new_value = MemoryBlockMap.find key multijob.shared.shared_block_to_bytes in
							match new_value, value with
								| DataStructures.Deferred.Immediate x, DataStructures.Deferred.Immediate y -> not (Bytes.bytes__equal x y)
								| _, _ -> new_value != value
						with
							| Not_found -> true (* block was gfreed and so counts as changed *)
					in
					if(MemoryBlockMap.fold fold_func io_block_to_bytes false) then
						(pc, ls, { md with priority = Running; })::(update_process_list t) (* something changed, wake up process *)
					else
						(pc, ls, md)::(update_process_list t) (* nothing changed, keep thread asleep *)
	in
	let processes = update_process_list multijob.processes in
	List.stable_sort (* TODO: use a priority queue instead *)
		begin fun (_, _, ls1) (_, _, ls2) -> 
			match ls1.priority, ls2.priority with
				| Atomic, Atomic -> 0
				| Atomic, _ -> -1
				| _, Atomic -> 1
				| Running, Running -> 0
				| Running, _ -> -1
				| _, Running -> 1
				| TimeWait _, TimeWait _ -> 0
				| TimeWait _, _ -> -1
				| _, TimeWait _ -> 1
				| _, _ -> 0
		end
		processes

(* get a job from a multijob *)
let get_job multijob = 
	match schedule_process_list multijob with (* TODO: use a priority queue instead *)
	| [] ->
		None
	| (program_counter, local_state, metadata)::processes ->
		(* extract the first job from a multijob *)
		let state = { local_state with
			Types.block_to_bytes = update_from_shared_memory multijob.shared.shared_block_to_bytes local_state.block_to_bytes;
			Types.path_condition = multijob.shared.shared_path_condition;
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
			current_metadata = metadata;
		} in
		Some (job, multijob)

