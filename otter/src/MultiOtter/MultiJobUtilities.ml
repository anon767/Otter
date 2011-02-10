open OtterCore
open OtterBytes
open MultiTypes
open Bytes
open State
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
		MultiTypes.instrList = job#instrList;
		stmt = job#stmt;
		inTrackedFn = job#inTrackedFn;
	} in
	let local_state = { job#state with path_condition = []; } in
	let shared = {
		shared_path_condition = job#state.path_condition;
		shared_block_to_bytes = update_to_shared_memory multijob.shared.shared_block_to_bytes job#state.block_to_bytes;
		MultiTypes.trackedFns = job#trackedFns;
		exHist = job#exHist;
	} in
	{ multijob with
		MultiTypes.file = job#file;
		processes =
			(match metadata.priority with
				| Atomic -> (program_counter, local_state, metadata)::multijob.processes (* save time sorting by putting an atomic process on the front *)
				| _ -> List.append multijob.processes [ (program_counter, local_state, metadata) ])
			;
		shared = shared;
		jid = job#jid;
		next_pid = multijob.next_pid;
		current_metadata = multijob.current_metadata;
	}


(* update the multijob with a completed job *)
let put_completion completion multijob = match completion with
	| Return (_, job_result)
	| Exit (_, job_result)
	| Abandoned (_, job_result)
	| Truncated (_, job_result) ->
		(* update process parents of children of the compleated process to point to the compleated process's parent *)
		let processes = List.map
			(fun (pc, ls, md) ->
				if md.parent_pid = multijob.current_metadata.pid then
					(pc, ls, { md with parent_pid = multijob.current_metadata.parent_pid; })
				else
					(pc, ls, md)
			)
			multijob.processes
		in
		let shared = {
			shared_path_condition = job_result#state.path_condition;
			shared_block_to_bytes = update_to_shared_memory
				multijob.shared.shared_block_to_bytes
				job_result#state.block_to_bytes;
			MultiTypes.trackedFns = multijob.shared.MultiTypes.trackedFns;
			exHist = job_result#exHist;
		} in
		{ multijob with
			processes = processes;
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
				| TimeWait x, TimeWait y -> compare x y
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
		(* TODO: make multijob a subtype of Job.job that contains a set of processes, one of which is designated as active, so that
			it can be used transparently as job without having to go through the trouble of creating a new job like below *)
		let job = multijob.initial_job in
		let job = job#with_file multijob.file in
		let job = job#with_state { local_state with
			State.block_to_bytes = update_from_shared_memory multijob.shared.shared_block_to_bytes local_state.block_to_bytes;
			State.path_condition = multijob.shared.shared_path_condition;
		} in
		let job = job#with_instrList (program_counter.MultiTypes.instrList) in
		let job = job#with_stmt (program_counter.MultiTypes.stmt) in
		let job = job#with_jid (multijob.MultiTypes.jid) in
		let job = job#with_trackedFns multijob.shared.MultiTypes.trackedFns in
		let job = job#with_inTrackedFn program_counter.MultiTypes.inTrackedFn in
		let job = job#with_exHist multijob.shared.MultiTypes.exHist in
		let multijob = { multijob with
			processes = processes;
			current_metadata = metadata;
		} in
		Some (job, multijob)

