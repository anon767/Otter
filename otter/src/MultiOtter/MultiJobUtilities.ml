open OcamlUtilities
open OtterCore
open OtterBytes
open MultiJob
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


(* update the multijob with a completed job *)
let process_completed = function
    | Return (_, job)
    | Exit (_, job)
    | Abandoned (_, job)
    | Truncated (_, job) ->
        if job#other_processes = [] then
            None
        else
            (* update process parents of children of the compleated process to point to the completed process's parent *)
            let job = job#with_other_processes begin List.map
                (fun other -> if other#parent_pid = job#pid then other#with_parent_pid job#parent_pid else other)
                job#other_processes
            end in
            let job = job#with_priority MultiJob.Complete in
            Some job


(* schedule a process in a job *)
let schedule_job job =
    let update_process process =
        match process#priority with
            | Atomic _
            | Running
            | MultiJob.Complete -> process
            | TimeWait n ->
                if n <= 0 then
                    process#with_priority Running
                else
                    process#with_priority (TimeWait (n-1))
            | IOBlock io_block_to_bytes -> (* look for changed blocks *)
                let fold_func key value prev =
                    prev || (* stop checking if one changed *)
                    try
                        let new_value = MemoryBlockMap.find key job#state.block_to_bytes in
                        match new_value, value with
                            | DataStructures.Deferred.Immediate x, DataStructures.Deferred.Immediate y -> not (Bytes.bytes__equal x y)
                            | _, _ -> new_value != value
                    with
                        | Not_found -> true (* block was gfreed and so counts as changed *)
                in
                if (MemoryBlockMap.fold fold_func io_block_to_bytes false) then
                    process#with_priority Running (* something changed, wake up process *)
                else
                    process (* nothing changed, keep thread asleep *)
    in

    let job = update_process job in
    let other_processes = List.map update_process job#other_processes in
    let job = job#with_other_processes other_processes in

    let job_process = new process_state job in

    (* since the current process is prepended to the list below, pick the later process if tied for round-robin *)
    let max_process = List.fold_left begin fun max_process process ->
        match max_process#priority, process#priority with
            | Atomic _, Atomic _ -> failwith "There are multiple Atomic processes in the process list"
            | _, Atomic _ -> process
            | Atomic _, _ -> max_process
            | _, Running _ -> process
            | Running _, _ -> max_process
            | TimeWait x, TimeWait y -> if x < y then max_process else process
            | _, TimeWait _ -> process
            | TimeWait _, _ -> max_process
            | _, MultiJob.Complete -> assert false (* should never enter other_processes by below *)
            | _, _ -> process
    end job_process other_processes in

    if max_process != job_process then
        let other_processes = List.filter ((!=) max_process) other_processes in
        let job = match job_process#priority with
            | MultiJob.Complete -> job#with_other_processes other_processes
            | _ -> job#with_other_processes (job_process::other_processes)
        in

        let job = job#with_shared_block_to_bytes (update_to_shared_memory job#shared_block_to_bytes job#state.block_to_bytes) in

        let job = job#with_state { max_process#state with
            (* take the previous process state and update the shared parts *)
            block_to_bytes = update_from_shared_memory job#shared_block_to_bytes max_process#state.block_to_bytes;
            path_condition = job#state.path_condition;
        } in
        let job = job#with_instrList max_process#instrList in
        let job = job#with_stmt max_process#stmt in
        let job = job#with_inTrackedFn max_process#inTrackedFn in
        let job = job#with_pid max_process#pid in
        let job = job#with_parent_pid max_process#parent_pid in
        let job = job#with_priority max_process#priority in
        job
    else
        job

