open DataStructures
open OcamlUtilities
open OtterBytes
open OtterCore
open State



type 'job scheduling_data =
	| Running (* Nomal round robin *)
	| TimeWait of int (* Letting other processes go for a while; uses a counter that is decremented each time a job is stepped *)
	| IOBlock of ('job, Bytes.bytes) Deferred.t MemoryBlockMap.t (* Blocking until a shared value changes *)
	| Atomic (* Exclusive control, used when several opeations must be done without preemption *)

(* Environment state as seen by a process.
 * This includes items that are included in state.
 * path_condition and block_to_bytes are invalid when this is not the active process.
 *)
type 'job local_state = 'job State.state

(* State about the execution of the process.
 * This includes items that are in job, but are process specific.
 *)
type program_counter = {
	instrList : Cil.instr list;
	stmt : Cil.stmt;
	inTrackedFn : bool;
}

(* Data about how the process relates to other processes.
 * This includes items that are not in state or job, but are process specific.
 *)
type 'job process_metadata = {
	pid : int;
	parent_pid : int;
	priority : 'job scheduling_data;
}

(* Additional state that applies to all processes.
 * This includes items in state that are shared between processes.
 *)
type 'job shared_state = {
	shared_block_to_bytes : ('job, Bytes.bytes) Deferred.t MemoryBlockMap.t;
}

(* TODO: turn multijob into a subclass of Job.t *)
type 'job multijob = {
	processes : (program_counter * 'job local_state * 'job process_metadata) list;
	shared : 'job shared_state;
	next_pid : int;
	current_metadata : 'job process_metadata; (* this is preserved here when a job is running, since it can't be put in job or state *)

	(* the job to run, which will be modified as necessary to represent different processes;
	   (also, jobs can't be simply constructed, otherwise the types won't match) *)
	active_job : 'job;
} constraint 'job = #Job.t
