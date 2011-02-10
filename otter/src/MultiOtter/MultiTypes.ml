open DataStructures
open OcamlUtilities
open OtterBytes
open OtterCore
open State



type scheduling_data =
	| Running (* Nomal round robin *)
	| TimeWait of int (* Letting other processes go for a while; uses a counter that is decremented each time a job is stepped *)
	| IOBlock of (State.t, Bytes.bytes) Deferred.t MemoryBlockMap.t (* Blocking until a shared value changes *)
	| Atomic (* Exclusive control, used when several opeations must be done without preemption *)

(* Environment state as seen by a process.
 * This includes items that are included in state.
 * path_condition and block_to_bytes are invalid when this is not the active process.
 *)
type local_state = State.t

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
type process_metadata = {
	pid : int;
	parent_pid : int;
	priority : scheduling_data;
}

(* Additional state that applies to all processes.
 * This includes items in state that are shared between processes.
 *)
type shared_state = {
	shared_path_condition : Bytes.bytes list;
	shared_block_to_bytes : (State.t, Bytes.bytes) Deferred.t MemoryBlockMap.t;
	trackedFns : Job.StringSet.t;
	exHist : Job.executionHistory;
}

(* TODO: turn multijob into a subclass of Job.t *)
type 'job multijob = {
	file : Cil.file;
	processes : (program_counter * local_state * process_metadata) list;
	shared : shared_state;
	jid : int;
	next_pid : int;
	current_metadata : process_metadata; (* this is preserved here when a job is running, since it can't be put in job or state *)
	initial_job : 'job; (* HACK: need to keep a job to modify since job is now polymorphic, and can't be simply constructed (the types won't match otherwise) *)
} constraint 'job = _ #Job.t
