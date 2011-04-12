open DataStructures
open OcamlUtilities
open OtterBytes
open OtterCore
open State



type 'job scheduling_data =
	| Running (* Nomal round robin *)
	| TimeWait of int (* Letting other processes go for a while; uses a counter that is decremented each time a job is stepped *)
	| IOBlock of ('job, Bytes.bytes) Deferred.t MemoryBlockMap.t (* Blocking until a shared value changes *)
	| Atomic of int (* Exclusive control, used when several opeations must be done without preemption. The int is the depth of nested atomic sections. *)


class t file cmdline =
    object (self : 'self)
        constraint 'self = #Job.t
        inherit OtterJob.FileJob.t file cmdline (* have to use inheritance to satisfy the type constraint *)

        val pid = 0
        method pid = pid
        method with_pid pid = {< pid = pid >}

        val parent_pid = -2
        method parent_pid = parent_pid
        method with_parent_pid parent_pid = {< parent_pid = parent_pid >}

        val priority : 'self scheduling_data = Running
        method priority = priority
        method with_priority priority = {< priority = priority >}

        val shared_block_to_bytes : ('self, Bytes.bytes) Deferred.t MemoryBlockMap.t = MemoryBlockMap.empty
        method shared_block_to_bytes = shared_block_to_bytes
        method with_shared_block_to_bytes shared_block_to_bytes = {< shared_block_to_bytes = shared_block_to_bytes >}

        val next_pid = 1
        method next_pid = next_pid
        method with_next_pid next_pid = {< next_pid = next_pid >}

        (* TODO: perhaps other_processes should only carry the local state *)
        val other_processes : 'self list = []
        method other_processes = other_processes
        method with_other_processes other_processes = {< other_processes = other_processes >}
    end

