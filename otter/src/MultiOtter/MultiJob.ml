open DataStructures
open OcamlUtilities
open OtterBytes
open OtterCore
open State


module SharedBlocks = Set.Make (struct
    type t = Bytes.memory_block
    let compare x y = Pervasives.compare x.Bytes.memory_block_id y.Bytes.memory_block_id
end)


type 'job scheduling_data =
    | Running (* Nomal round robin *)
    | TimeWait of int (* Letting other processes go for a while; uses a counter that is decremented each time a job is stepped *)
    | IOBlock of ('job, Bytes.bytes) Deferred.t MemoryBlockMap.t (* Blocking until a shared value changes *)
    | Atomic of int (* Exclusive control, used when several opeations must be done without preemption. The int is the depth of nested atomic sections. *)
    | Complete


(* could also simply upcast job, but this will allow unused fields to be GC'ed *)
class ['job] process_state other =
    object
        val state : 'job State.state = other#state
        method state = state
        method with_state state = {< state = state >}

        val instrList : Cil.instr list = other#instrList
        method instrList = instrList

        val stmt : Cil.stmt = other#stmt
        method stmt = stmt

        val inTrackedFn : bool = other#inTrackedFn
        method inTrackedFn = inTrackedFn

        val pid : int = other#pid
        method pid = pid

        val parent_pid : int = other#parent_pid
        method parent_pid = parent_pid
        method with_parent_pid parent_pid = {< parent_pid = parent_pid >}

        val priority : 'job scheduling_data = other#priority
        method priority = priority
        method with_priority priority = {< priority = priority >}
    end


class t file cmdline =
    object (self : 'self)
        constraint 'self = #Job.t
        inherit OtterJob.FileJob.t file cmdline (* easier to satisfy the type constraint using inheritance *)

        val pid = 0
        method pid = pid
        method with_pid pid = {< pid = pid >}

        val parent_pid = -2
        method parent_pid = parent_pid
        method with_parent_pid parent_pid = {< parent_pid = parent_pid >}

        val priority : 'self scheduling_data = Running
        method priority = priority
        method with_priority priority = {< priority = priority >}

        val shared_blocks : SharedBlocks.t = SharedBlocks.empty
        method shared_blocks = shared_blocks
        method with_shared_blocks shared_blocks = {< shared_blocks = shared_blocks >}

        val next_pid = 1
        method next_pid = next_pid
        method with_next_pid next_pid = {< next_pid = next_pid >}

        val other_processes : 'self process_state list = []
        method other_processes = other_processes
        method with_other_processes other_processes = {< other_processes = other_processes >}
    end

