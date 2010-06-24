open Bytes

module VarinfoMap = Cilutility.VarinfoMap
module TypeMap = Cilutility.TypeMap

type operator_action = (bytes*Cil.typ) list -> (bytes (* *Cil.typ*))


module MemoryBlockMap =
	Utility.MakeMap (
	struct
		type t = memory_block
		let compare (a:t) b = Pervasives.compare a.memory_block_id b.memory_block_id
	end
	)

module VargsMap =
	Utility.MakeMap (
	struct
		type t = bytes 
		let compare : t -> t -> int = Pervasives.compare				
	end
	)	

module LocMap =
	Utility.MakeMap (
	struct
		type t = Cil.location*int 
		let compare ((a,ai):t) (b,bi) =
			if ai<>bi then Pervasives.compare ai bi else
				Cil.compareLoc a b
	end
	)	

module BytesMap =
	Utility.MakeMap (
	struct
		type t = bytes 
		let compare = Pervasives.compare				
	end
	)	

	
(** A calling context may either be the symbolic executor, represented by
		[Runtime], or from another function in the source code, represented
		either by a tuple [Source (destOpt,callStmt,callInstr,nextStmt)] if the
		function returns or [NoReturn (callInstr)] if the function doesn't return.
		[nextStmt] is the [stmt] to execute after the call returns; [callStmt]
		and [callInstr] are the function call statement and instruction;
		and [destOpt] is [None] if we ignore the result of the call, or it is
		[Some Cil.lval], which means we should assign the result there. *)
type callingContext =
    | Runtime
    | Source of (Cil.lval option * Cil.stmt * Cil.instr * Cil.stmt)
	| NoReturn of Cil.instr



type 'a deferred =
	| Immediate of 'a
	| Deferred of (state -> (state * 'a))
and memory_frame = 
	lval_block deferred VarinfoMap.t
and state =
	{
		global : memory_frame list;                  (* Map global lvals to blocks *)
		formals : memory_frame list list;            (* Map formal lvals to blocks *)
		locals : memory_frame list list;             (* Map local lvals to blocks *)
		extra : memory_block list VarinfoMap.t; (* Map for extra blocks, e.g., from unknown call stack recursion *)
		malloc : memory_block list TypeMap.t VarinfoMap.t; (* Map for malloc blocks from unknown allocation *)
		callstack : Cil.fundec list list;            (* Function call stack *)
		block_to_bytes : bytes deferred MemoryBlockMap.t list;
		path_condition : bytes list;
		path_condition_tracked : bool list;
		callContexts : callingContext list list;

		(** The last element of callstack is the function at which the
				executor started execution. The last element of callContexts
				is [Runtime]. Other than that, the nth element of callstack is
				the fundec called by the [Instr] at the nth position in
				callContexts. *)
		
		va_arg : bytes list list;			(* A stack of va_arg *)
		va_arg_map : bytes list VargsMap.t;
		loc_map : bytes LocMap.t;     (* Map loc to symbolic bytes *)
        	bytes_eval_cache : bool BytesMap.t; (* Map bytes to boolean value, if exists *) 
		proc_index : int; (* Current process context *)
	}


let get_callstack state =
	List.nth state.callstack state.proc_index

let get_callContexts state =
	List.nth state.callContexts state.proc_index

(* http://www.c-faq.com/decl/strlitinit.html *)

let (string_table : bytes MemoryBlockMap.t ref) = ref MemoryBlockMap.empty

(*let (vargs_table : bytes list VargsMap.t ref) = ref VargsMap.empty*)

(* some globals that are helpful *)
let stp_count = ref 0

(* With statement ids which are unique only within functions, we need
	 to know the function's name in addition to the sid to uniquely
	 identify a stmt. *)
type stmtInfo = { siFuncName : string ; siStmt : Cil.stmt }
let compareStmtInfo x y =
	let tmp = Pervasives.compare x.siFuncName y.siFuncName in
	if tmp = 0
	then Pervasives.compare x.siStmt.Cil.sid y.siStmt.Cil.sid
	else tmp

module CondSet = Set.Make
	(struct
		type t = stmtInfo*bool
		(* the if statement and branch direction *)
		let compare ((stmt1,truth1):t) (stmt2,truth2) =
			let stmtCmp = compareStmtInfo stmt1 stmt2 in
			if stmtCmp = 0
			then Pervasives.compare truth1 truth2
			else stmtCmp
	end)

module EdgeSet = Set.Make
	(struct
		type t = stmtInfo*stmtInfo (* These should always be the final stmts in their basic blocks *)
		(* Order edges primarily by source, then by destination *)
		let compare ((src1,dst1):t) (src2,dst2) =
			let srcCmp = compareStmtInfo src1 src2 in
			if srcCmp = 0
			then compareStmtInfo dst1 dst2
			else srcCmp
	end)

module StmtInfoSet = Set.Make
	(struct
		 type t = stmtInfo
		 let compare = compareStmtInfo
	 end)

module LineSet = Set.Make
	(struct
		 type t = string * int (** (filename,line number) pair *)
		 let compare ((f1,l1):t) (f2,l2) =
			 let tmp = String.compare f1 f2 in
			 if tmp = 0
			 then Pervasives.compare l1 l2
			 else tmp
	 end)

type executionHistory = {
	coveredLines : LineSet.t; (** Which lines we've hit *)
	coveredBlocks : StmtInfoSet.t; (** Which basic blocks we've hit. We identify a block by the last stmt within it. *)
	coveredEdges : EdgeSet.t; (** Which edges we've traversed on this execution *)
	coveredConds : CondSet.t; (** Which conditions we've hit *)
	executionPath : stmtInfo list; (** The execution path (list of stmts) so far, most recent stmt first. *)
	bytesToVars : (bytes * Cil.varinfo) list;
		(** List associating symbolic bytes to the variable that was
				assigned this value by a call to __SYMBOLIC(&<variable>). *)
}

let emptyHistory = {
	coveredLines = LineSet.empty;
	coveredBlocks = StmtInfoSet.empty;
	coveredEdges = EdgeSet.empty;
	coveredConds = CondSet.empty;
	executionPath = [];
	bytesToVars = [];
}

module SymbolSet = Set.Make
	(struct
		 type t = symbol
		 let compare (x:t) y = Pervasives.compare x.symbol_id y.symbol_id
	 end)

let signalStringOpt : string option ref = ref None
exception SignalException

type proc_job = {
	instrList : Cil.instr list; (** [instr]s to execute before moving to the next [stmt] *)
	stmt : Cil.stmt;            (** The next statement the job should execute *)
	inTrackedFn : bool;         (** Is stmt in a function in the original program (as opposed to in a library or system call)? *)
	pid : int; (** process id **)
}

type job = {
	state : state;
	exHist : executionHistory;
	proc_info : proc_job list;
	mergePoints : StmtInfoSet.t;     (** A list of potential merge points *)
	jid : int; (** A unique identifier for the job *)
	num_procs : int;
	next_pid : int;
}

let get_proc_info job =
	List.nth job.proc_info job.state.proc_index


let rec set_nth l v n = 
	match n,l with
		| 0,h::t -> v::t
		| _,h::t -> h::(set_nth t v (n-1))
		| _,[] -> failwith "index out of bounds set_nth"


let set_proc_info job info =
	set_nth job.proc_info info job.state.proc_index


let next_proc job =
	{job with
		state = {job.state with
			proc_index = ((job.state.proc_index + 1) mod (job.num_procs));
		};
	}


let rec del_nth l n =
	match n,l with
		| 0,h::t -> t
		| _,h::t -> h::(del_nth t (n-1))
		| _,[] -> failwith "index out of bounds del_nth"


let kill_proc job =
	{job with
		num_procs = job.num_procs - 1;
		proc_info = del_nth job.proc_info job.state.proc_index;
		state = {job.state with
				global = del_nth job.state.global job.state.proc_index;
				formals = del_nth job.state.formals job.state.proc_index;
				locals = del_nth job.state.locals job.state.proc_index;
				callstack = del_nth job.state.callstack job.state.proc_index;
				block_to_bytes = del_nth job.state.block_to_bytes job.state.proc_index;
				callContexts = del_nth job.state.callContexts job.state.proc_index;
				proc_index = job.state.proc_index mod (job.num_procs - 1);
			};
	}


type job_result = {
	result_state : state;
	result_history : executionHistory;
}

type job_completion_reason =
	| Return of bytes option * job_result
	| Exit of bytes option * job_result
	| Abandoned of string * Cil.location * job_result
	| Truncated of job_result * job_result

type job_completion = {
	job : job;
	reason : job_completion_reason;
}

type job_state =
	| Active of job
	| Big_Fork of job_state list
	| Complete of job_completion

module JobSet = Set.Make
	(struct
		 type t = job
		 let compare (job1:t) job2 =
			 (* I want the job with earliest stmt.sid to be first in the ordering *)
			 let c = Pervasives.compare (get_proc_info job1).stmt.Cil.sid (get_proc_info job2).stmt.Cil.sid in
			 if c = 0 then Pervasives.compare job1.jid job2.jid
			 else c
	 end)

(** Map [stmtInfo]s of [If] statements to the [stmtInfo]s of join
		points which the [If]s dominate.
		This will allow us to know where we should expect to merge paths. *)
let ifToJoinPointsHash : (stmtInfo,stmtInfo) Hashtbl.t = Hashtbl.create 500

(* target to be used in prioritizer *)
type target = {
  func: Cil.fundec;
  entry_state: state;
  failing_condition: bytes;
}

