open DataStructures
open OtterBytes
open Bytes

module VarinfoMap = Map.Make (struct
	type t = Cil.varinfo
	let compare a b = Pervasives.compare a.Cil.vid b.Cil.vid
end)

module TypeMap = Map.Make (struct
	type t = Cil.typ
	let compare x y =
		let canonicalize t = Cil.typeSigWithAttrs (fun _ -> []) t in
		Pervasives.compare (canonicalize x) (canonicalize y)
end)

module StringSet = Set.Make(String)

module MallocMap = Map.Make (struct
    type t = Cil.varinfo * string
    let compare (va, sa) (vb, sb) = match Pervasives.compare va.Cil.vid vb.Cil.vid with
        | 0 -> String.compare sa sb
        | i -> i
end)

module IndexMap = Map.Make (struct
	type t = int
	let compare (a : int) (b : int) = Pervasives.compare a b
end)

module MemoryBlockMap = Map.Make (struct
	type t = memory_block
	let compare (a:t) b = Pervasives.compare a.memory_block_id b.memory_block_id
end)

module VargsMap = Map.Make (struct
	type t = bytes
	let compare : t -> t -> int = Pervasives.compare
end)

module BytesMap = Map.Make (struct
	type t = bytes
	let compare = Pervasives.compare
end)


exception SignalException of string


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
		global : memory_frame;                  (* Map global lvals to blocks *)
		formals : memory_frame list;            (* Map formal lvals to blocks *)
		locals : memory_frame list;             (* Map local lvals to blocks *)
		aliases : memory_block list VarinfoMap.t; (* Map from varinfos to aliased blocks, e.g., from unknown call stack recursion *)
		mallocs : memory_block list TypeMap.t MallocMap.t; (* Map from malloc sites to aliased blocks from unknown allocation *)
		callstack : Cil.fundec list;            (* Function call stack *)
		block_to_bytes : bytes deferred MemoryBlockMap.t;
		path_condition : bytes list;
		path_condition_tracked : bool list;
		callContexts : callingContext list;
		(** The last element of callstack is the function at which the
				executor started execution. The last element of callContexts
				is [Runtime]. Other than that, the nth element of callstack is
				the fundec called by the [Instr] at the nth position in
				callContexts. *)
		stmtPtrs : callingContext IndexMap.t;     (* Pointers into code.  Used for longjump. *)
		
		va_arg : bytes list list;			(* A stack of va_arg *)
		va_arg_map : bytes list VargsMap.t;
		bytes_eval_cache : bool BytesMap.t; (* Map bytes to boolean value, if exists *)
	}



(* http://www.c-faq.com/decl/strlitinit.html *)

let (string_table : bytes MemoryBlockMap.t ref) = ref MemoryBlockMap.empty

(*let (vargs_table : bytes list VargsMap.t ref) = ref VargsMap.empty*)

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

let job_counter = Counter.make ()

type job = {
	file : Cil.file;
	state : state;
	exHist : executionHistory;
    decisionPath :  (stmtInfo * bool) list; (** The decision path is a list of (conditional statement, boolean) pair. Most recent statement first. *)
	instrList : Cil.instr list; (** [instr]s to execute before moving to the next [stmt] *)
	stmt : Cil.stmt;            (** The next statement the job should execute *)
	trackedFns : StringSet.t;	(** The set of functions (names) in which to track coverage *)
	inTrackedFn : bool;         (** Is stmt in a function in the original program (as opposed to in a library or system call)? *)
	jid : int; (** A unique identifier for the job *)
}

type job_result = {
	result_file : Cil.file;
	result_state : state;
	result_history : executionHistory;
    result_decision_path : (stmtInfo * bool) list;
}

type 'reason job_completion =
	| Return of bytes option * job_result
	| Exit of bytes option * job_result
	| Abandoned of 'reason * Cil.location * job_result
	| Truncated of job_result * job_result

type 'reason job_state =
	| Active of job
	| Fork of 'reason job_state list
	| Complete of 'reason job_completion
	| Paused of job
