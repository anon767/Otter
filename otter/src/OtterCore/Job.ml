open DataStructures
open OtterBytes

module StringSet = Set.Make(String)

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
	bytesToVars : (Bytes.bytes * Cil.varinfo) list;
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

type job = {
    file : Cil.file;
    state : Types.state;
    exHist : executionHistory;
    decisionPath : Decision.t list; (** The decision path is a list of decision. Most recent decision first. *)
    boundingPaths : Decision.t list list option; (** The execution can only run on these paths, if exist. Paths have most recent decision first. *)
    instrList : Cil.instr list; (** [instr]s to execute before moving to the next [stmt] *)
    stmt : Cil.stmt;            (** The next statement the job should execute *)
    trackedFns : StringSet.t;	(** The set of functions (names) in which to track coverage *)
    inTrackedFn : bool;         (** Is stmt in a function in the original program (as opposed to in a library or system call)? *)
    jid : int; (** A unique identifier for the job *)
}

type job_result = {
	result_file : Cil.file;
	result_state : Types.state;
	result_history : executionHistory;
	result_decision_path : Decision.t list;
}

type ('abandoned, 'truncated) job_completion =
    | Return of Bytes.bytes option * job_result (* Jobs that successfully completed by returning from the entry function *)
    | Exit of Bytes.bytes option * job_result (* Jobs that successfully completed by calling _exit *)
    | Abandoned of 'abandoned * Cil.location * job_result (* Jobs that are terminated due to an error in the source program *)
    | Truncated of 'truncated * job_result (* Jobs that are terminated for other reasons *)
    (* TODO: change Abandoned to be like Truncated *)

type ('abandoned, 'truncated) job_state =
	| Active of job
	| Fork of ('abandoned, 'truncated) job_state list
	| Complete of ('abandoned, 'truncated) job_completion
	| Paused of job

let job_counter = Counter.make ()

(* create a job that begins at a function, given an initial state *)
let make file state fn argvs =
    let state = MemOp.state__start_fcall state Types.Runtime fn argvs in
    let trackedFns = List.fold_left (fun set elt -> StringSet.add elt set) StringSet.empty !Executeargs.arg_fns in
    (* create a new job *)
    {
        file = file;
        state = state;
        exHist = emptyHistory;
        decisionPath = [];
        boundingPaths = None;
        instrList = [];
        stmt = List.hd fn.Cil.sallstmts;
        trackedFns = trackedFns;
        inTrackedFn = StringSet.mem fn.Cil.svar.Cil.vname trackedFns;
        jid = Counter.next job_counter;
    }

(** Get the file location for the current job instruction.
		@param job the job to get the current location from
		@return the file location
*)
let get_loc job = match job.instrList with
    | [] -> Cil.get_stmtLoc job.stmt.Cil.skind
    | instr::_ -> Cil.get_instrLoc instr


(** Get the {!OtterCFG.Instruction.t} for current instruction in a job. *)
let get_instruction job =
    (* TODO: refactor Statement to use OtterCFG.Instruction, to not need this function *)
    match job.stmt.Cil.skind with
        | Cil.Instr instrs when job.instrList = [] ->
            (* if stmt is Cil.Instr, instrList may be temporarily empty: see handling of Cil.Instr in Statement.exec_stmt *)
            OtterCFG.Instruction.make job.file (List.hd job.state.Types.callstack) job.stmt instrs
        | _ ->
            OtterCFG.Instruction.make job.file (List.hd job.state.Types.callstack) job.stmt job.instrList


(** Get a list of {!OtterCFG.Instruction.t} representing the current calling context in a job. *)
let get_instruction_context job =
    let rec get_instruction_context context return stack = match return, stack with
        | Types.Source (_, _, _, stmt)::return, fundec::stack ->
            let context = (OtterCFG.Instruction.of_stmt_first job.file fundec stmt)::context in
            get_instruction_context context return stack
        | Types.Source (_, _, _, _)::_, [] ->
            invalid_arg "get_instruction_context: job with malformed callstack"
        | _, _ ->
            List.rev context
    in
    get_instruction_context [] job.state.Types.callContexts (List.tl job.state.Types.callstack)

