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

module CondData = struct
	type t = stmtInfo*bool
	(* the if statement and branch direction *)
	let compare ((stmt1,truth1):t) (stmt2,truth2) =
		let stmtCmp = compareStmtInfo stmt1 stmt2 in
		if stmtCmp = 0
		then Pervasives.compare truth1 truth2
		else stmtCmp
end

module EdgeData = struct
	type t = stmtInfo*stmtInfo (* These should always be the final stmts in their basic blocks *)
	(* Order edges primarily by source, then by destination *)
	let compare ((src1,dst1):t) (src2,dst2) =
		let srcCmp = compareStmtInfo src1 src2 in
		if srcCmp = 0
		then compareStmtInfo dst1 dst2
		else srcCmp
end

module StmtInfoData = struct
    type t = stmtInfo
    let compare = compareStmtInfo
end

module LineData = struct
    type t = string * int (** (filename,line number) pair *)
    let compare ((f1,l1):t) (f2,l2) =
   	 let tmp = String.compare f1 f2 in
   	 if tmp = 0
   	 then Pervasives.compare l1 l2
   	 else tmp
end

module CondSet     = TimedSet.Make (CondData)
module EdgeSet     = TimedSet.Make (EdgeData)
module StmtInfoSet = TimedSet.Make (StmtInfoData)
module LineSet     = TimedSet.Make (LineData)

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


let job_counter = Counter.make ()
let job_counter_unique = Counter.make ()


class ['self] t file' fn =
    let trackedFns' = TrackingFunctions.trackedFns file' in
    object (_ : 'self)
        (* TODO: perhaps use a camlp4 syntax extension to deal with this boilerplate? *)
        (* TODO: group methods into logical components that can be composed mix-in style *)
        (* TODO: currently just a glorified extensible record; but perhaps some core functionality like maintaining job ids? *)

        val mutable file : Cil.file = file'
        method file = file
        method with_file file = {< file = file >}

        inherit ['self] State.t as state_super

        val mutable exHist : executionHistory = emptyHistory
        method exHist = exHist
        method with_exHist exHist = {< exHist = exHist >}

        (** The decision path is a list of decision. Most recent decision first. *)  (* TODO: take this out *)
        val mutable decision_path : Decision.t list = []
        method decision_path = decision_path
        method with_decision_path decision_path = {< decision_path = decision_path >}

        (** [instr]s to execute before moving to the next [stmt] *)
        val mutable instrList : Cil.instr list = []
        method instrList = instrList
        method with_instrList instrList = {< instrList = instrList >}

        (** The next statement the job should execute *)
        val mutable stmt : Cil.stmt = List.hd fn.Cil.sallstmts
        method stmt = stmt
        method with_stmt stmt = {< stmt = stmt >}

        (** The set of functions (names) in which to track coverage *)
        val mutable trackedFns : StringSet.t = trackedFns'
        method trackedFns = trackedFns
        method with_trackedFns trackedFns = {< trackedFns = trackedFns >}

        (** Is stmt in a function in the original program (as opposed to in a library or system call)? *)
        val mutable inTrackedFn : bool = StringSet.mem fn.Cil.svar.Cil.vname trackedFns'
        method inTrackedFn = inTrackedFn
        method with_inTrackedFn inTrackedFn = {< inTrackedFn = inTrackedFn >}

        (** An identifier for the job. Not unique. *)
        val mutable jid : int = Counter.next job_counter
        method jid = jid
        method with_jid jid = {< jid = jid >}

        (** A unique identifier for the job *)
        val mutable jid_unique : int = Counter.next job_counter_unique
        method jid_unique = jid_unique
        method with_jid_unique jid_unique = {< jid_unique = jid_unique >}

        (** The unique identifier for the parent of the job *)
        val mutable jid_parent : int= -1 (* Indicates no parent *)
        method jid_parent = jid_parent
        method with_jid_parent jid_parent = {< jid_parent = jid_parent >}

        (** [x#become y] destructively copies all instance variables from [y] to [x]. This should be used sparingly,
            typically only in object initializers.

            {b Subclasses are responsible for providing [#become] that calls [#become] on all superclasses, in addition
            to copying their own instance variables.}

            Unfortunately, Ocaml's object initializers returns [unit], not ['self], and so can't perform initialization
            via the functional update syntax. As a workaround, we'll declare all instance variables mutable, and
            provide [#become] which simply copies all instance variables from another object of the same type. Thus,
            initializers can be used to perform further initialization using the pattern:
{[
    initializer
        let x = self in;
        let x = some_operation x in
        ...
        self#become x
]}
        *)
        method become (other : 'self) =
            state_super#become other;
            file <- other#file;
            exHist <- other#exHist;
            decision_path <- other#decision_path;
            instrList <- other#instrList;
            stmt <- other#stmt;
            trackedFns <- other#trackedFns;
            inTrackedFn <- other#inTrackedFn;
            jid <- other#jid;
            jid_unique <- other#jid_unique;
            jid_parent <- other#jid_parent
    end


type ('job, 'abandoned, 'truncated) job_completion =
    | Return of Bytes.bytes option * 'job (* Jobs that successfully completed by returning from the entry function *)
    | Exit of Bytes.bytes option * 'job (* Jobs that successfully completed by calling _exit *)
    | Abandoned of 'abandoned * 'job (* Jobs that are terminated due to an error in the source program *)
    | Truncated of 'truncated * 'job (* Jobs that are terminated for other reasons *)
    constraint 'job = _ #t

type ('job, 'abandoned, 'truncated) job_state =
    | Active of 'job
    | Fork of ('job, 'abandoned, 'truncated) job_state list
    | Complete of ('job, 'abandoned, 'truncated) job_completion
    | Paused of 'job
    constraint 'job = _ #t


(** Get the file location for the current job instruction.
		@param job the job to get the current location from
		@return the file location
*)
let get_loc job = match job#instrList with
    | [] -> Cil.get_stmtLoc job#stmt.Cil.skind
    | instr::_ -> Cil.get_instrLoc instr


(** Get the {!OtterCFG.Instruction.t} for current instruction in a job. *)
let get_instruction job =
    (* TODO: refactor Statement to use OtterCFG.Instruction, to not need this function *)
    match job#stmt.Cil.skind with
        | Cil.Instr instrs when job#instrList = [] ->
            (* if stmt is Cil.Instr, instrList may be temporarily empty: see handling of Cil.Instr in Statement.exec_stmt *)
            OtterCFG.Instruction.make job#file (List.hd job#state.State.callstack) job#stmt instrs
        | _ ->
            OtterCFG.Instruction.make job#file (List.hd job#state.State.callstack) job#stmt job#instrList


(** Get a list of {!OtterCFG.Instruction.t} representing the current calling context in a job. *)
let get_instruction_context job =
    let rec get_instruction_context context return stack = match return, stack with
        | State.Source (_, _, _, stmt)::return, fundec::stack ->
            let context = (OtterCFG.Instruction.of_stmt_first job#file fundec stmt)::context in
            get_instruction_context context return stack
        | State.Source (_, _, _, _)::_, [] ->
            invalid_arg "get_instruction_context: job with malformed callstack"
        | _, _ ->
            List.rev context
    in
    get_instruction_context [] job#state.State.callContexts (List.tl job#state.State.callstack)

