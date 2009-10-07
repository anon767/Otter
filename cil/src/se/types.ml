
type operator = 
	(* binop *)
	| OP_PLUS
	| OP_SUB
	| OP_MULT
	| OP_DIV
	| OP_MOD
	| OP_LSL
	| OP_LSR
	| OP_LT
	| OP_GT
	| OP_LE
	| OP_GE
	| OP_EQ
	| OP_NE
	| OP_BAND
	| OP_BXOR
	| OP_BOR
	| OP_LAND
	| OP_LOR
	| OP_SX
	(* unop *)
	| OP_UMINUS
	| OP_BNOT
	| OP_LNOT
;;


type symbol =	
	{ 
		symbol_id: int; 	
		(*symbol_writable: bool;*)
	}

and

indicator = Indicator of int
          | Indicator_Not of indicator 
          | Indicator_And of indicator * indicator

and

byte = (* corresponds to BV *)
| Byte_Concrete of char
| Byte_Symbolic of symbol
| Byte_Bytes of bytes * int (* condense a bytes into a byte, that can be put into an array *)  

and

bytes =
| Bytes_Constant of Cil.constant (* length=Cil.sizeOf (Cil.typeOf (Const(constant))) *)
| Bytes_ByteArray of byte ImmutableArray.t  (* content *) 
| Bytes_Address of memory_block option * bytes (* offset *)
| Bytes_MayBytes of indicator * bytes * bytes (* conditional value of the form: if indicator then bytes1 else bytes2 *)
| Bytes_Op of operator * (bytes * Cil.typ) list
| Bytes_Read of bytes * bytes * int						(* less preferrable type *)
| Bytes_Write of bytes * bytes * int * bytes	(* least preferrable type*)
| Bytes_FunPtr of Cil.fundec * bytes (* bytes is the "imaginary address" of the funptr *)
(*| Bytes_DS of data_structure*)
(* | Bytes_Concat  (* allow this to make things more efficient *) *)

and

lval_block = Lval_Block of memory_block * bytes
           | Lval_May of indicator * lval_block * lval_block

and

data_structure = 
    | DS_Set of  bytes list * bytes    (* items and rest *)

and

memory_block_type = 
	| Block_type_StringLiteral
	| Block_type_Global
	| Block_type_Local
	| Block_type_Heap

and

memory_block =
	{
		memory_block_name : string;
		memory_block_id : int;
		memory_block_size : int;
		memory_block_addr : bytes;
		memory_block_type : memory_block_type; 
	}
;;

let hash_consing_bytes_enabled = ref false;;
let hash_consing_bytes_hits = ref 0;;
let hash_consing_bytes_misses = ref 0;;
let hash_consing_bytes_init_size = 1000000;;
let hash_consing_bytes_tbl : (bytes,bytes) Hashtbl.t = Hashtbl.create hash_consing_bytes_init_size;;
let hash_consing_bytes_create bs = 
  if not (!hash_consing_bytes_enabled) then bs else
  try let rv = Hashtbl.find hash_consing_bytes_tbl bs in Utility.increment hash_consing_bytes_hits; rv
  with Not_found -> Hashtbl.add hash_consing_bytes_tbl bs bs; Utility.increment hash_consing_bytes_misses; bs;;


(*
 *  Since bytes objects are immutable, and bytes is private type, all *bs* are
 *  created by calling make_Bytes_* and do not require hash consing check.
 *)
let rec 
make_Byte_Concrete (c) =
	Byte_Concrete (c)
and
make_Byte_Symbolic (s) =
	Byte_Symbolic (s)
and
make_Byte_Bytes ( bs, n ) =
	Byte_Bytes ( bs, n )
and
make_Bytes_Constant ( const ) =
	hash_consing_bytes_create (Bytes_Constant ( const ))
and
make_Bytes_ByteArray ( bytearray ) =
	hash_consing_bytes_create (Bytes_ByteArray ( bytearray ))
and
make_Bytes_Address ( blockopt , bs ) =
	hash_consing_bytes_create (Bytes_Address ( blockopt , bs ))
and
make_Bytes_MayBytes ( indr , bs1 , bs2 ) =
	hash_consing_bytes_create (Bytes_MayBytes ( indr , bs1 , bs2 ))
and
make_Bytes_Op ( op , lst) =
	hash_consing_bytes_create (Bytes_Op ( op , lst))
and
make_Bytes_Read ( src , off , len ) =
	hash_consing_bytes_create (Bytes_Read ( src , off , len ))
and
make_Bytes_Write ( des , off , n , src ) =
	hash_consing_bytes_create (Bytes_Write ( des , off , n , src ))
and
make_Bytes_FunPtr ( f , bs ) =
	hash_consing_bytes_create (Bytes_FunPtr ( f , bs ))
;;


(* A single global byte representing uninitialized memory *)
let byte__undef = Byte_Symbolic({symbol_id = 0}) ;;

module VarinfoMap = Cilutility.VarinfoMap

type memory_frame = 
	{
		varinfo_to_block: memory_block VarinfoMap.t;
	}
;;

type operator_action = (bytes*Cil.typ) list -> (bytes (* *Cil.typ*))
;;

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
		[Some (lval_block,size)], which means we should assign the result there. *)
type callingContext =
    | Runtime
    | Source of ((lval_block * int) option * Cil.stmt * Cil.instr * Cil.stmt)
	| NoReturn of Cil.instr
;;

type state =
	{
		global : memory_frame;				(* Map global lvals to blocks *)
		formals : memory_frame list;		(* Map formal lvals to blocks *)
		locals : memory_frame list;		(* Map local lvals to blocks *)
		(*heap : memory_heap;						(* Map a 4-byte thing to block *)*)
		callstack : Cil.fundec list;	(* Function call stack *)
		block_to_bytes : bytes MemoryBlockMap.t;
		path_condition : bytes list;
		path_condition_tracked : bool list;
		callContexts : callingContext list;
		(** The last element of callstack is the function at which the
				executor started execution. The last element of callContexts
				is [Runtime]. Other than that, the nth element of callstack is
				the fundec called by the [Instr] at the nth position in
				callContexts. *)
		
		va_arg : bytes list list;			(* A stack of va_arg *)
		va_arg_map : bytes list VargsMap.t;
		loc_map : bytes LocMap.t;     (* Map loc to symbolic bytes *)
        bytes_eval_cache : bool BytesMap.t; (* Map bytes to boolean value, if exists *) 
	}
;;

let word__size = 4
;;

(* http://www.c-faq.com/decl/strlitinit.html *)

let (string_table : bytes MemoryBlockMap.t ref) = ref MemoryBlockMap.empty;;

(*let (vargs_table : bytes list VargsMap.t ref) = ref VargsMap.empty;;*)

(* some globals that are helpful *)
let stp_count = ref 0;;

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
	bytesToVars : (bytes * Cil.varinfo) list;
		(** List associating symbolic bytes to the variable that was
				assigned this value by a call to __SYMBOLIC(&<variable>). *)
}

let emptyHistory = {
	coveredLines = LineSet.empty;
	coveredBlocks = StmtInfoSet.empty;
	coveredEdges = EdgeSet.empty;
	coveredConds = CondSet.empty;
	bytesToVars = [];
}

(** A set of (path condition, execution history) pairs *)
module PcHistSet = Set.Make
	(struct
		type t = bytes list * executionHistory
		let compare ((bl1,eh1):t) (bl2,eh2) =
			let bytesListCmp = Pervasives.compare bl1 bl2 in
			if bytesListCmp = 0
			then Pervasives.compare eh1.bytesToVars eh2.bytesToVars
			else bytesListCmp
	end)

(** This maps (Cil.exp,Cil.location) pairs to a pair (T_set,F_set) of
	PcHistSet refs, which are the sets of path conditions under which we
	took the true branch and false branch, respectively, of this condition. *)
let branches_taken : (Cil.exp * Cil.location, PcHistSet.t ref * PcHistSet.t ref) Hashtbl.t =
	Hashtbl.create 100

module SymbolSet = Set.Make
	(struct
		 type t = symbol
		 let compare (x:t) y = Pervasives.compare x.symbol_id y.symbol_id
	 end)

let signalStringOpt : string option ref = ref None
exception SignalException

type job = {
	state : state;
	exHist : executionHistory;
	instrList : Cil.instr list; (** [instr]s to execute before moving to the next [stmt] *)
	stmt : Cil.stmt;            (** The next statement the job should execute *)
	inTrackedFn : bool;         (** Is stmt in a function in the original program (as opposed to in a library or system call)? *)
	mergePoints : StmtInfoSet.t;     (** A list of potential merge points *)
	jid : int; (** A unique identifier for the job *)
}

type job_result = {
	result_state : state;
	result_history : executionHistory;
}

type job_completion =
	| Return of bytes option * job_result
	| Exit of bytes option * job_result
	| Abandoned of string * Cil.location * job_result
	| Truncated of job_result * job_result

type job_state =
	| Active of job
	| Fork of job * job
	| Complete of job_completion

module JobSet = Set.Make
	(struct
		 type t = job
		 let compare (job1:t) job2 =
			 (* I want the job with earliest stmt.sid to be first in the ordering *)
			 let c = Pervasives.compare job1.stmt.Cil.sid job2.stmt.Cil.sid in
			 if c = 0 then Pervasives.compare job1.jid job2.jid
			 else c
	 end)

(** Map [stmtInfo]s of [If] statements to the [stmtInfo]s of join
		points which the [If]s dominate.
		This will allow us to know where we should expect to merge paths. *)
let ifToJoinPointsHash : (stmtInfo,stmtInfo) Hashtbl.t = Hashtbl.create 500

module StringSet = Set.Make(String)
