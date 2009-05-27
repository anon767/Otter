
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
		let compare a b = Pervasives.compare a.memory_block_id b.memory_block_id
	end
	)

module VargsMap =
	Utility.MakeMap (
	struct
		type t = bytes 
		let compare = Pervasives.compare				
	end
	)	

module LocMap =
	Utility.MakeMap (
	struct
		type t = Cil.location*int 
		let compare (a,ai) (b,bi) =
			if ai<>bi then Pervasives.compare ai bi else
				Cil.compareLoc a b
	end
	)	
	
(** A calling context may either be the symbolic executor, represented by
		[Runtime], or from another function in the source code, represented
		either by a triple [Source (destOpt,callInstr,nextStmt)] if the
		function returns or [NoReturn (callInstr)] if the function doesn't return.
		[nextStmt] is the [stmt] to execute after the call returns; [callInstr]
		is the function call instruction; and [destOpt] is [None] if we ignore
		the result of the call, or it is [Some (lval_block,size)], which
		means we should assign the result to that triple. *)
type callingContext =
    | Runtime
    | Source of ((lval_block * int) option * Cil.instr * Cil.stmt)
	| NoReturn of Cil.instr
;;

type state =
	{
		global : memory_frame;				(* Map global lvals to blocks *)
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
	}
;;

let word__size = 4
;;

(* A single global byte representing uninitialized memory *)
let byte__undef = Byte_Symbolic({symbol_id = 0});;

(* http://www.c-faq.com/decl/strlitinit.html *)

let (string_table : bytes MemoryBlockMap.t ref) = ref MemoryBlockMap.empty;;

(*let (vargs_table : bytes list VargsMap.t ref) = ref VargsMap.empty;;*)

(* some globals that are helpful *)
let stp_count = ref 0;;

module EdgeSet = Set.Make
	(struct
		type t = Cil.stmt*Cil.stmt
		(* Order edges primarily by source id, then by destination id *)
		let compare (src1,dst1) (src2,dst2) =
			let srcCmp = compare src1.Cil.sid src2.Cil.sid in
			if srcCmp = 0
			then compare dst1.Cil.sid dst2.Cil.sid
			else srcCmp
	end)

module IntSet = Set.Make
	(struct
		 type t = int
		 let compare (x: int) (y: int) = Pervasives.compare x y
	 end)

module LineSet = Set.Make
	(struct
		 type t = string * int (** (filename,line number) pair *)
		 let compare (f1,l1) (f2,l2) =
			 let tmp = compare f1 f2 in
			 if tmp = 0
			 then Pervasives.compare l1 l2
			 else tmp
	 end)

type executionHistory = {
	coveredLines : LineSet.t; (** Which lines we've hit *)
	coveredStmts : IntSet.t; (** Which statements we've hit, by sid *)
	coveredEdges : EdgeSet.t; (** Which edges we've traversed on this execution *)
	bytesToVars : (bytes * Cil.varinfo) list;
		(** List associating symbolic bytes to the variable that was
				assigned this value by a call to __SYMBOLIC(&<variable>). *)
}

let emptyHistory = {
	coveredLines = LineSet.empty;
	coveredStmts = IntSet.empty;
	coveredEdges = EdgeSet.empty;
	bytesToVars = [];
}

(** A set of (path condition, execution history) pairs *)
module PcHistSet = Set.Make
	(struct
		type t = bytes list * executionHistory
		let compare (bl1,eh1) (bl2,eh2) =
			let bytesListCmp = compare bl1 bl2 in
			if bytesListCmp = 0
			then compare eh1.bytesToVars eh2.bytesToVars
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
		 let compare x y = Pervasives.compare x.symbol_id y.symbol_id
	 end)

let signalStringOpt : string option ref = ref None
exception SignalException

type job = {
	state : state;
	exHist : executionHistory;
	instrList : Cil.instr list; (** [instr]s to execute before moving to the next [stmt] *)
	stmt : Cil.stmt;            (** The next statement the job should execute *)
	inTrackedFn : bool;         (** Is nextStmt in a function in the original program (as opposed to in a library or system call)? *)
	mergePoints : IntSet.t;     (** A list of potential merge points, by sid *)
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
		 let compare job1 job2 =
			 (* I want the job with earliest stmt.sid to be first in the ordering *)
			 let c = Pervasives.compare job1.stmt.Cil.sid job2.stmt.Cil.sid in
			 if c = 0 then Pervasives.compare job1.jid job2.jid
			 else c
	 end)

(** Map [sid]s of if statements to the [sid]s of join points which the
		[If]s dominate.
		This will allow to know where we should expect to merge paths. *)
let ifToJoinPointsHash : int Inthash.t = Inthash.create 500

module StringSet = Set.Make(String)
