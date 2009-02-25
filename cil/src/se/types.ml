
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

byte = (* corresponds to BV *)
| Byte_Concrete of char
| Byte_Symbolic of symbol
| Byte_Bytes of bytes * int (* condense a bytes into a byte, that can be put into an array *)  

and

bytes =
| Bytes_Constant of Cil.constant (* length=Cil.sizeOf (Cil.typeOf (Const(constant))) *)
| Bytes_ByteArray of byte ImmutableArray.t  (* content *) 
| Bytes_Address of memory_block option * bytes (* offset *)
| Bytes_Op of operator * (bytes * Cil.typ) list
| Bytes_Read of bytes * bytes * int						(* less preferrable type *)
| Bytes_Write of bytes * bytes * int * bytes	(* least preferrable type*)
| Bytes_FunPtr of Cil.fundec * bytes (* bytes is the "imaginary address" of the funptr *)
(*| Bytes_DS of data_structure*)
(* | Bytes_Concat  (* allow this to make things more efficient *) *)

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

module AddressMap =
	Utility.MakeMap (
	struct
		type t = bytes
		let compare a b = Hashtbl.hash a - Hashtbl.hash b
	end
	)
(*
type memory_heap = 
	{
		address_to_block: memory_block AddressMap.t;
	}
;;
*)
type operator_action = (bytes*Cil.typ) list -> (bytes (* *Cil.typ*))
;;

module MemoryBlockMap =
	Utility.MakeMap (
	struct
		type t = memory_block
		let compare a b = a.memory_block_id - b.memory_block_id
	end
	)

module VargsMap =
	Utility.MakeMap (
	struct
		type t = bytes 
		let compare a b = Hashtbl.hash a - Hashtbl.hash b				
	end
	)	

module LocMap =
	Utility.MakeMap (
	struct
		type t = Cil.location*int 
		let compare (a,ai) (b,bi) = if ai<>bi then ai-bi else
			let strcmp = String.compare a.Cil.file b.Cil.file in
			if strcmp <> 0 then strcmp else a.Cil.line - b.Cil.line
	end
	)	
	
(** A calling context is a triple [(destOpt,callInstr,nextStmtOpt)].
		[nextStmtOpt] is the [stmt] to execute after the call returns, or
		[None] if the call does not return; [callInstr] is the function
		call instruction; and [destOpt] is [None] if we ignore the result
		of the call, or it is [Some (block,offset,size)], which means we
		should assign the result to that triple. *)
type callingContext =
	(memory_block * bytes * int) option * Cil.instr * Cil.stmt option;;

type state =
	{
		global : memory_frame;				(* Map global lvals to blocks *)
		locals : memory_frame list;		(* Map local lvals to blocks *)
		(*heap : memory_heap;						(* Map a 4-byte thing to block *)*)
		callstack : Cil.fundec list;	(* Function call stack *)
		block_to_bytes : bytes MemoryBlockMap.t;
		path_condition : bytes list;
		callContexts : callingContext list;
		(** callContexts is off by one relative to callstack because
				callstack starts out as [main] while callContexts starts out
				empty. *)
		
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

type executionHistory = {
	edgesTaken : EdgeSet.t; (** Which edges we've traversed on this execution *)
	prevStmt : Cil.stmt; (** The [stmt] we just executed *)

	bytesToVars : (bytes * Cil.varinfo) list;
		(** List associating symbolic bytes to the variable that was
				assigned this value by a call to __SYMBOLIC(&<variable>). *)
}

let emptyHistory = {
	edgesTaken = EdgeSet.empty;
	prevStmt = Cil.dummyStmt;
	bytesToVars = [];
}

(** A set of path conditions *)
module PcSet = Set.Make
	(struct
		type t = bytes list
		let compare = compare
	end)

(** This maps (Cil.exp,Cil.location) pairs to a pair (T_set,F_set) of
	PcSet refs, which are the sets of path conditions under which we
	took the true branch and false branch, respectively, of this condition. *)
let branches_taken : (Cil.exp * Cil.location, PcSet.t ref * PcSet.t ref) Hashtbl.t =
	Hashtbl.create 100

module SymbolSet = Set.Make
	(struct
		 type t = symbol
		 let compare x y = x.symbol_id - y.symbol_id
	 end)

let signalStringOpt : string option ref = ref None
exception SignalException

module IntSet = Set.Make
	(struct
		 type t = int
		 let compare = (-)
	 end)

type job = {
	state : state;
	exHist : executionHistory;
	nextStmt : Cil.stmt; (** The next statement the job should execute *)
	mergePoints : IntSet.t; (** A list of potential merge points, by sid *)
	jid : int; (** A unique identifier for the job *)
}

let updateJob job state exHist nextStmt =
	{ job with
			state = state;
			exHist = exHist;
			nextStmt = nextStmt;
	}

let forkJob job nextStateT nextStateF nextStmtT nextStmtF newExHist newMergePoints =
	let j =
		{ job with mergePoints = newMergePoints; exHist = newExHist; } in
	(* Increment the jid of the job which takes the false branch *)
	({ j with state = nextStateT; nextStmt = nextStmtT; },
	 { j with state = nextStateF; nextStmt = nextStmtF;
			 jid = Utility.next_id Output.jidCounter; })

module JobSet = Set.Make
	(struct
		 type t = job
				 (** The state, executionHistory, and stmt about to be executed *)
		 let compare job1 job2 =
			 let c = job1.nextStmt.Cil.sid - job2.nextStmt.Cil.sid in
			 if c = 0 then compare (job1.state,job1.exHist,job1.mergePoints) (job2.state,job2.exHist,job2.mergePoints)
			 else c
	 end)

(** Map [sid]s of if statements to the [sid]s of join points which the
		[If]s dominate.
		This will allow to know where we should expect to merge paths. *)
let ifToJoinPointsHash : int Inthash.t = Inthash.create 500
