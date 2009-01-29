
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
(* | Bytes_Concat  (* allow this to make things more efficient *) *)

and

annotated_bytes =
| Annot_Bytes_Constant of Cil.constant (* length=Cil.sizeOf (Cil.typeOf (Const(constant))) *)
| Annot_Bytes_ByteArray of byte ImmutableArray.t  (* content *) 
| Annot_Bytes_Address of memory_block option * annotated_bytes (* offset *)
| Annot_Bytes_Op of operator * (annotated_bytes * Cil.typ) list
| Annot_Bytes_Read of annotated_bytes * annotated_bytes * int						(* less preferrable type *)
| Annot_Bytes_Write of annotated_bytes * annotated_bytes * int * annotated_bytes	(* least preferrable type*)
| Annot_Bytes_FunPtr of Cil.fundec * annotated_bytes (* annotated_bytes is the "imaginary address" of the funptr *)
| Annot_Bytes of Cil.varinfo * annotated_bytes (* varinfo is the variable with the associated bytes as its value *)

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
	
type statement = Statement of Cil.stmt | Instruction of Cil.instr * Cil.stmt | MainEntry ;;

type state =
	{
		global : memory_frame;				(* Map global lvals to blocks *)
		locals : memory_frame list;		(* Map local lvals to blocks *)
		(*heap : memory_heap;						(* Map a 4-byte thing to block *)*)
		callstack : Cil.fundec list;	(* Function call stack *)
		block_to_bytes : bytes MemoryBlockMap.t;
		path_condition : bytes list;
		human_readable_path_condition : annotated_bytes list;
		caller_stmts : statement list;	
		
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

(* Coerce a bytes into an annotated_bytes (without adding any annotations) *)
let rec bytes_to_annotated = function
	| Bytes_Constant(const) -> Annot_Bytes_Constant(const)
	| Bytes_ByteArray(arr) -> Annot_Bytes_ByteArray(arr)
	| Bytes_Address(memBlockOpt,off) -> Annot_Bytes_Address(memBlockOpt,bytes_to_annotated off)
	| Bytes_Op(op,bytes_typ_list) ->
		Annot_Bytes_Op(op,List.map (fun (b,t) -> (bytes_to_annotated b,t)) bytes_typ_list)
	| Bytes_Read(oldBytes,off,len) ->
		Annot_Bytes_Read(bytes_to_annotated oldBytes,bytes_to_annotated off,len)
	| Bytes_Write(oldBytes,offset,len,bytesToWrite) ->
		Annot_Bytes_Write(
			bytes_to_annotated oldBytes,
			bytes_to_annotated offset,
			len,
			bytes_to_annotated bytesToWrite)
	| Bytes_FunPtr(fdec,addr) -> Annot_Bytes_FunPtr(fdec,bytes_to_annotated addr)
;;

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
				assigned this value by a call to __SYMBOLIC(&<variable>), in
				reverse order (so the most recent is the head of the list).
				For purposes of mapping symbolic values back to the variables
				from which they came, if a given varinfo is paired with more
				than one bytes, ambiguities may arise if a report refers to
				'the' symbolic value given to this variable. *)
}

let emptyHistory = {
	edgesTaken = EdgeSet.empty;
	prevStmt = Cil.dummyStmt;
	bytesToVars = [];
}

(** A set of human-readable path conditions *)
module PcSet = Set.Make
	(struct
		type t = annotated_bytes list
		let compare = compare
	end)

(** This maps (Cil.exp,Cil.location) pairs to a pair (T_set,F_set) of
	PcSet refs, which are the sets of path conditions under which we
	took the true branch and false branch, respectively, of this condition. *)
let branches_taken : (Cil.exp * Cil.location, PcSet.t ref * PcSet.t ref) Hashtbl.t =
	Hashtbl.create 100
