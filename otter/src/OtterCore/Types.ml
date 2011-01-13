open DataStructures
open OtterBytes
open Bytes

module VarinfoMap = Map.Make (CilUtilities.CilData.CilVar)

module MallocMap = Map.Make (CilUtilities.CilData.Malloc)

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
		function returns or [NoReturn (callStmt,callInstr)] if the function doesn't return.
		[nextStmt] is the [stmt] to execute after the call returns; [callStmt]
		and [callInstr] are the function call statement and instruction;
		and [destOpt] is [None] if we ignore the result of the call, or it is
		[Some Cil.lval], which means we should assign the result there. *)
type callingContext =
    | Runtime
    | Source of (Cil.lval option * Cil.stmt * Cil.instr * Cil.stmt)
    | NoReturn of (Cil.stmt * Cil.instr)



type memory_frame =
	(state, lval_block) Deferred.t VarinfoMap.t
and state =
	{
		global : memory_frame;                  (* Map global lvals to blocks *)
		formals : memory_frame list;            (* Map formal lvals to blocks *)
		locals : memory_frame list;             (* Map local lvals to blocks *)
		aliases : memory_block list VarinfoMap.t; (* Map from varinfos to aliased blocks, e.g., from unknown call stack recursion *)
		mallocs : memory_block list MallocMap.t; (* Map from malloc sites to aliased blocks from unknown allocation *)
		callstack : Cil.fundec list;            (* Function call stack *)
		block_to_bytes : (state, bytes) Deferred.t MemoryBlockMap.t;
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
	}



(* http://www.c-faq.com/decl/strlitinit.html *)

let (string_table : bytes MemoryBlockMap.t ref) = ref MemoryBlockMap.empty

(*let (vargs_table : bytes list VargsMap.t ref) = ref VargsMap.empty*)

