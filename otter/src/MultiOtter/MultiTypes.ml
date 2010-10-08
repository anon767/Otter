open DataStructures
open OcamlUtilities
open OtterBytes
open OtterCore
open Types

type program_counter = {
	instrList : Cil.instr list;
	stmt : Cil.stmt;
}

type local_state = {
	global : memory_frame;
	formals : memory_frame list;
	locals : memory_frame list;
	callstack : Cil.fundec list;
	callContexts : callingContext list;
	stmtPtrs : callingContext IndexMap.t;
	va_arg : Bytes.bytes list list;
	va_arg_map : Bytes.bytes list VargsMap.t;
	block_to_bytes : (state, Bytes.bytes) Deferred.t MemoryBlockMap.t;
	pid : int;
}

type shared_state = {
	path_condition : Bytes.bytes list;
	shared_block_to_bytes : (state, Bytes.bytes) Deferred.t MemoryBlockMap.t;
}

type multijob = {
	file : Cil.file;
	processes : (program_counter * local_state) list;
	shared : shared_state;
	jid : int;
	next_pid : int;
	current_pid : int;
}
