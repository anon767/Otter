open Cil
open Bytes
open BytesUtility
open Types

let libc___builtin_va_arg state exps =
	let state, key = Eval.rval state (List.hd exps) in
	let state, ret = MemOp.vargs_table__get state key in
	let lastarg = List.nth exps 2 in
	match lastarg with
		| CastE(_, AddrOf(cil_lval)) ->
			let state, lval = Eval.lval state cil_lval in
			let state = MemOp.state__assign state lval ret in
			(state, ret)
		| _ -> failwith "Last argument of __builtin_va_arg must be of the form CastE(_,AddrOf(lval))"


let libc___builtin_va_copy state exps =
	let state, keyOfSource = Eval.rval state (List.nth exps 1) in
	let srcList = MemOp.vargs_table__get_list state keyOfSource in
	let state, key = MemOp.vargs_table__add state srcList in
	match List.hd exps with
		| Lval(cil_lval) ->
			let state, lval = Eval.lval state cil_lval in
			let state = MemOp.state__assign state lval key in
			(state, bytes__zero)
		| _ -> failwith "First argument of va_copy must have lval"


let libc___builtin_va_end state exps =
	let state, key = Eval.rval state (List.hd exps) in
	let state = MemOp.vargs_table__remove state key in
	(state, bytes__zero)


let libc___builtin_va_start state exps =
	(* TODO: assign first arg with new bytes that maps to vargs *)
	match List.hd exps with
		| Lval(cil_lval) ->
			let state, key = MemOp.vargs_table__add state (List.hd state.va_arg) in
			let state, lval = Eval.lval state cil_lval in
			let state = MemOp.state__assign state lval key in
			(state, bytes__zero)
		| _ -> failwith "First argument of va_start must have lval"

let libc_free state exps =
	(* Remove the mapping of (block,bytes) in the state. *)
	(* From opengroup: The free() function causes the space pointed to by
	ptr to be deallocated; that is, made available for further allocation. If ptr
	is a null pointer, no action occurs. Otherwise, if the argument does not match
	a pointer earlier returned by the calloc(), malloc(), realloc() or valloc()
	function, or if the space is deallocated by a call to free() or realloc(), the
	behaviour is undefined.  Any use of a pointer that refers to freed space causes
	undefined behaviour.  *)
	let warning msg = Output.print_endline msg; (state,bytes__zero) in
	let state, ptr = Eval.rval state (List.hd exps) in
	match ptr with
		| Bytes_Address (block, _) ->
			if block.memory_block_type != Block_type_Heap
			then warning ("Freeing a non-malloced pointer:" ^ (To_string.exp (List.hd exps)) ^ " = " ^ (To_string.bytes ptr)) else 
			if not (MemOp.state__has_block state block)
			then warning ("Double-free:" ^ (To_string.exp (List.hd exps)) ^ " = " ^ (To_string.bytes ptr)) else 
			let state = MemOp.state__remove_block state block in
			(state, bytes__zero)
		| _ ->
			Output.set_mode Output.MSG_MUSTPRINT;
			warning ("Freeing something that is not a valid pointer: " ^ (To_string.exp (List.hd exps)) ^ " = " ^ (To_string.bytes ptr))



(* TODO: why are there two completely identical memset? *)
let libc_memset__concrete state exps =
	let state, bytes = Eval.rval state (List.hd exps) in
	let block, offset = bytes_to_address bytes in
	let state, old_whole_bytes = MemOp.state__get_bytes_from_block state block in
	let state, char_bytes = Eval.rval state (List.nth exps 1) in
	let c = bytes__get_byte char_bytes 0 (* little endian *) in
	let state, n_bytes = Eval.rval state (List.nth exps 2) in
	if isConcrete_bytes n_bytes then
		let n = bytes_to_int_auto n_bytes in
		let newbytes = bytes__make_default n c in
		let finalbytes = bytes__write old_whole_bytes offset n newbytes in
		let state = MemOp.state__add_block state block finalbytes in
		(state, bytes)
	else
		failwith "libc_memset__concrete: n is symbolic (TODO)"


let libc_memset state exps =
	let state, bytes = Eval.rval state (List.hd exps) in
	let block, offset = bytes_to_address bytes in
	let state, old_whole_bytes = MemOp.state__get_bytes_from_block state block in
	let state, char_bytes = Eval.rval state (List.nth exps 1) in
	let c = bytes__get_byte char_bytes 0 (* little endian *) in
	let state, n_bytes = Eval.rval state (List.nth exps 2) in
	if isConcrete_bytes n_bytes then
		let n = bytes_to_int_auto n_bytes in
		let newbytes = bytes__make_default n c in
		let finalbytes = bytes__write old_whole_bytes offset n newbytes in
		let state = MemOp.state__add_block state block finalbytes in
		(state, bytes)
	else
		failwith "libc_memset: n is symbolic (TODO)"


let libc_strlen state exps =
	let gotoFail () = failwith "libc_strlen: can't run builtin" in
	let state, bytes = Eval.rval state (List.hd exps) in
	match bytes with
		| Bytes_Address(block, offset) when isConcrete_bytes offset ->
			let state, bytes_str = MemOp.state__get_bytes_from_block state block in
			let strlen = match bytes_str with
				| Bytes_ByteArray(ba) ->
					let len = ImmutableArray.length ba in
					let n_offset = bytes_to_int_auto offset in
					let rec impl i l =
						if i>=len then l else
						match ImmutableArray.get ba i with
							| Byte_Concrete(c) -> if c='\000' then l else impl (i+1) (l+1)
							| _ -> gotoFail ()
					in
					impl n_offset 0
				| _ ->
					gotoFail ()
			in
			(state, lazy_int_to_bytes strlen)
		| _ ->
			gotoFail ()



let libc___create_file state exps = (state,bytes__zero)
let libc___error state exps = (state,bytes__zero)
let libc___maskrune state exps = (state,bytes__zero)
let libc___toupper state exps = (state,bytes__zero)
let libc_accept state exps = (state,bytes__zero)
let libc_bind state exps = (state,bytes__zero)
let libc_close state exps = (state,bytes__zero)
let libc_dup2 state exps = (state,bytes__zero)
let libc_execl state exps = (state,bytes__zero)
let libc_fclose state exps = (state,bytes__zero)
let libc_feof state exps = (state,bytes__zero)
let libc_fileno state exps = (state,bytes__zero)
let libc_fork state exps = (state,bytes__zero)
let libc_getc state exps = (state,bytes__zero)
let libc_getsockname state exps = (state,bytes__zero)
let libc_listen state exps = (state,bytes__zero)
let libc_open state exps = (state,bytes__zero)
let libc_pipe state exps = (state,bytes__zero)
let libc_putenv state exps = (state,bytes__zero)
let libc_read state exps = (state,bytes__zero)
let libc_recv state exps = (state,bytes__zero)
let libc_send state exps = (state,bytes__zero)
let libc_socket state exps = (state,bytes__zero)
let libc_stat state exps = (state,bytes__zero)
let libc_waitpid state exps = (state,bytes__zero)
let libc_write state exps = (state,bytes__zero)

let posix_umask state exps = (state,bytes__zero)
let posix_openlog state exps = (state,bytes__zero)
let posix_syslog state exps = (state,bytes__zero)

type t = state -> exp list -> state * bytes

let get = function
	| "__builtin_va_arg_fixed" -> libc___builtin_va_arg
	| "__builtin_va_arg" -> libc___builtin_va_arg
	| "__builtin_va_copy" -> libc___builtin_va_copy
	| "__builtin_va_end" -> libc___builtin_va_end
	| "__builtin_va_start" -> libc___builtin_va_start
(*	| "__create_file" -> libc___create_file *)
(*	| "__error" -> libc___error *)
(*	| "__maskrune" -> libc___maskrune*)
(*	| "__toupper" -> libc___toupper *)
(*	| "accept" -> libc_accept *)
(*	| "bind" -> libc_bind *)
(*	| "close" -> libc_close *)
(*	| "dup2" -> libc_dup2 *)
(*	| "execl" -> libc_execl *)
(*	| "fclose" -> libc_fclose *)
(*	| "feof" -> libc_feof *)
(*	| "fileno" -> libc_fileno  (* write to file *) *)
(*	| "fork" -> libc_fork *)
	| "free" -> libc_free
(*	| "getc" -> libc_getc *)
(*	| "getsockname" -> libc_getsockname *)
(*	| "listen" -> libc_listen *)
	| "memset" -> libc_memset
	| "memset__concrete" -> libc_memset__concrete
(*	| "strlen" -> libc_strlen *)
(*	| "open" -> libc_open *)
(*	| "pipe" -> libc_pipe *)
(*	| "putenv" -> libc_putenv *)
(*	| "read" -> libc_read *)
(*	| "recv" -> libc_recv *)
(*	| "send" -> libc_send *)
(*	| "socket" -> libc_socket *)
(*	| "stat" -> libc_stat *)
(*	| "waitpid" -> libc_waitpid *)
(*	| "write" -> libc_write *)
	(* these are from posix *)
(*	| "umask" -> posix_umask			*)
(*	| "openlog" -> posix_openlog	*)
(*	| "syslog" -> posix_syslog		*)

	| _ -> failwith "No such builtin function"


(* TODO: change this so that we don't call built-in functions twice
 * when they work. *)
let can_apply_builtin state fname args =
	try
		begin match fname with
			| "memset" ->
				let _ = libc_memset state args in ()
(* TODO: is this right? libc_strlen was never called before, because it's not in get *)
(*			| "strlen" ->
				ignore (libc_strlen state args) *)
			| _ ->
				let _ = get fname in ()
		end;
		true
	with Failure(_) ->
		false



(*********************************)
(* replace_func retopt exps loc job job_queue *)

let op_exps state exps binop =
	let rec impl exps =
		match exps with
			| [] -> failwith "AND/OR must take at least 1 argument"
			| h::[] -> h
			| h:: tail -> let t = impl tail in BinOp(binop, h, t, Cil.intType)
	in
	Eval.rval state (impl exps)

let stmtInfo_of_job job =
	{ siFuncName = (List.hd job.state.callstack).svar.vname;
		siStmt = Cilutility.stmtAtEndOfBlock job.stmt; }

let call_wrapper replace_func retopt exps loc job job_queue =
	(* Wrapper for calling an Otter function and advancing the execution to the next statement *)
	(* replace_func retopt exps loc job -> state *)

	let instr = List.hd job.instrList in

	let state_end = replace_func retopt exps loc job in

	let nextStmt =
		(* [stmt] is an [Instr] which doesn't end with a call to a
			 [noreturn] function, so it has exactly one successor. *)
		match job.stmt.succs with
			| [h] -> h
			| _ -> assert false
	in
	
	(* We didn't add the outgoing edge in exec_stmt because the
		 call might have never returned. Since there isn't an
		 explicit return (because we handle the call internally), we
		 have to add the edge now. *)
	let job =
		if job.inTrackedFn && Executeargs.run_args.Executeargs.arg_line_coverage
		then { job with 
				exHist = 
					(let instrLoc = get_instrLoc instr in
					{ job.exHist with coveredLines = LineSet.add (instrLoc.file, instrLoc.line) job.exHist.coveredLines; }
					); 
				instrList = [];
			}
		else {job with instrList = [];}
	in
	let nextExHist = ref job.exHist in
	if job.inTrackedFn && Executeargs.run_args.Executeargs.arg_edge_coverage then
		nextExHist := { !nextExHist with coveredEdges =
		EdgeSet.add (stmtInfo_of_job job,
			{ 
				siFuncName = (List.hd job.state.callstack).svar.vname;
				siStmt = Cilutility.stmtAtEndOfBlock nextStmt; })
				!nextExHist.coveredEdges; 
			};

	(* Update state, the stmt to execute, and exHist (which may
		 have gotten an extra bytesToVar mapping added to it). *)
	(Active { job with state = state_end; stmt = nextStmt; exHist = !nextExHist; }, job_queue)

let state_update_return_value retopt state bytes = 
	match retopt with
		| None ->
			state
		| Some cil_lval ->
			let state, lval = Eval.lval state cil_lval in
			MemOp.state__assign state lval bytes

let simple_call_wrapper replace_func retopt exps loc job job_queue =
	(* wrapper for simple non-side effecting functions *)
	(* replace_func state exps -> bytes *)
	let wrapper retopt exps loc job = 
		let (state, bytes) = replace_func job.state exps in
		state_update_return_value retopt state bytes
	in
	call_wrapper wrapper retopt exps loc job job_queue

(* __builtin_alloca is used for local arrays with variable size; has the same semantics as malloc *)
let libc___builtin_alloca__id = ref 1
let libc___builtin_alloca_size state size bytes loc =
	let name = Printf.sprintf "%s(%d)#%d/%s%s"
		(List.hd state.callstack).svar.vname
		size
		(Utility.next_id libc___builtin_alloca__id)
		(To_string.location loc)
		(MemOp.state__trace state)
	in
	let block = block__make name size Block_type_Heap in
	let addrof_block = make_Bytes_Address (block, bytes__zero) in
	let state = MemOp.state__add_block state block bytes in
	(state, addrof_block)

let libc___builtin_alloca retopt exps loc job =
	let state, b_size = Eval.rval job.state (List.hd exps) in
	let size =
		if isConcrete_bytes b_size then
			bytes_to_int_auto b_size (*safe to use bytes_to_int as arg should be small *)
		else
			1 (* currently bytearray have unbounded length *)
	in
	let bytes =
	  if Executeargs.run_args.Executeargs.arg_init_malloc_zero
	  then bytes__make size (* initially zero, as though malloc were calloc *)
	  else bytes__make_default size byte__undef (* initially the symbolic 'undef' byte *)
	in
    let (state, bytes) = libc___builtin_alloca_size state size bytes loc in
	state_update_return_value retopt state bytes


(* share implementation with __builtin_alloca *)
let libc_malloc = libc___builtin_alloca
	
