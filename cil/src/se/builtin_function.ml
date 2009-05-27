open Cil
open Types

let libc___builtin_va_arg state exps = 	
	let key = Eval.rval state (List.hd exps) in
	let (state2,ret) = MemOp.vargs_table__get state key in
	let lastarg = List.nth exps 2 in
		match lastarg with
			| CastE(_,AddrOf(lval)) -> 
					let lvals = Eval.lval state lval in
					let size = (Cil.bitsSizeOf (Cil.typeOfLval lval))/8 in
					(* cast ret to sth of typ sth *)
					let ret2 = MemOp.bytes__resize ret size in
					let state3 = MemOp.state__assign state2 (lvals,size) ret2 in
						(state3,ret2)
			| _ -> failwith "Last argument of __builtin_va_arg must be of the form CastE(_,AddrOf(lval))"
		
;;
	
let libc___builtin_va_copy state exps = 
	let keyOfSource = Eval.rval state (List.nth exps 1) in
	let srcList = MemOp.vargs_table__get_list state keyOfSource in
	let (state2,key) = MemOp.vargs_table__add state srcList in
	match List.hd exps with
	|	Lval(lval) ->
			let lvals = Eval.lval state lval in
			let size = (Cil.bitsSizeOf (Cil.typeOfLval lval))/8 in
			let state3 = MemOp.state__assign state2 (lvals,size) key in
			(state3,MemOp.bytes__zero)
	|	_ -> failwith "First argument of va_copy must have lval"
;;

let libc___builtin_va_end state exps = 
	let key = Eval.rval state (List.hd exps) in
	let state2 = MemOp.vargs_table__remove state key in
	(state2,MemOp.bytes__zero)
;;

let libc___builtin_va_start state exps =
	(* TODO: assign first arg with new bytes that maps to vargs *)
	match List.hd exps with
		| Lval(lval) -> 
				let (state2,key) = MemOp.vargs_table__add state (List.hd state.va_arg) in
				let lvals = Eval.lval state lval in
				let size = (Cil.bitsSizeOf (Cil.typeOfLval lval))/8 in
				let state3 = MemOp.state__assign state2 (lvals,size) key in
					(state3,MemOp.bytes__zero)
		| _ -> failwith "First argument of va_start must have lval"
;;

let libc_malloc__id = ref 1;;
let libc_malloc state exps = 
	let b_size = Eval.rval state (List.hd exps) in
	let size = 
		if Convert.isConcrete_bytes b_size then
			Convert.bytes_to_int_auto b_size (*safe to use bytes_to_int as arg should be small *)
		else
			1 (* currently bytearray have unbounded length *)
	in
	let name = Printf.sprintf "malloc(%d)#%d/%s%s" 
		size
		(Utility.next_id libc_malloc__id) 
		(To_string.location !Output.cur_loc) 
		(MemOp.state__trace state)
	in
	let block =  MemOp.block__make name size Block_type_Heap in
(*	let bytes = Bytes_ByteArray ({ImmutableArray.empty with ImmutableArray.length = size}) in*)
(*	let bytes = MemOp.bytes__make_default size byte__undef in (* initially the symbolic 'undef' byte *) *)
	let bytes = MemOp.bytes__make size in (* initially zero, as though malloc were calloc *)
	let addrof_block = Bytes_Address (Some(block),MemOp.bytes__zero) in
	let state2 = MemOp.state__add_block state block bytes in
		(state2,addrof_block)	
;;

let libc_free state exps = 
  (* What it does is to remove the mapping of (block,bytes) in the state.
   *)
	let ptr = Eval.rval state (List.hd exps) in
      match ptr with
        | Bytes_Address (Some(block),_) -> 
            let state2 = MemOp.state__remove_block state block  in
              (state2,MemOp.bytes__zero)
        | _ ->
            Output.set_mode Output.MSG_DEBUG;
            Output.print_endline "Warning: memory leak!";
              (state,MemOp.bytes__zero)
;;

let libc_memset__concrete state exps = 
	let bytes = Eval.rval state (List.hd exps) in
	try
		let (blockopt,offset) = Convert.bytes_to_address bytes in
		match blockopt with None -> failwith "libc_memset: passed with null pointer" | Some(block) ->
		let old_whole_bytes = MemOp.state__get_bytes_from_block state block in
		let c = MemOp.bytes__get_byte (Eval.rval state (List.nth exps 1)) 0 (* little endian *) in
		let n_bytes = Eval.rval state (List.nth exps 2) in
		if Convert.isConcrete_bytes n_bytes then
			let n = Convert.bytes_to_int_auto n_bytes in
			let newbytes = MemOp.bytes__make_default n c in
			let finalbytes = MemOp.bytes__write old_whole_bytes offset n newbytes in
			let state2 = MemOp.state__add_block state block finalbytes  in
  			(state2,bytes)
		else failwith "libc_memset__concrete: n is symbolic (TODO)"
	with x -> raise x
;;
let libc_memset state exps = 
	let bytes = Eval.rval state (List.hd exps) in
	try
		let (blockopt,offset) = Convert.bytes_to_address bytes in
		match blockopt with None -> failwith "libc_memset: passed with null pointer" | Some(block) ->
		let old_whole_bytes = MemOp.state__get_bytes_from_block state block in
		let c = MemOp.bytes__get_byte (Eval.rval state (List.nth exps 1)) 0 (* little endian *) in
		let n_bytes = Eval.rval state (List.nth exps 2) in
		if Convert.isConcrete_bytes n_bytes then
			let n = Convert.bytes_to_int_auto n_bytes in
			let newbytes = MemOp.bytes__make_default n c in
			let finalbytes = MemOp.bytes__write old_whole_bytes offset n newbytes in
			let state2 = MemOp.state__add_block state block finalbytes  in
  			(state2,bytes)
		else failwith "libc_memset: n is symbolic (TODO)"
	with x -> raise x
;;

let libc_strlen state exps = 
	let gotoFail () = failwith "libc_strlen: can't run builtin" in
	let bytes = Eval.rval state (List.hd exps) in
	match bytes with
		| Bytes_Address(Some(block),offset) when Convert.isConcrete_bytes offset ->
			let bytes_str = MemOp.state__get_bytes_from_block state block in
			let strlen = match bytes_str with
				| Bytes_ByteArray(ba) ->
					let len = ImmutableArray.length ba in
					let n_offset = Convert.bytes_to_int_auto offset in
					let rec impl i l =
						if i>=len then l else
						match ImmutableArray.get ba i with
							| Byte_Concrete(c) -> if c='\000' then l else impl (i+1) (l+1)
							| _ -> gotoFail ()
					in
						impl n_offset 0
				| _ -> gotoFail ()
			in
				(state,Convert.lazy_int_to_bytes strlen)
		| _ -> gotoFail ()
	
;;

	
let libc___create_file state exps = (state,MemOp.bytes__zero);;
let libc___error state exps = (state,MemOp.bytes__zero);;
let libc___maskrune state exps = (state,MemOp.bytes__zero);;
let libc___toupper state exps = (state,MemOp.bytes__zero);;
let libc_accept state exps = (state,MemOp.bytes__zero);;
let libc_bind state exps = (state,MemOp.bytes__zero);;
let libc_close state exps = (state,MemOp.bytes__zero);;
let libc_dup2 state exps = (state,MemOp.bytes__zero);;
let libc_execl state exps = (state,MemOp.bytes__zero);;
let libc_fclose state exps = (state,MemOp.bytes__zero);;
let libc_feof state exps = (state,MemOp.bytes__zero);;
let libc_fileno state exps = (state,MemOp.bytes__zero);;
let libc_fork state exps = (state,MemOp.bytes__zero);;
let libc_getc state exps = (state,MemOp.bytes__zero);;
let libc_getsockname state exps = (state,MemOp.bytes__zero);;
let libc_listen state exps = (state,MemOp.bytes__zero);;
let libc_open state exps = (state,MemOp.bytes__zero);;
let libc_pipe state exps = (state,MemOp.bytes__zero);;
let libc_putenv state exps = (state,MemOp.bytes__zero);;
let libc_read state exps = (state,MemOp.bytes__zero);;
let libc_recv state exps = (state,MemOp.bytes__zero);;
let libc_send state exps = (state,MemOp.bytes__zero);;
let libc_socket state exps = (state,MemOp.bytes__zero);;
let libc_stat state exps = (state,MemOp.bytes__zero);;
let libc_waitpid state exps = (state,MemOp.bytes__zero);;
let libc_write state exps = (state,MemOp.bytes__zero);;

let posix_umask state exps = (state,MemOp.bytes__zero);;
let posix_openlog state exps = (state,MemOp.bytes__zero);;
let posix_syslog state exps = (state,MemOp.bytes__zero);;

type t = state -> exp list -> state * bytes

let get fname =
	let f = 
	match fname with
		| "__builtin_va_arg_fixed" -> libc___builtin_va_arg
		| "__builtin_va_arg" -> libc___builtin_va_arg
		| "__builtin_va_copy" -> libc___builtin_va_copy
		| "__builtin_va_end" -> libc___builtin_va_end
		| "__builtin_va_start" -> libc___builtin_va_start
(*		| "__create_file" -> libc___create_file *)
(*		| "__error" -> libc___error *)
(*		| "__maskrune" -> libc___maskrune*)
(*		| "__toupper" -> libc___toupper *)
(*		| "accept" -> libc_accept *)
(*		| "bind" -> libc_bind *)
(*		| "close" -> libc_close *)
(*		| "dup2" -> libc_dup2 *)
(*		| "execl" -> libc_execl *)
(*		| "fclose" -> libc_fclose *)
(*		| "feof" -> libc_feof *)
(*		| "fileno" -> libc_fileno  (* write to file *) *)
(*		| "fork" -> libc_fork *)
		| "free" -> libc_free
(*		| "getc" -> libc_getc *)
(*		| "getsockname" -> libc_getsockname *)
(*		| "listen" -> libc_listen *)
		| "malloc" -> libc_malloc
		| "memset" -> libc_memset
		| "memset__concrete" -> libc_memset__concrete
(*		| "strlen" -> libc_strlen *)
(*		| "open" -> libc_open *)
(*		| "pipe" -> libc_pipe *)
(*		| "putenv" -> libc_putenv *)
(*		| "read" -> libc_read *)
(*		| "recv" -> libc_recv *)
(*		| "send" -> libc_send *)
(*		| "socket" -> libc_socket *)
(*		| "stat" -> libc_stat *)
(*		| "waitpid" -> libc_waitpid *)
(*		| "write" -> libc_write *)
		(* these are from posix *)
(*		| "umask" -> posix_umask			*)
(*		| "openlog" -> posix_openlog	*)
(*		| "syslog" -> posix_syslog		*)

		| _ -> failwith "No such builtin function"
	in
			f
;;

(* TODO: change this so that we don't call built-in functions twice
	 when they work. *)
let can_apply_builtin state fname args =
	try
		let _ = get fname in
		match fname with
			| "memset" ->
				(try
					let (_,_) = libc_memset state args in 
						true
				with Failure(_) -> false)
			| "strlen" ->
				(try
					let (_,_) = libc_strlen state args in 
						true
				with Failure(_) -> false)
				
			| _ -> true
	with Failure(_) -> false
;;
