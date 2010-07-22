(**

This module contains a library of built-in functions for Otter.

*)

open Cil
open Ternary
open Bytes
open BytesUtility
open Types
open Interceptors

(*

Implementation notes:

A function call operates on a job and returns one or more job results:
	- for a successful function call:
		- update state in job
			- evaluate the arguments
			- perform the function
			- assign the return value
		- update program counter in job
		- update coverage in job
		- return an Active job result
	- for an unsuccessful function call:
		- return a Complete job result
	- return the job result (if only one), or Fork of job results (more than one)

*)


(** Convenience function to join a list of expressions using a binary operator and evaluate it.
		@param state is the symbolic executor state in which to evaluate the list of expressions
		@param exps is the list of expressions to join and evaluate
		@param binop is the binary operator used to join the list of expressions
		@return the updated state and the evaluated, joined expression
*)
let eval_join_exps state exps binop =
	let rec join_exps = function
		| x::[] -> x
		| x::xs -> BinOp(binop, x, join_exps xs, Cil.intType)
		| [] -> failwith "AND/OR must take at least 1 argument"
	in
	Eval.rval state (join_exps exps)


(** Convenience function to assign a value to an optional return lvalue.
		@param state is the symbolic executor state in which to evaluate the return lvalue
		@param retopt is the optional return lvalue
		@param bytes is the value to assign to the return lvalue
		@return the updated state
*)
let set_return_value state retopt bytes =
	(* TODO: also cast bytes to the expected return type of the function, since, according to
	   cil/doc/api/Cil.html#TYPEinstr, the return lvalue may not match the return type of the
	   function *)
	match retopt with
		| None ->
			state
		| Some cil_lval ->
			let state, lval = Eval.lval state cil_lval in
			MemOp.state__assign state lval bytes


(** Convenience function to end a function call in a symbolic executor job with the standard epilogue of incrementing
	the program counter and updating coverage information.
		@param job is the symbolic executor job in which to end a function call
		@return the updated job
*)
let end_function_call job =
	(* Increment the program counter. *)
	let stmt =
		(* [stmt] is an [Instr] which doesn't end with a call to a
			 [noreturn] function, so it has exactly one successor. *)
		match job.stmt.succs with
			| [h] -> h
			| _ -> assert false
	in

	(* Update coverage. *)
	let exHist = job.exHist in
	let exHist =
		(* We didn't add the outgoing edge in exec_stmt because the
			 call might have never returned. Since there isn't an
			 explicit return (because we handle the call internally), we
			 have to add the edge now. *)
		if job.inTrackedFn && Executeargs.run_args.Executeargs.arg_line_coverage then
			let loc = Core.get_job_loc job in
			{ exHist with coveredLines = LineSet.add (loc.Cil.file, loc.Cil.line) exHist.coveredLines; }
		else
			exHist
	in
	let exHist =
		(* Update edge coverage. *)
		if job.inTrackedFn && Executeargs.run_args.Executeargs.arg_edge_coverage then
			let fn = (List.hd job.state.callstack).svar.vname in
			let edge = (
				{ siFuncName = fn; siStmt = Cilutility.stmtAtEndOfBlock job.stmt; },
				{ siFuncName = fn; siStmt = Cilutility.stmtAtEndOfBlock stmt; }
			) in
			{ exHist with coveredEdges = EdgeSet.add edge exHist.coveredEdges; }
		else
			exHist
	in

	(* Update the state, program counter, and coverage  *)
	{ job with stmt = stmt; exHist = exHist; instrList = []; }


(** Convenience wrapper for creating function call handlers that works on symbolic executor jobs from simplified
	function call handlers that works on the symbolic executor states in the jobs.
		@param fn is the simplified function call handler to wrap
		@return a function call handler
*)
let wrap_state_function fn =
	fun job retopt exps ->
		let job = { job with state = fn job.state retopt exps } in
		let job = end_function_call job in
		Active job


(** Function Implimentations **)

let libc___builtin_va_arg = wrap_state_function begin fun state retopt exps ->
	let state, key = Eval.rval state (List.hd exps) in
	let state, ret = MemOp.vargs_table__get state key in
	let lastarg = List.nth exps 2 in
	match lastarg with
		| CastE(_, AddrOf(cil_lval)) ->
			let state, lval = Eval.lval state cil_lval in
			let state = MemOp.state__assign state lval ret in
			set_return_value state retopt ret
		| _ -> failwith "Last argument of __builtin_va_arg must be of the form CastE(_,AddrOf(lval))"
end


let libc___builtin_va_copy = wrap_state_function begin fun state retopt exps ->
	let state, keyOfSource = Eval.rval state (List.nth exps 1) in
	let srcList = MemOp.vargs_table__get_list state keyOfSource in
	let state, key = MemOp.vargs_table__add state srcList in
	match List.hd exps with
		| Lval(cil_lval) ->
			let state, lval = Eval.lval state cil_lval in
			let state = MemOp.state__assign state lval key in
			set_return_value state retopt bytes__zero
		| _ -> failwith "First argument of va_copy must have lval"
end


let libc___builtin_va_end = wrap_state_function begin fun state retopt exps ->
	let state, key = Eval.rval state (List.hd exps) in
	let state = MemOp.vargs_table__remove state key in
	set_return_value state retopt bytes__zero
end


let libc___builtin_va_start = wrap_state_function begin fun state retopt exps ->
	(* TODO: assign first arg with new bytes that maps to vargs *)
	match List.hd exps with
		| Lval(cil_lval) ->
			let state, key = MemOp.vargs_table__add state (List.hd state.va_arg) in
			let state, lval = Eval.lval state cil_lval in
			let state = MemOp.state__assign state lval key in
			set_return_value state retopt bytes__zero
		| _ -> failwith "First argument of va_start must have lval"
end


let libc_free = wrap_state_function begin fun state retopt exps ->
	(* Remove the mapping of (block,bytes) in the state. *)
	(* From opengroup: The free() function causes the space pointed to by
	ptr to be deallocated; that is, made available for further allocation. If ptr
	is a null pointer, no action occurs. Otherwise, if the argument does not match
	a pointer earlier returned by the calloc(), malloc(), realloc() or valloc()
	function, or if the space is deallocated by a call to free() or realloc(), the
	behaviour is undefined.  Any use of a pointer that refers to freed space causes
	undefined behaviour.  *)
	let warning msg = Output.print_endline msg; set_return_value state retopt bytes__zero in
	let state, ptr = Eval.rval state (List.hd exps) in
	match ptr with
		| Bytes_Address (block, _) ->
			if block.memory_block_type != Block_type_Heap
			then warning ("Freeing a non-malloced pointer:" ^ (To_string.exp (List.hd exps)) ^ " = " ^ (To_string.bytes ptr)) else 
			if not (MemOp.state__has_block state block)
			then warning ("Double-free:" ^ (To_string.exp (List.hd exps)) ^ " = " ^ (To_string.bytes ptr)) else 
			let state = MemOp.state__remove_block state block in
			set_return_value state retopt bytes__zero
		| _ ->
			Output.set_mode Output.MSG_MUSTPRINT;
			warning ("Freeing something that is not a valid pointer: " ^ (To_string.exp (List.hd exps)) ^ " = " ^ (To_string.bytes ptr))
end


let libc_memset = wrap_state_function begin fun state retopt exps ->
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
		set_return_value state retopt bytes
	else
		failwith "libc_memset: n is symbolic (TODO)"
end


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

let libc___builtin_alloca job retopt exps =
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
	let state, bytes = libc___builtin_alloca_size state size bytes (Core.get_job_loc job) in
	let job = end_function_call { job with state = set_return_value state retopt bytes } in
	Active job


let otter_given = wrap_state_function begin fun state retopt exps ->
	if List.length exps <> 2 then 
		failwith "__GIVEN takes 2 arguments"
	else
		let truthvalue =
			let state, given = Eval.rval state (List.nth exps 0) in
			let state, rv = Eval.rval state (List.nth exps 1 ) in
			let state, truth = MemOp.eval_with_cache state (given::state.path_condition) rv in
			if truth == True then lazy_int_to_bytes 1
			else if truth == False then lazy_int_to_bytes 0
			else bytes__symbolic (bitsSizeOf intType / 8)
		in
		set_return_value state retopt truthvalue
end


let otter_truth_value = wrap_state_function begin fun state retopt exps ->
	let truthvalue = 
		lazy_int_to_bytes
		begin
			if List.length exps = 0 then 0 
			else
				let state, rv = Eval.rval state (List.hd exps) in
				let state, truth = MemOp.eval_with_cache state state.path_condition rv in
				if truth == True then 1
				else if truth == False then -1
				else 0
		end
	in
	set_return_value state retopt truthvalue
end


let libc_exit job retopt exps =
	Output.set_mode Output.MSG_MUSTPRINT;
	let exit_code = 
		match exps with
			| exp1::_ -> 
				Output.print_endline ("exit() called with code "^(To_string.bytes (snd (Eval.rval job.state exp1))));
				Some ((snd (Eval.rval job.state exp1)))
			| [] -> 
				Output.print_endline ("exit() called with code (NONE)");
				None
	in
	Complete (Types.Exit (exit_code, { result_state = job.state; result_history = job.exHist; }))

let otter_evaluate = wrap_state_function begin fun state retopt exps ->
	let state, bytes = eval_join_exps state exps Cil.LAnd in
	Output.set_mode Output.MSG_MUSTPRINT;
	Output.print_endline ("	Evaluates to "^(To_string.bytes bytes));
	state
end


let otter_evaluate_string = wrap_state_function begin fun state retopt exps ->
	let exp = List.hd exps in
	let sizeexp = List.nth exps 1 in
	let state, addr_bytes = Eval.rval state exp in
	let state, str = 
		match addr_bytes with
			| Bytes_Address(block, offset) ->
				let state, size_bytes = Eval.rval state sizeexp in
				let size =
					try bytes_to_int_auto size_bytes with
					  Failure(s) -> Output.print_endline s; 32
				in
				let state, bytes = MemOp.state__deref state (conditional__lval_block (block, offset), size) in
				let str = 
					match bytes with
						| Bytes_ByteArray(bytearray) -> To_string.bytestring bytearray
						| Bytes_Constant(CInt64(i,_,_)) -> Int64.to_string i
						| _ -> "(complicate)"
				in
					(state, str)
			| _ ->
				(state, "(nil)")
	in
	Output.set_mode Output.MSG_MUSTPRINT;
	Output.print_endline (
		"Evaluates to string: \"" ^ (
			if (Executeargs.print_args.Executeargs.arg_print_no_escaped_string)
			then
				str
			else
				String.escaped str
			) ^ "\""
	);
	state
end


let otter_symbolic_state = wrap_state_function begin fun state retopt exps ->
	MemoryBlockMap.fold 
		begin fun block _ state ->
			(* TODO: what about deferred bytes? *)
			(* TODO: handle pointers with an alias analysis *)
			let state, bytes = MemOp.state__get_bytes_from_block state block in
				match bytes with
					| Bytes_FunPtr(_) ->
						state
					| _ ->
						MemOp.state__add_block state block (bytes__symbolic (bytes__length bytes))
		end 
		state.block_to_bytes
		state
end


let otter_assume = wrap_state_function begin fun state retopt exps ->
	let state, pc = eval_join_exps state exps Cil.LAnd in
	MemOp.state__add_path_condition state pc false
end


let otter_path_condition = wrap_state_function begin fun state retopt exps ->
	let pc_str = (Utility.print_list To_string.bytes state.path_condition "\n AND \n") in
	Output.set_mode Output.MSG_MUSTPRINT;
	Output.print_endline (if String.length pc_str = 0 then "(nil)" else pc_str);
	state
end


let otter_assert = wrap_state_function begin fun state retopt exps ->
	let state, assertion = eval_join_exps state exps Cil.LAnd in
	Eval.check state assertion exps
end


let otter_if_then_else = wrap_state_function begin fun state retopt exps ->
	let state, bytes0 = Eval.rval state (List.nth exps 0) in
	let state, bytes1 = Eval.rval state (List.nth exps 1) in
	let state, bytes2 = Eval.rval state (List.nth exps 2) in
	let c = IfThenElse (
		guard__bytes bytes0, conditional__bytes bytes1, conditional__bytes bytes2
	) in
	let rv = make_Bytes_Conditional c in
	set_return_value state retopt rv
end


let otter_boolean_op binop = wrap_state_function begin fun state retopt exps ->
	let state, rv = eval_join_exps state exps binop in
	set_return_value state retopt rv
end


let otter_boolean_not = wrap_state_function begin fun state retopt exps ->
	let state, rv = Eval.rval state (UnOp(Cil.LNot, List.hd exps, Cil.voidType)) in
	set_return_value state retopt rv
end


let otter_comment = wrap_state_function begin fun state retopt exps ->
	let exp = List.hd exps in
	Output.set_mode Output.MSG_MUSTPRINT;
	Output.print_endline ("COMMENT:"^(To_string.exp exp));
	state
end


let otter_break_pt = wrap_state_function begin fun state retopt exps ->
	Output.set_mode Output.MSG_REG;
	Output.print_endline "Option (h for help):";
	Scanf.scanf "%d\n" 
	begin
		fun p1->
			Output.printf "sth\n";	
			state
	end
end


let otter_print_state = wrap_state_function begin fun state retopt exps ->
	Output.set_mode Output.MSG_MUSTPRINT;
	let arg_print_char_as_int = Executeargs.print_args.Executeargs.arg_print_char_as_int in
	Executeargs.print_args.Executeargs.arg_print_char_as_int <- true;
	let module BOSMap = Utility.MakeMap (
		struct
		type t = memory_block * bytes * int  (* block, offset, size *)
		let compare = Pervasives.compare				
		end)	
	in
	let bosmap = ref BOSMap.empty in
	let printStringString s1 s2 =
		Output.print_endline (s1 ^ " = " ^ s2)
	in
	let rec printVarFieldsBytes varname typ bytes off =
		(* break down each local by its fields *)
		(* canonicalize concrete values by their array rep*)
		match typ with
			| TComp (compinfo,_) -> 
				List.iter 
					(fun fieldinfo -> 
						printVarFieldsBytes 
							(varname^"."^fieldinfo.fname) 
							fieldinfo.ftype bytes 
							(off + fst(Cil.bitsOffset typ (Field(fieldinfo,NoOffset)))/8)
					)
				  compinfo.cfields
			| _ -> 
				let size = (Cil.bitsSizeOf typ/8) in
				let rec p b = 
					match b with
						| Bytes_Constant const ->  p (constant_to_bytes const)
						| Bytes_ByteArray ba -> To_string.bytes (Bytes_ByteArray(ImmutableArray.sub ba off size))
						| Bytes_Address (block, boff) -> 
							if off = 0 && size = (bitsSizeOf voidPtrType / 8) then begin
								bosmap := BOSMap.add (block,boff,size) None (!bosmap);
								To_string.bytes b
							end else
								failwith (Printf.sprintf "PRINT STATE: Reading part of a Bytes_Address: %s %d %d" (To_string.bytes b) off size)
						| _ -> "("^(To_string.bytes b)^","^(string_of_int off)^","^(string_of_int size)^")"
				in 
				let rhs = p bytes in 
				printStringString varname rhs
	in
	let printVarBytes var bytes =
		printVarFieldsBytes var.vname var.vtype bytes 0 
	in
	let printVar var lval_block =
		if Cilutility.isConstType var.vtype then () 
		else
			match lval_block with
				| Immediate (Unconditional (block, _)) ->
					begin match (MemoryBlockMap.find block state.block_to_bytes) with
						| Immediate bytes -> printVarBytes var bytes
						| Deferred _ -> printStringString var.vname "(deferred)"
					end
				(* TODO: print something useful *)
				| Immediate (IfThenElse _) ->
					printStringString var.vname "(IfThenElse)"
				| Immediate (ConditionalException _) ->
					printStringString var.vname "(ConditionalException)"
				| Deferred _ ->
					printStringString var.vname "(deferred)"
	in
	
	Output.print_endline "#BEGIN PRINTSTATE";
	Output.print_endline "#Globals:";
	VarinfoMap.iter printVar state.global;
	Output.print_endline "#Locals:";
	VarinfoMap.iter printVar (List.hd state.locals);
	Output.print_endline "#Formals:";
	VarinfoMap.iter printVar (List.hd state.formals);
	(* explore only one level of memory *)
	bosmap := BOSMap.mapi begin fun (block,off,size) des -> match des with
		| Some _ -> des
		| None -> Some (snd(MemOp.state__deref state (conditional__lval_block (block, off), size)))
	end (!bosmap);
	Output.print_endline "#Memory: (one level)";
	BOSMap.iter (fun (block,off,size) des -> 
		let sdes =
			match des with 
				| None -> "None"
				| Some b -> To_string.bytes b
		in
		Output.print_endline ((To_string.bytes (Bytes_Address(block, off))) ^ " -> " ^ sdes)
	)
	(!bosmap);
	Output.print_endline "#END PRINTSTATE";
	Executeargs.print_args.Executeargs.arg_print_char_as_int <- arg_print_char_as_int;
	state
end


let otter_current_state = wrap_state_function begin fun state retopt exps ->
	let state, bytes = Eval.rval state (List.hd exps) in
	let key = bytes_to_int_auto bytes in
	Output.set_mode Output.MSG_MUSTPRINT;
	Output.printf "Record state %d\n" key;
	MemOp.index_to_state__add key state;
	state
end


let otter_compare_state = wrap_state_function begin fun state retopt exps ->
	let state, bytes0 = Eval.rval state (List.nth exps 0) in
	let state, bytes1 = Eval.rval state (List.nth exps 1) in
	let key0 = bytes_to_int_auto bytes0 in
	let key1 = bytes_to_int_auto bytes1 in
	Output.set_mode Output.MSG_MUSTPRINT;
	Output.printf "Compare states %d and %d\n" key0 key1;
	begin try
		let s0 = try MemOp.index_to_state__get key0 
		with Not_found -> (
		   	Output.set_mode Output.MSG_MUSTPRINT;
		   	Output.printf "Warning: snapshot %d is absent\n" key0;
			raise Not_found
		)
		in
		let s1 = try MemOp.index_to_state__get key1
			with Not_found -> (
			Output.set_mode Output.MSG_MUSTPRINT;
			Output.printf "Warning: snapshot %d is absent\n" key1;
			raise Not_found
		)
		in
		Output.set_mode Output.MSG_MUSTPRINT;
		ignore (MemOp.cmp_states s0 s1);
		state
	with Not_found -> 
	   	Output.set_mode Output.MSG_MUSTPRINT;
	   	Output.printf "Compare states fail\n";
		state
	end
end


let otter_assert_equal_state = wrap_state_function begin fun state retopt exps ->
	let state, bytes0 = Eval.rval state (List.nth exps 0) in
	let key0 = bytes_to_int_auto bytes0 in
	begin try 
		let s0 = MemOp.index_to_state__get key0 in
		Output.set_mode Output.MSG_MUSTPRINT;
		if MemOp.cmp_states s0 state then
			Output.print_endline "AssertEqualState satisfied";
			MemOp.index_to_state__add key0 state; 
			state
	with Not_found -> 
		MemOp.index_to_state__add key0 state; 
		state
	end
end


(* There are 2 ways to use __SYMBOLIC:
	 (1) '__SYMBOLIC(&x);' gives x a fresh symbolic value and associates
	 that value with the variable x.
	 (2) 'x = __SYMBOLIC(n);' assigns to x a fresh symbolic value which
	 is not associated to any variable. If n > 0, n is the number of
	 symbolic bytes desired; if n <= 0, a number of symbolic bytes equal
	 to the size of x is returned. (If you manage to get something like
	 'x = __SYMBOLIC();' past CIL despite the disagreement in the number
	 of arguments, this behaves like the n <= 0 case.) *)
let intercept_symbolic job job_queue interceptor =
	match job.Types.instrList with
		| Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc)::_ when varinfo.vname = "__SYMBOLIC" ->

			let job = match exps with
				| [AddrOf (Var varinf, NoOffset as cil_lval)]
				| [CastE (_, AddrOf (Var varinf, NoOffset as cil_lval))] ->

					let state = job.state in
					let exHist = job.exHist in

					(* If we are given a variable's address, we track the symbolic value.
						 But make sure we don't give the same variable two different values. *)
					if List.exists (fun (_,v) -> v == varinf) exHist.bytesToVars
					then Errormsg.s
						(Errormsg.error "Can't assign two tracked values to variable %s" varinf.vname);

					let state, (_, size as lval) = Eval.lval state cil_lval in
					let symbBytes = bytes__symbolic size in
					Output.set_mode Output.MSG_MUSTPRINT;
					Output.print_endline (varinf.vname ^ " = " ^ (To_string.bytes symbBytes));

					let exHist = { exHist with bytesToVars = (symbBytes,varinf)::exHist.bytesToVars } in
					let state = MemOp.state__assign state lval symbBytes in

					end_function_call { job with state = state; exHist = exHist }

				| _ ->

					let state = job.state in
					begin match retopt with
						| None -> failwith "Incorrect usage of __SYMBOLIC(): symbolic value generated and ignored"
						| Some lval ->
							let state, (_, size as lval) = Eval.lval state lval in
							let state, size = match exps with
								| [] -> (state, size)
								| [CastE (_, h)] | [h] ->
									let state, bytes = Eval.rval state h in
									let newsize = bytes_to_int_auto bytes in
									(state, if newsize <= 0 then size else newsize)
								| _ -> failwith "__SYMBOLIC takes at most one argument"
							in
							let state = MemOp.state__assign state lval (bytes__symbolic size) in
							end_function_call { job with state = state }
					end
			in
			(Active job, job_queue)

		| _ -> 
			interceptor job job_queue


let interceptor job job_queue interceptor =
	try
		(
		(* intercept builtin functions *)
		(                                  (*"__SYMBOLIC"*)                  intercept_symbolic) @@
		(intercept_function_by_name_internal "__builtin_alloca"        libc___builtin_alloca) @@
		(intercept_function_by_name_internal "malloc"                  libc___builtin_alloca) @@
		(intercept_function_by_name_internal "free"                    libc_free) @@
		(intercept_function_by_name_internal "__builtin_va_arg_fixed"  libc___builtin_va_arg) @@
		(intercept_function_by_name_internal "__builtin_va_arg"        libc___builtin_va_arg) @@
		(intercept_function_by_name_internal "__builtin_va_copy"       libc___builtin_va_copy) @@
		(intercept_function_by_name_internal "__builtin_va_end"        libc___builtin_va_end) @@
		(intercept_function_by_name_internal "__builtin_va_start"      libc___builtin_va_start) @@
		(* memset defaults to the C implimentation on failure *)
		(try_with_job_abandoned_interceptor
		(intercept_function_by_name_internal "memset"                  libc_memset)) @@
		(intercept_function_by_name_internal "memset__concrete"        libc_memset) @@
		(intercept_function_by_name_internal "exit"                    libc_exit) @@
		(intercept_function_by_name_internal "__TRUTH_VALUE"           otter_truth_value) @@
		(intercept_function_by_name_internal "__GIVEN"                 otter_given) @@
		(intercept_function_by_name_internal "__EVAL"                  otter_evaluate) @@
		(intercept_function_by_name_internal "__EVALSTR"               otter_evaluate_string) @@
		(intercept_function_by_name_internal "__SYMBOLIC_STATE"        otter_symbolic_state) @@
		(intercept_function_by_name_internal "__ASSUME"                otter_assume) @@
		(intercept_function_by_name_internal "__PATHCONDITION"         otter_path_condition) @@
		(intercept_function_by_name_internal "__ASSERT"                otter_assert) @@
		(intercept_function_by_name_internal "__ITE"                   otter_if_then_else) @@
		(intercept_function_by_name_internal "AND"                     (otter_boolean_op Cil.LAnd)) @@
		(intercept_function_by_name_internal "OR"                      (otter_boolean_op Cil.LOr)) @@
		(intercept_function_by_name_internal "NOT"                     otter_boolean_not) @@
		(intercept_function_by_name_internal "__COMMENT"               otter_comment) @@
		(intercept_function_by_name_internal "__BREAKPT"               otter_break_pt) @@
		(intercept_function_by_name_internal "__PRINT_STATE"           otter_print_state) @@
		(intercept_function_by_name_internal "__CURRENT_STATE"         otter_current_state) @@
		(intercept_function_by_name_internal "__COMPARE_STATE"         otter_compare_state) @@
		(intercept_function_by_name_internal "__ASSERT_EQUAL_STATE"    otter_assert_equal_state) @@
		(* pass on the job when none of those match *)
		interceptor

		) job job_queue
	with Failure msg ->
		if Executeargs.run_args.Executeargs.arg_failfast then failwith msg;
		let result = { result_state = job.state; result_history = job.exHist } in
		(Complete (Types.Abandoned (msg, Core.get_job_loc job, result)), job_queue)
