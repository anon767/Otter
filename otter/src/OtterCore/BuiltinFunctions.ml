(**

This module contains a library of built-in functions for Otter.

*)

open DataStructures
open OcamlUtilities
open Cil
open OtterBytes
open Bytes
open BytesUtility
open Types
open Job
open Interceptor

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
let eval_join_exps state exps binop errors =
	let rec join_exps = function
		| x::[] -> x
		| x::xs -> BinOp(binop, x, join_exps xs, Cil.intType)
		| [] -> failwith "AND/OR must take at least 1 argument"
	in
	Expression.rval state (join_exps exps) errors


(** Convenience function to assign a value to an optional return lvalue.
		@param state is the symbolic executor state in which to evaluate the return lvalue
		@param retopt is the optional return lvalue
		@param bytes is the value to assign to the return lvalue
		@return the updated state
*)
let set_return_value state retopt bytes errors =
	(* TODO: also cast bytes to the expected return type of the function, since, according to
	   cil/doc/api/Cil.html#TYPEinstr, the return lvalue may not match the return type of the
	   function *)
	match retopt with
		| None ->
			(state, errors)
		| Some cil_lval ->
			let state, lval, errors = Expression.lval state cil_lval errors in
			(MemOp.state__assign state lval bytes, errors)


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
		if job.inTrackedFn && !Executeargs.arg_line_coverage then
			let loc = Job.get_loc job in
			{ exHist with coveredLines = LineSet.add (loc.Cil.file, loc.Cil.line) exHist.coveredLines; }
		else
			exHist
	in
	let exHist =
		(* Update edge coverage. *)
		if job.inTrackedFn && !Executeargs.arg_edge_coverage then
			let fn = (List.hd job.state.callstack).svar.vname in
			let edge = (
				{ siFuncName = fn; siStmt = Coverage.stmtAtEndOfBlock job.stmt; },
				{ siFuncName = fn; siStmt = Coverage.stmtAtEndOfBlock stmt; }
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
    fun job retopt exps errors ->
        let state, errors = fn job.state retopt exps errors in
        let job = { job with state = state } in
        let job = end_function_call job in
        (Active job, errors)


(** Function Implimentations **)

let libc___builtin_va_arg job = wrap_state_function begin fun state retopt exps errors ->
	let state, key, errors = Expression.rval state (List.hd exps) errors in
	let state, ret = MemOp.vargs_table__get state key in
	let lastarg = List.nth exps 2 in
	match lastarg with
		| CastE(_, AddrOf(cil_lval)) ->
			let state, lval, channe = Expression.lval state cil_lval errors in
			let state = MemOp.state__assign state lval ret in
			set_return_value state retopt ret errors
		| _ -> failwith "Last argument of __builtin_va_arg must be of the form CastE(_,AddrOf(lval))"
end job


let libc___builtin_va_copy job = wrap_state_function begin fun state retopt exps errors ->
	let state, keyOfSource, errors = Expression.rval state (List.nth exps 1) errors in
	let srcList = MemOp.vargs_table__get_list state keyOfSource in
	let state, key = MemOp.vargs_table__add state srcList in
	match List.hd exps with
		| Lval(cil_lval) ->
			let state, lval, errors = Expression.lval state cil_lval errors in
			let state = MemOp.state__assign state lval key in
			set_return_value state retopt bytes__zero errors
		| _ -> failwith "First argument of va_copy must have lval"
end job


let libc___builtin_va_end job = wrap_state_function begin fun state retopt exps errors ->
	let state, key, errors = Expression.rval state (List.hd exps) errors in
	let state = MemOp.vargs_table__remove state key in
	set_return_value state retopt bytes__zero errors
end job


let libc___builtin_va_start job = wrap_state_function begin fun state retopt exps errors ->
	(* TODO: assign first arg with new bytes that maps to vargs *)
	match List.hd exps with
		| Lval(cil_lval) ->
			let state, key = MemOp.vargs_table__add state (List.hd state.va_arg) in
			let state, lval, errors = Expression.lval state cil_lval errors in
			let state = MemOp.state__assign state lval key in
			set_return_value state retopt bytes__zero errors
		| _ -> failwith "First argument of va_start must have lval"
end job


let libc_free job = wrap_state_function begin fun state retopt exps errors ->
	(* Remove the mapping of (block,bytes) in the state. *)
	(* From opengroup: The free() function causes the space pointed to by
	ptr to be deallocated; that is, made available for further allocation. If ptr
	is a null pointer, no action occurs. Otherwise, if the argument does not match
	a pointer earlier returned by the calloc(), malloc(), realloc() or valloc()
	function, or if the space is deallocated by a call to free() or realloc(), the
	behaviour is undefined.  Any use of a pointer that refers to freed space causes
	undefined behaviour.  *)
	let warning format = Output.kprintf (fun _ -> set_return_value state retopt bytes__zero errors) format in
	let state, ptr, errors = Expression.rval state (List.hd exps) errors in
	match ptr with
		| Bytes_Address (block, _) ->
			if block.memory_block_type != Block_type_Heap
			then warning "Freeing a non-malloced pointer:@ @[%a@]@ = @[%a@]@\n" Printer.exp (List.hd exps) BytesPrinter.bytes ptr else
			if not (MemOp.state__has_block state block)
			then warning "Double-free:@ @[%a@]@ = @[%a@]@\n" Printer.exp (List.hd exps) BytesPrinter.bytes ptr else
			let state = MemOp.state__remove_block state block in
			set_return_value state retopt bytes__zero errors
		| _ ->
			Output.set_mode Output.MSG_MUSTPRINT;
			warning "Freeing something that is not a valid pointer:@ @[%a@]@ = @[%a@]@\n" Printer.exp (List.hd exps) BytesPrinter.bytes ptr
end job


let libc_memset job = wrap_state_function begin fun state retopt exps errors ->
	let state, bytes, errors = Expression.rval state (List.hd exps) errors in
	let block, offset = bytes_to_address bytes in
	let state, old_whole_bytes = MemOp.state__get_bytes_from_block state block in
	let state, char_bytes, errors = Expression.rval state (List.nth exps 1) errors in
	let c = bytes__get_byte char_bytes 0 (* little endian *) in
	let state, n_bytes, errors = Expression.rval state (List.nth exps 2) errors in
	if isConcrete_bytes n_bytes then
		let n = bytes_to_int_auto n_bytes in
		let newbytes = bytes__make_default n c in
		let finalbytes = bytes__write old_whole_bytes offset n newbytes in
		let state = MemOp.state__add_block state block finalbytes in
		set_return_value state retopt bytes errors
	else
		failwith "libc_memset: n is symbolic (TODO)"
end job


(* __builtin_alloca is used for local arrays with variable size; creates a dummy local variable so that the memory is deallocated on return *)
let libc___builtin_alloca__id = Counter.make ()
let libc___builtin_alloca_size state size bytes loc =
	let name = FormatPlus.sprintf "%s(%d)#%d/%a%s"
		(List.hd state.callstack).svar.vname
		size
		(Counter.next libc___builtin_alloca__id)
		Printcil.loc loc
		(MemOp.state__trace state)
	in
	let block = block__make name size Block_type_Local in
	let addrof_block = make_Bytes_Address (block, bytes__zero) in
	let state = MemOp.state__add_block state block bytes in
	let local = (List.hd state.locals) in
	let local = VarinfoMap.add (Cil.makeVarinfo false "alloca" Cil.voidType) (Deferred.Immediate (conditional__lval_block (block, bytes__zero))) local in
	let state = {state with locals = local::(List.tl state.locals); } in
	(state, addrof_block)

let libc___builtin_alloca job retopt exps errors =
	let state, b_size, errors = Expression.rval job.state (List.hd exps) errors in
	let size =
		if isConcrete_bytes b_size then
			bytes_to_int_auto b_size (*safe to use bytes_to_int as arg should be small *)
		else
			1 (* currently bytearray have unbounded length *)
	in
	let bytes =
	  if !Executeargs.arg_init_malloc_zero
	  then bytes__make size (* initially zero, as though malloc were calloc *)
	  else bytes__make_default size byte__undef (* initially the symbolic 'undef' byte *)
	in
	let state, bytes = libc___builtin_alloca_size state size bytes (Job.get_loc job) in
	let state, errors = set_return_value state retopt bytes errors in
	let job = end_function_call { job with state = state} in
	(Active job, errors)

(* allocates on the heap *)
let libc_malloc_size state size bytes loc =
	let name = FormatPlus.sprintf "%s(%d)#%d/%a%s"
		(List.hd state.callstack).svar.vname
		size
		(Counter.next libc___builtin_alloca__id)
		Printcil.loc loc
		(MemOp.state__trace state)
	in
	let block = block__make name size Block_type_Heap in
	let addrof_block = make_Bytes_Address (block, bytes__zero) in
	let state = MemOp.state__add_block state block bytes in
	(state, addrof_block)

let libc_malloc job retopt exps errors =
	let state, b_size, errors = Expression.rval job.state (List.hd exps) errors in
	let size =
		if isConcrete_bytes b_size then
			bytes_to_int_auto b_size (*safe to use bytes_to_int as arg should be small *)
		else
			1 (* currently bytearray have unbounded length *)
	in
	let bytes =
	  if !Executeargs.arg_init_malloc_zero
	  then bytes__make size (* initially zero, as though malloc were calloc *)
	  else bytes__make_default size byte__undef (* initially the symbolic 'undef' byte *)
	in
	let state, bytes = libc_malloc_size state size bytes (Job.get_loc job) in
	let state, errors = set_return_value state retopt bytes errors in
	let job = end_function_call { job with state = state } in
	(Active job, errors)


let otter_given job = wrap_state_function begin fun state retopt exps errors ->
	if List.length exps <> 2 then
		failwith "__GIVEN takes 2 arguments"
	else
		let truthvalue =
			let state, given, errors = Expression.rval state (List.nth exps 0) errors in
			let state, rv, errors = Expression.rval state (List.nth exps 1 ) errors in
			let state, truth = MemOp.eval_with_cache state (given::state.path_condition) rv in
			match truth with
				| Ternary.True -> int_to_bytes 1
				| Ternary.False -> int_to_bytes 0
				| Ternary.Unknown ->bytes__symbolic (bitsSizeOf intType / 8)
		in
		set_return_value state retopt truthvalue errors
end job


let otter_truth_value job = wrap_state_function begin fun state retopt exps errors ->
	let truthvalue =
		int_to_bytes
		begin
			if List.length exps = 0 then 0
			else
				let state, rv, errors = Expression.rval state (List.hd exps) errors in
				let state, truth = MemOp.eval_with_cache state state.path_condition rv in
				match truth with
					| Ternary.True -> 1
					| Ternary.False -> -1
					| Ternary.Unknown -> 0
		end
	in
	set_return_value state retopt truthvalue errors
end job


let libc_exit job retopt exps errors =
	Output.set_mode Output.MSG_MUSTPRINT;
	let exit_code, errors =
		match exps with
			| exp1::_ ->
				let _, bytes, errors = Expression.rval job.state exp1 errors in
				Output.printf "exit() called with code@ @[%a@]@\n" BytesPrinter.bytes bytes;
				let _, bytes, errors = Expression.rval job.state exp1 errors in
				(Some (bytes), errors)
			| [] ->
				Output.printf "exit() called with code (NONE)@\n";
				(None, errors)
	in
	let result = {
		result_file = job.file;
		result_state = job.state;
		result_history = job.exHist;
		result_decision_path = job.decisionPath;
	} in
	(Complete (Exit (exit_code, result)), errors)

let otter_evaluate job = wrap_state_function begin fun state retopt exps errors ->
	let state, bytes, errors = eval_join_exps state exps Cil.LAnd errors in
	Output.set_mode Output.MSG_MUSTPRINT;
	Output.printf "Evaluates to@ @[%a@]@\n" BytesPrinter.bytes bytes;
	(state, errors)
end job


let otter_evaluate_string job = wrap_state_function begin fun state retopt exps errors ->
	let exp = List.hd exps in
	let sizeexp = List.nth exps 1 in
	let state, addr_bytes, errors = Expression.rval state exp errors in
	Output.set_mode Output.MSG_MUSTPRINT;
	match addr_bytes with
		| Bytes_Address(block, offset) ->
			let state, size_bytes, errors = Expression.rval state sizeexp errors in
			let size =
				try
					bytes_to_int_auto size_bytes
				with Failure s ->
					Output.printf "%s@\n" s; 32
			in
			let state, bytes = MemOp.state__deref state (conditional__lval_block (block, offset), size) in
			begin match bytes with
				| Bytes_ByteArray bytearray ->
					Output.printf "Evaluates to string:@ \"@[%a@]\"@\n" BytesPrinter.bytestring bytearray
				| Bytes_Constant (CInt64 (i, _, _)) ->
					Output.printf "Evaluates to non-string constant: %Ld@\n" i
				| _ ->
					Output.printf "Evaluates to a complicated string.@\n"
			end;
			(state, errors)
		| _ ->
			Output.printf "Evaluates to an invalid string.@\n";
			(state, errors)
end job


let otter_symbolic_state job = wrap_state_function begin fun state retopt exps errors ->
	let state = MemoryBlockMap.fold
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
	in
	(state, errors)
end job


let otter_assume job = wrap_state_function begin fun state retopt exps errors ->
	let state, pc, errors = eval_join_exps state exps Cil.LAnd errors in
	(MemOp.state__add_path_condition state pc false, errors)
end job


let otter_path_condition job = wrap_state_function begin fun state retopt exps errors ->
	Output.set_mode Output.MSG_MUSTPRINT;
	if state.path_condition = [] then
		Output.printf "(nil)@\n"
	else
		Output.printf "@[%a@]@\n" (FormatPlus.pp_print_list BytesPrinter.bytes "@\n  AND@\n") state.path_condition;
	(state, errors)
end job


let otter_assert job = wrap_state_function begin fun state retopt exps errors ->
	let state, assertion, errors = eval_join_exps state exps Cil.LAnd errors in
	(Expression.check state assertion exps, errors)
end job


let otter_if_then_else job = wrap_state_function begin fun state retopt exps errors ->
	let state, bytes0, errors = Expression.rval state (List.nth exps 0) errors in
	let state, bytes1, errors = Expression.rval state (List.nth exps 1) errors in
	let state, bytes2, errors = Expression.rval state (List.nth exps 2) errors in
	let c = IfThenElse (
		guard__bytes bytes0, conditional__bytes bytes1, conditional__bytes bytes2
	) in
	let rv = make_Bytes_Conditional c in
	set_return_value state retopt rv errors
end job


let otter_boolean_op binop job = wrap_state_function begin fun state retopt exps errors ->
	let state, rv, errors = eval_join_exps state exps binop errors in
	set_return_value state retopt rv errors
end job


let otter_boolean_not job = wrap_state_function begin fun state retopt exps errors ->
	let state, rv, errors = Expression.rval state (UnOp(Cil.LNot, List.hd exps, Cil.voidType)) errors in
	set_return_value state retopt rv errors
end job


let otter_comment job = wrap_state_function begin fun state retopt exps errors ->
	let exp = List.hd exps in
	Output.set_mode Output.MSG_MUSTPRINT;
	Output.printf "COMMENT:@ @[%a@]@\n" Printer.exp exp;
	(state, errors)
end job


let otter_break_pt job = wrap_state_function begin fun state retopt exps errors ->
	Output.set_mode Output.MSG_REG;
	Output.printf "Option (h for help):@\n";
	Scanf.scanf "%d\n"
		begin fun p1->
			Output.printf "sth\n";
			(state, errors)
		end
end job


let otter_print_state job = wrap_state_function begin fun state retopt exps errors ->
	Output.set_mode Output.MSG_MUSTPRINT;
	let module BOSMap = Map.Make (struct
		type t = memory_block * bytes * int  (* block, offset, size *)
		let compare = Pervasives.compare
	end) in
	let bosmap = ref BOSMap.empty in
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
				let rec p ff b =
					match b with
						| Bytes_Constant const ->  p ff (constant_to_bytes const)
						| Bytes_ByteArray ba -> BytesPrinter.bytes ff (Bytes_ByteArray(ImmutableArray.sub ba off size))
						| Bytes_Address (block, boff) ->
							if off = 0 && size = (bitsSizeOf voidPtrType / 8) then begin
								bosmap := BOSMap.add (block,boff,size) None (!bosmap);
								BytesPrinter.bytes ff b
							end else
								FormatPlus.failwith "PRINT STATE: Reading part of a Bytes_Address: %a %d %d" BytesPrinter.bytes b off size
						| _ -> Format.fprintf ff "(@[%a@],@ %d,@ %d@,)" BytesPrinter.bytes b off size
				in
				Output.printf "%s@ = @[%a@]@\n" varname p bytes
	in
	let printVarBytes var bytes =
		printVarFieldsBytes var.vname var.vtype bytes 0
	in
	let rec is_or_points_to_const = function
		| TVoid a
		| TInt(_, a)
		| TFloat(_, a)
		| TNamed (_, a)
		| TEnum(_,a)
		| TFun(_,_,_,a)
		| TComp (_, a)
		| TBuiltin_va_list a -> Cil.hasAttribute "const" a
		| TArray(t,_,a)
		| TPtr(t, a) -> is_or_points_to_const t
	in
	let printVar var lval_block =
		if is_or_points_to_const var.vtype then ()
		else
			match lval_block with
				| Deferred.Immediate (Unconditional (block, _)) ->
					begin match (MemoryBlockMap.find block state.block_to_bytes) with
						| Deferred.Immediate bytes -> printVarBytes var bytes
						| Deferred.Deferred _ -> Output.printf "%s@ = (Deferred)@\n" var.vname
					end
				(* TODO: print something useful *)
				| Deferred.Immediate (IfThenElse _) ->
					Output.printf "%s@ = (IfThenElse)@\n" var.vname
				| Deferred.Deferred _ ->
					Output.printf "%s@ = (Deferred)@\n" var.vname
	in

	Output.printf "#BEGIN PRINTSTATE@\n";
	Output.printf "#Globals:@\n";
	VarinfoMap.iter printVar state.global;
	Output.printf "#Locals:@\n";
	VarinfoMap.iter printVar (List.hd state.locals);
	Output.printf "#Formals:@\n";
	VarinfoMap.iter printVar (List.hd state.formals);
	(* explore only one level of memory *)
	bosmap := BOSMap.mapi begin fun (block,off,size) des -> match des with
		| Some _ -> des
		| None -> Some (snd(MemOp.state__deref state (conditional__lval_block (block, off), size)))
	end (!bosmap);
	Output.printf "#Memory: (one level)@\n";
	BOSMap.iter (fun (block,off,size) des ->
		match des with
			| None -> Output.printf "@[%a@]@ -> None@\n" BytesPrinter.bytes (Bytes_Address(block, off))
			| Some b -> Output.printf "@[%a@]@ -> @[%a@]@\n" BytesPrinter.bytes (Bytes_Address(block, off)) BytesPrinter.bytes b
	)
	(!bosmap);
	Output.printf "#END PRINTSTATE@\n";
	(state, errors)
end job


let otter_current_state job = wrap_state_function begin fun state retopt exps errors ->
	let state, bytes, errors = Expression.rval state (List.hd exps) errors in
	let key = bytes_to_int_auto bytes in
	Output.set_mode Output.MSG_MUSTPRINT;
	Output.printf "Record state %d\n" key;
	MemOp.index_to_state__add key state;
	(state, errors)
end job


let otter_compare_state job = wrap_state_function begin fun state retopt exps errors ->
	let state, bytes0, errors = Expression.rval state (List.nth exps 0) errors in
	let state, bytes1, errors = Expression.rval state (List.nth exps 1) errors in
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
		(state, errors)
	with Not_found ->
	   	Output.set_mode Output.MSG_MUSTPRINT;
	   	Output.printf "Compare states fail\n";
		(state, errors)
	end
end job


let otter_assert_equal_state job = wrap_state_function begin fun state retopt exps errors ->
	let state, bytes0, errors = Expression.rval state (List.nth exps 0) errors in
	let key0 = bytes_to_int_auto bytes0 in
	begin try
		let s0 = MemOp.index_to_state__get key0 in
		Output.set_mode Output.MSG_MUSTPRINT;
		if MemOp.cmp_states s0 state then
			Output.printf "AssertEqualState satisfied@\n";
		MemOp.index_to_state__add key0 state;
		(state, errors)
	with Not_found ->
		MemOp.index_to_state__add key0 state;
		(state, errors)
	end
end job


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
	match job.instrList with
		| Cil.Call(retopt, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), exps, loc)::_ when varinfo.vname = "__SYMBOLIC" ->
			let errors = [] in
			let job, errors = match exps with
				| [AddrOf (Var varinf, NoOffset as cil_lval)]
				| [CastE (_, AddrOf (Var varinf, NoOffset as cil_lval))] ->
					let state = job.state in
					let exHist = job.exHist in

					let state, (_, size as lval), errors = Expression.lval state cil_lval errors in
					let symbBytes = bytes__symbolic size in
					Output.set_mode Output.MSG_MUSTPRINT;
					Output.printf "%s@ = @[%a@]@\n" varinf.vname BytesPrinter.bytes symbBytes;

					let exHist = { exHist with bytesToVars = (symbBytes,varinf)::exHist.bytesToVars } in
					let state = MemOp.state__assign state lval symbBytes in

					(end_function_call { job with state = state; exHist = exHist }, errors)

				| _ ->

					let state = job.state in
					begin match retopt with
						| None -> failwith "Incorrect usage of __SYMBOLIC(): symbolic value generated and ignored"
						| Some lval ->
							let state, (_, size as lval), errors = Expression.lval state lval errors in
							let state, size = match exps with
								| [] -> (state, size)
								| [CastE (_, h)] | [h] ->
									let state, bytes, errors = Expression.rval state h errors in
									let newsize = bytes_to_int_auto bytes in
									(state, if newsize <= 0 then size else newsize)
								| _ -> failwith "__SYMBOLIC takes at most one argument"
							in
							let state = MemOp.state__assign state lval (bytes__symbolic size) in
							(end_function_call { job with state = state }, errors)
					end
			in
			if errors = [] then
				(Active job, job_queue)
			else
				let abandoned_job_states = Statement.errors_to_abandoned_list job errors in
				(Fork ((Active job)::abandoned_job_states), job_queue)

		| _ ->
			let job_state, job_queue = interceptor job job_queue in
			(job_state, job_queue)


let libc_setjmp job retopt exps errors =
	match exps with
		| [Lval cil_lval]
		| [CastE (_, Lval cil_lval)] ->
			let state = job.state in

			(* assign value to the environment argument *)
			let state, (_, size as lval), errors = Expression.lval state cil_lval errors in
			let stmtPtrAddr = (Counter.next libc___builtin_alloca__id) in
			let state = MemOp.state__assign state lval (int_to_bytes stmtPtrAddr) in

			(* create statemet pointer to the set_jmp call *)
			let nextStmt =
				(* [stmt] is an [Instr] which doesn't end with a call to a
					 [noreturn] function, so it has exactly one successor. *)
				match job.stmt.succs with
					| [h] -> h
					| _ -> assert false
			in
			let stmtPtr = Source (retopt, job.stmt, (List.hd job.instrList), nextStmt) in
			let state = {state with stmtPtrs = Types.IndexMap.add stmtPtrAddr stmtPtr state.stmtPtrs; } in

			let state, errors = set_return_value state retopt bytes__zero errors in
			let job = end_function_call { job with state = state } in
			(Active job, errors)
		| _ -> failwith "setjmp invalid arguments"

let libc_longjmp job retopt exps errors =
	match exps with
		| [Lval cil_lval; value]
		| [CastE (_, Lval cil_lval); value] ->
		begin
			let state = job.state in

			(* get the return value to send to setjmp *)
			let state, bytes, errors =
				match value with
					| CastE (_, v) | v -> Expression.rval state v errors
			in

			(* get the statment pointer *)
			let state, lval, errors =
				try
					Expression.lval state cil_lval errors
				with
					| _ -> failwith "longjmp with invalid statement pointer"
			in
			let state, stmtPtrAddrBytes = MemOp.state__deref state lval in
			let fold_func acc pre leaf =
				(bytes_to_int_auto leaf)::acc
			in
			let stmtPtrAddrs =
				match stmtPtrAddrBytes with
					| Bytes_Constant _
					| Bytes_ByteArray _ -> [bytes_to_int_auto stmtPtrAddrBytes]
					| Bytes_Read(bytes2, offset, len) ->
						let sp = BytesUtility.expand_read_to_conditional bytes2 offset len in
						conditional__fold ~test:(Stp.query_stp state.path_condition) fold_func [] sp
					| Bytes_Conditional(c) ->
						conditional__fold ~test:(Stp.query_stp state.path_condition) fold_func [] c
					| _ -> failwith "Non-constant statement ptr not supported"
			in

			let process_stmtPtr stmtPtrAddr errors =
				let stmtPtr = Types.IndexMap.find stmtPtrAddr state.stmtPtrs in
				let retopt, stmt =
					match stmtPtr with
						| Source (r, _, _, s) -> r, s
						| Runtime
						| NoReturn _ -> failwith "Impossible"
				in

				(* find correct stack frame to jump to *)
				let rec unwind_stack state errors =
					match retopt with
						| None -> (* try to use sentinal value to find stack fram by detecting when it is missing *)
							begin
								try
									let state, _ = MemOp.state__deref state lval in
									unwind_stack (MemOp.state__end_fcall state) errors
								with _ ->
									(state, errors)
							end
						| Some _ -> (* try to use return lval to find stack frame by detecting when it appears *)
							begin
								try
									set_return_value state retopt bytes__zero errors
								with _ ->
									unwind_stack (MemOp.state__end_fcall state) errors
							end
				in
				let state, errors = unwind_stack state errors in

				(* Update coverage. *)
				let exHist = job.exHist in
				let exHist =
					(* We didn't add the outgoing edge in exec_stmt because the
						 call might have never returned. Since there isn't an
						 explicit return (because we handle the call internally), we
						 have to add the edge now. *)
					if job.inTrackedFn && !Executeargs.arg_line_coverage then
						let loc = Job.get_loc job in
						{ exHist with coveredLines = LineSet.add (loc.Cil.file, loc.Cil.line) exHist.coveredLines; }
					else
						exHist
				in
				let exHist =
					(* Update edge coverage. *)
					if job.inTrackedFn && !Executeargs.arg_edge_coverage then
						let fn = (List.hd job.state.callstack).svar.vname in
						let edge = (
							{ siFuncName = fn; siStmt = Coverage.stmtAtEndOfBlock job.stmt; },
							{ siFuncName = fn; siStmt = Coverage.stmtAtEndOfBlock stmt; }
						) in
						{ exHist with coveredEdges = EdgeSet.add edge exHist.coveredEdges; }
					else
						exHist
				in

                (* TODO (martin): update job.decisionPath *)
				(* Update the state, program counter, and coverage  *)
				let state, errors = set_return_value state retopt bytes errors in
				let job = { job with
					stmt = stmt;
					exHist = exHist;
					instrList = [];
					state = state; }
				in
				(Active job, errors)
			in

			let jobs, errors = List.fold_left (fun (jobs, errors) arg ->
				let job, errors = process_stmtPtr arg errors in
				(job::jobs, errors)
			) ([], errors) stmtPtrAddrs in
			(* Reverse the job list to maintain original order *)
			let jobs = List.rev jobs in
			match jobs with
				| _::_::_ -> (Fork jobs, errors)
				| [a] -> (a, errors)
				| [] -> failwith "No valid jongjmp target found!"
		end

		| _ -> failwith "longjmp invalid arguments"


let libc_get_block_size job = wrap_state_function begin fun state retopt exps errors ->
	match exps with
		| [Lval cil_lval]
		| [CastE (_, Lval cil_lval)]
		| [AddrOf (_, NoOffset as cil_lval)]
		| [CastE (_, AddrOf (_, NoOffset as cil_lval))]->
			let state, bytes, errors = Expression.rval state (Lval cil_lval) errors in
			let state, lvals, errors = Expression.deref state bytes errors in
			let size = make_Bytes_Conditional (conditional__map
				~test:(Stp.query_stp state.path_condition)
				(fun (x, y) -> Unconditional (int_to_bytes x.memory_block_size))
				lvals)
			in
			set_return_value state retopt size errors
		| _ -> failwith "libc_get_block_size invalid arguments"
end job

let otter_mute job = wrap_state_function begin fun state retopt exps errors ->
	Output.arg_print_mute := !Output.arg_print_mute + 1;
	(state, errors)
end job

let otter_voice job = wrap_state_function begin fun state retopt exps errors ->
	Output.arg_print_mute := !Output.arg_print_mute - 1;
	(state, errors)
end job

let interceptor job job_queue interceptor =
	try
		(
		(* intercept builtin functions *)
		(                                  (*"__SYMBOLIC"*)            intercept_symbolic) @@
		(intercept_function_by_name_internal "__builtin_alloca"        libc___builtin_alloca) @@
		(intercept_function_by_name_internal "alloca"                  libc___builtin_alloca) @@
		(intercept_function_by_name_internal "malloc"                  libc_malloc) @@
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
		(intercept_function_by_name_internal "_exit"                   libc_exit) @@
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
		(intercept_function_by_name_internal "__otter_mute"            otter_mute) @@
		(intercept_function_by_name_internal "__otter_voice"           otter_voice) @@

		(* pass on the job when none of those match *)
		interceptor

		) job job_queue
	with Failure msg ->
		if !Executeargs.arg_failfast then failwith msg;
		let result = {
			result_file = job.file;
			result_state = job.state;
			result_history = job.exHist;
			result_decision_path = job.decisionPath; }
		in
		(Complete (Abandoned (`Failure msg, Job.get_loc job, result)), job_queue) (* TODO (see above) *)

let libc_interceptor job job_queue interceptor =
	try
		(
		(* assert.h *)
		(intercept_function_by_name_external "__libc_failwith"         "__otter_libc_failwith") @@

		(* ctype.h *)
		(intercept_function_by_name_external "isalnum"                 "__otter_libc_isalnum") @@
		(intercept_function_by_name_external "isalpha"                 "__otter_libc_isalpha") @@
		(intercept_function_by_name_external "iscntrl"                 "__otter_libc_iscntrl") @@
		(intercept_function_by_name_external "isdigit"                 "__otter_libc_isdigit") @@
		(intercept_function_by_name_external "isgraph"                 "__otter_libc_isgraph") @@
		(intercept_function_by_name_external "islower"                 "__otter_libc_islower") @@
		(intercept_function_by_name_external "isprint"                 "__otter_libc_isprint") @@
		(intercept_function_by_name_external "ispunct"                 "__otter_libc_ispunct") @@
		(intercept_function_by_name_external "isspace"                 "__otter_libc_isspace") @@
		(intercept_function_by_name_external "isupper"                 "__otter_libc_isupper") @@
		(intercept_function_by_name_external "isxdigit"                "__otter_libc_isxdigit") @@
		(intercept_function_by_name_external "tolower"                 "__otter_libc_tolower") @@
		(intercept_function_by_name_external "toupper"                 "__otter_libc_toupper") @@

		(* setjmp.h *)
		(intercept_function_by_name_internal "__libc_setjmp"           libc_setjmp) @@
		(intercept_function_by_name_internal "__libc_longjmp"          libc_longjmp) @@

		(* signal.h *)
		(intercept_function_by_name_external "sigaltstack"             "__otter_libc_sigaltstack") @@
		(intercept_function_by_name_external "sigfillset"              "__otter_libc_sigfillset") @@
		(intercept_function_by_name_external "sigaction"               "__otter_libc_sigaction") @@
		(intercept_function_by_name_external "sigemptyset"             "__otter_libc_sigemptyset") @@
		(intercept_function_by_name_external "sigaddset"               "__otter_libc_sigaddset") @@
		(intercept_function_by_name_external "sigprocmask"             "__otter_libc_sigprocmask") @@

		(* stdlib.h *)
		(intercept_function_by_name_external "atoi"                    "__otter_libc_atoi") @@
		(intercept_function_by_name_external "atol"                    "__otter_libc_atol") @@
		(intercept_function_by_name_external "atoll"                   "__otter_libc_atoll") @@
		(intercept_function_by_name_external "strtol"                  "__otter_libc_strtol") @@
		(intercept_function_by_name_external "strtoll"                 "__otter_libc_strtoll") @@
		(intercept_function_by_name_external "strtoul"                 "__otter_libc_strtoul") @@
		(intercept_function_by_name_external "strtoull"                "__otter_libc_strtoull") @@
		(intercept_function_by_name_external "rand"                    "__otter_libc_rand") @@
		(intercept_function_by_name_external "srand"                   "__otter_libc_srand") @@
		(intercept_function_by_name_internal "malloc"                  libc_malloc) @@
		(intercept_function_by_name_internal "__otter_multi_gmalloc"   libc_malloc) @@
		(intercept_function_by_name_external "calloc"                  "__otter_libc_calloc") @@
		(intercept_function_by_name_external "realloc"                 "__otter_libc_realloc") @@
		(intercept_function_by_name_internal "free"                    libc_free) @@
		(intercept_function_by_name_internal "__libc_get_block_size"   libc_get_block_size) @@
		(intercept_function_by_name_external "abort"                   "__otter_libc_abort") @@
		(intercept_function_by_name_external "atexit"                  "__otter_libc_atexit") @@
		(intercept_function_by_name_external "exit"                    "__otter_libc_exit") @@
		(intercept_function_by_name_external "_Exit"                   "__otter_libc__Exit") @@
		(intercept_function_by_name_external "getenv"                  "__otter_libc_getenv") @@
		(intercept_function_by_name_external "system"                  "__otter_libc_system") @@
		(intercept_function_by_name_external "bsearch"                 "__otter_libc_bsearch") @@
		(intercept_function_by_name_external "qsort"                   "__otter_libc_qsort") @@
		(intercept_function_by_name_external "abs"                     "__otter_libc_abs") @@
		(intercept_function_by_name_external "labs"                    "__otter_libc_labs") @@
		(intercept_function_by_name_external "llabs"                   "__otter_libc_llabs") @@
		(intercept_function_by_name_external "div"                     "__otter_libc_div") @@
		(intercept_function_by_name_external "ldiv"                    "__otter_libc_ldiv") @@
		(intercept_function_by_name_external "lldiv"                   "__otter_libc_lldiv") @@
		(intercept_function_by_name_external "mblen"                   "__otter_libc_mblen") @@
		(intercept_function_by_name_external "mbtowc"                  "__otter_libc_mbtowc") @@
		(intercept_function_by_name_external "wctomb"                  "__otter_libc_wctomb") @@

		(* string.h *)
		(intercept_function_by_name_external "memcpy"                  "__otter_libc_memcpy") @@
		(intercept_function_by_name_external "memmove"                 "__otter_libc_memmove") @@
		(intercept_function_by_name_external "strcpy"                  "__otter_libc_strcpy") @@
		(intercept_function_by_name_external "strncpy"                 "__otter_libc_strncpy") @@
		(intercept_function_by_name_external "strcat"                  "__otter_libc_strcat") @@
		(intercept_function_by_name_external "strncat"                 "__otter_libc_strncat") @@
		(intercept_function_by_name_external "memcmp"                  "__otter_libc_memcmp") @@
		(intercept_function_by_name_external "strcmp"                  "__otter_libc_strcmp") @@
		(intercept_function_by_name_external "strcoll"                 "__otter_libc_strcoll") @@
		(intercept_function_by_name_external "strxfrm"                 "__otter_libc_strxfrm") @@
		(intercept_function_by_name_external "memchr"                  "__otter_libc_memchr") @@
		(intercept_function_by_name_external "strchr"                  "__otter_libc_strchr") @@
		(intercept_function_by_name_external "strcspn"                 "__otter_libc_strcspn") @@
		(intercept_function_by_name_external "strpbrk"                 "__otter_libc_strpbrk") @@
		(intercept_function_by_name_external "strrchr"                 "__otter_libc_strrchr") @@
		(intercept_function_by_name_external "strspn"                  "__otter_libc_strspn") @@
		(intercept_function_by_name_external "strstr"                  "__otter_libc_strstr") @@
		(intercept_function_by_name_external "strtok"                  "__otter_libc_strtok") @@
		(intercept_function_by_name_external "memset"                  "__otter_libc_memset") @@
		(intercept_function_by_name_external "strerror"                "__otter_libc_strerror") @@
		(intercept_function_by_name_external "strlen"                  "__otter_libc_strlen") @@
		(intercept_function_by_name_external "strdup"                  "__otter_libc_strdup") @@

		(* unistd.h *)
		(intercept_function_by_name_external "close"                   "__otter_libc_close") @@
		(intercept_function_by_name_external "read"                    "__otter_libc_read") @@
		(intercept_function_by_name_external "pread"                   "__otter_libc_pread") @@
		(intercept_function_by_name_external "write"                   "__otter_libc_write") @@
		(intercept_function_by_name_external "pwrite"                  "__otter_libc_pwrite") @@
		(intercept_function_by_name_external "unlink"                  "__otter_libc_unlink") @@
		(intercept_function_by_name_external "rmdir"                   "__otter_libc_rmdir") @@
		(intercept_function_by_name_external "lseek"                   "__otter_libc_lseek") @@
		(intercept_function_by_name_external "getuid"                  "__otter_libc_getuid") @@
		(intercept_function_by_name_external "setuid"                  "__otter_libc_setuid") @@
		(intercept_function_by_name_external "dup"                     "__otter_libc_dup") @@
		(intercept_function_by_name_external "dup2"                    "__otter_libc_dup2") @@
		(intercept_function_by_name_external "getpagesize"             "__otter_libc_getpagesize") @@
		(intercept_function_by_name_external "sysconf"                 "__otter_libc_sysconf") @@

		(* arpa/inet.h *)
		(intercept_function_by_name_external "inet_addr"               "__otter_libc_inet_addr") @@
		(intercept_function_by_name_external "inet_lnaof"              "__otter_libc_inet_lnaof") @@
		(intercept_function_by_name_external "inet_makeaddr"           "__otter_libc_inet_makeaddr") @@
		(intercept_function_by_name_external "inet_netof"              "__otter_libc_inet_netof") @@
		(intercept_function_by_name_external "inet_network"            "__otter_libc_inet_network") @@
		(intercept_function_by_name_external "inet_ntoa"               "__otter_libc_inet_ntoa") @@
		(intercept_function_by_name_external "inet_aton"               "__otter_libc_inet_aton") @@

		(* sys/fcntl.h *)
		(intercept_function_by_name_external "creat"                   "__otter_libc_creat") @@
		(intercept_function_by_name_external "fcntl"                   "__otter_libc_fcntl") @@
		(intercept_function_by_name_external "open"                    "__otter_libc_open") @@
		
		(* sys/mman.h *)
		(intercept_function_by_name_external "mmap"                    "__otter_libc_mmap") @@
		(intercept_function_by_name_external "munmap"                  "__otter_libc_munmap") @@
		(intercept_function_by_name_external "mprotect"                "__otter_libc_mprotect") @@

		(* sys/socket.h *)
		(intercept_function_by_name_external "socket"                  "__otter_libc_socket") @@
		(intercept_function_by_name_external "getsockopt"              "__otter_libc_getsockopt") @@
		(intercept_function_by_name_external "setsockopt"              "__otter_libc_setsockopt") @@

		(* sys/stat.h *)
		(intercept_function_by_name_external "chmod"                   "__otter_libc_chmod") @@
		(intercept_function_by_name_external "fchmod"                  "__otter_libc_fchmod") @@
		(intercept_function_by_name_external "fstat"                   "__otter_libc_fstat") @@
		(intercept_function_by_name_external "lstat"                   "__otter_libc_lstat") @@
		(intercept_function_by_name_external "mkdir"                   "__otter_libc_mkdir") @@
		(intercept_function_by_name_external "mkfifo"                  "__otter_libc_mkfifo") @@
		(intercept_function_by_name_external "mknod"                   "__otter_libc_mknod") @@
		(intercept_function_by_name_external "stat"                    "__otter_libc_stat") @@
		(intercept_function_by_name_external "umask"                   "__otter_libc_umask") @@

		(* pass on the job when none of those match *)
		interceptor

		) job job_queue
	with Failure msg ->
		if !Executeargs.arg_failfast then failwith msg;
		let result = {
			result_file = job.file;
			result_state = job.state;
			result_history = job.exHist;
			result_decision_path = job.decisionPath; } in
		(Complete (Abandoned (`Failure msg, Job.get_loc job, result)), job_queue)
