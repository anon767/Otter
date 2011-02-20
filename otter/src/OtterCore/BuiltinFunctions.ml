(**

This module contains a library of built-in functions for Otter.

*)

open DataStructures
open OcamlUtilities
open CilUtilities
open Cil
open OtterBytes
open Bytes
open BytesUtility
open State
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

(** Gets the argument for a function that takes only one argument
    @param exps the arguments to the function
    @raises Failure if given a non-singleton list
    @return the lone element of the list *)
let get_lone_arg = function
  | [ x ] -> x
  | _ -> failwith "This function takes exactly one argument"

(** Convenience function to receive a constant int value from an argument
		@param exp the argument 
        @raises Failure if the argument is not a constant int
		@return the constant int
*)
let get_constant_int = function
    | Const(CInt64(i64, _, _)) -> Int64.to_int i64
    | _ -> failwith "Argument is not constant integer"

(** Convenience function to assign a value to an optional return lvalue.
		@param job is the symbolic executor job in which to evaluate the return lvalue
		@param retopt is the optional return lvalue
		@param bytes is the value to assign to the return lvalue
		@return the updated job
*)
let set_return_value job retopt bytes errors =
	(* TODO: also cast bytes to the expected return type of the function, since, according to
	   cil/doc/api/Cil.html#TYPEinstr, the return lvalue may not match the return type of the
	   function *)
	match retopt with
		| None ->
			(job, errors)
		| Some cil_lval ->
			let job, lval, errors = Expression.lval job cil_lval errors in
			(MemOp.state__assign job lval bytes, errors)


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
		match job#stmt.succs with
			| [h] -> h
			| _ -> assert false
	in

	(* Update coverage. *)
	let exHist = job#exHist in
	let exHist =
		(* We didn't add the outgoing edge in exec_stmt because the
			 call might have never returned. Since there isn't an
			 explicit return (because we handle the call internally), we
			 have to add the edge now. *)
		if job#inTrackedFn && !Executeargs.arg_line_coverage then
			let loc = Job.get_loc job in
			{ exHist with Job.coveredLines = Job.LineSet.add (loc.Cil.file, loc.Cil.line) exHist.Job.coveredLines; }
		else
			exHist
	in
	let exHist =
		(* Update edge coverage. *)
		if job#inTrackedFn && !Executeargs.arg_edge_coverage then
			let fn = (List.hd job#state.callstack).svar.vname in
			let edge = (
				{ Job.siFuncName = fn; Job.siStmt = Coverage.stmtAtEndOfBlock job#stmt; },
				{ Job.siFuncName = fn; Job.siStmt = Coverage.stmtAtEndOfBlock stmt; }
			) in
			{ exHist with Job.coveredEdges = Job.EdgeSet.add edge exHist.Job.coveredEdges; }
		else
			exHist
	in

	(* Update the state, program counter, and coverage  *)
	((job#with_stmt stmt)#with_exHist exHist)#with_instrList []


(** Function Implimentations **)

let libc___builtin_va_arg job retopt exps errors =
	let job, key, errors = Expression.rval job (List.hd exps) errors in
	let job, ret = MemOp.vargs_table__get job key in
	let lastarg = List.nth exps 2 in
	match lastarg with
		| CastE(_, AddrOf(cil_lval)) ->
			let job, lval, channe = Expression.lval job cil_lval errors in
			let job = MemOp.state__assign job lval ret in
			let job, errors = set_return_value job retopt ret errors in
			let job = end_function_call job in
			(Job.Active job, errors)
		| _ -> failwith "Last argument of __builtin_va_arg must be of the form CastE(_,AddrOf(lval))"


let libc___builtin_va_copy job retopt exps errors =
	let job, keyOfSource, errors = Expression.rval job (List.nth exps 1) errors in
	let srcList = MemOp.vargs_table__get_list job keyOfSource in
	let job, key = MemOp.vargs_table__add job srcList in
	match List.hd exps with
		| Lval(cil_lval) ->
			let job, lval, errors = Expression.lval job cil_lval errors in
			let job = MemOp.state__assign job lval key in
			let job = end_function_call job in
			(Job.Active job, errors)
		| _ -> failwith "First argument of va_copy must have lval"


let libc___builtin_va_end job retopt exps errors =
	let job, key, errors = Expression.rval job (List.hd exps) errors in
	let job = MemOp.vargs_table__remove job key in
	let job = end_function_call job in
	(Job.Active job, errors)


let libc___builtin_va_start job retopt exps errors =
	(* TODO: assign first arg with new bytes that maps to vargs *)
	match List.hd exps with
		| Lval(cil_lval) ->
			let job, key = MemOp.vargs_table__add job (List.hd job#state.va_arg) in
			let job, lval, errors = Expression.lval job cil_lval errors in
			let job = MemOp.state__assign job lval key in
			let job = end_function_call job in
			(Job.Active job, errors)
		| _ -> failwith "First argument of va_start must have lval"


let libc_free job retopt exps errors =
    (* Remove the mapping of (block,bytes) in the job. *)
    let arg = List.hd exps in
    let job, ptr, errors = Expression.rval job arg errors in
    let job, errors = match ptr with
      | Bytes_Address (block, offset) ->
            if block.memory_block_type != Block_type_Heap || not (bytes__equal offset bytes__zero)
            then FormatPlus.failwith "Freeing a non-malloced pointer:@ @[%a@]@ = @[%a@]@\n" CilPrinter.exp arg BytesPrinter.bytes ptr;
            if not (MemOp.state__has_block job block)
            then FormatPlus.failwith "Double-free:@ @[%a@]@ = @[%a@]@\n" CilPrinter.exp arg BytesPrinter.bytes ptr;
            let job = MemOp.state__remove_block job block in
            (job, errors)
      | ptr when bytes__equal ptr bytes__zero -> (* Freeing a null pointer. Do nothing. *)
            (job, errors)
      | _ ->
            FormatPlus.failwith "Freeing something that is not a valid pointer:@ @[%a@]@ = @[%a@]@\n" CilPrinter.exp arg BytesPrinter.bytes ptr
    in
    let job = end_function_call job in
    (Job.Active job, errors)

(** [access_with_length job exp errors length] finds the conditional tree of
    lvalues (that is, [(memory_block, offset)] pairs) that exp can point to. It also
    checks that these lvalues can be accessed with the given length. That is, if a
    pointer points to (b, off), this performs a check that off+length is in
    bounds.
    @param job the job in which to evaluate [exp]
    @param exp the expression to evaluate
    @param errors the errors at the point of this call (TODO: This should not be an input to this function)
    @param length the size (in bytes) of the read or write

    @return a 4-tuple of (1) the [job] updated, if necessary, with the
    assumption that the bounds-check passed, (2) the [bytes] that
    [exp] evaluates to, (3) the conditional tree of lvalues that [exp]
    can point to, and (4) the errors updated, if necessary, with the
    failing bounds-check.
*)
let access_with_length job exp errors length =
  let job, addr_bytes, errors = Expression.rval job exp errors in
  let job, lvals, errors = Expression.deref job addr_bytes (Cil.typeOf exp) errors in
  let old_job = job in (* Checkpoint the job in case we must report an error *)
    let job, failing_bytes_opt = Expression.checkBounds job lvals length in
    let errors = match failing_bytes_opt with
      | None -> errors
      | Some b -> (old_job, `OutOfBounds exp) :: errors
    in
    (job, addr_bytes, lvals, errors)

let libc_memmove job retopt exps errors =
    match exps with
      | [ dest_exp; src_exp; length_exp ] ->
            let job, length, errors = Expression.rval job length_exp errors in
            let length = bytes_to_int_auto length in
            let job, dest, dest_lvals, errors = access_with_length job dest_exp errors length in
            let job, _, src_lvals, errors = access_with_length job src_exp errors length in
            let job, src_bytes = MemOp.state__deref job (src_lvals, length) in
            let job = MemOp.state__assign job (dest_lvals, length) src_bytes in
            let job, errors = set_return_value job retopt dest errors in
            let job = end_function_call job in
            (Job.Active job, errors)
      | _ -> failwith "Wrong number of arguments to memcpy"

(* TODO: memcpy should check that the source and destination do not overlap *)
let libc_memcpy = libc_memmove

let libc_memset job retopt exps errors =
    match exps with
      | [ dest_exp; value_exp; length_exp ] ->
            let job, length, errors = Expression.rval job length_exp errors in
            let length = bytes_to_int_auto length in
            let job, value, errors = Expression.rval job value_exp errors in
            let value_byte = bytes__get_byte value 0 (* little endian *) in
            let job, dest, lvals, errors = access_with_length job dest_exp errors length in
            let job = MemOp.state__assign job (lvals, length) (bytes__make_default length value_byte) in
            let job, errors = set_return_value job retopt dest errors in
            let job = end_function_call job in
            (Job.Active job, errors)
      | _ -> failwith "Wrong number of arguments to memset"


(* __builtin_alloca is used for local arrays with variable size; creates a dummy local variable so that the memory is deallocated on return *)
let libc___builtin_alloca__id = Counter.make ()
let libc___builtin_alloca_size job size bytes loc =
	let name = FormatPlus.sprintf "%s(%d)#%d/%a%s"
		(List.hd job#state.callstack).svar.vname
		size
		(Counter.next libc___builtin_alloca__id)
		Printcil.loc loc
		(MemOp.state__trace job)
	in
	let block = block__make name size Block_type_Local in
	let addrof_block = make_Bytes_Address (block, bytes__zero) in
	let job = MemOp.state__add_block job block bytes in
	let local = (List.hd job#state.locals) in
	let local = VarinfoMap.add (Cil.makeVarinfo false "alloca" Cil.voidType) (Deferred.Immediate (conditional__lval_block (block, bytes__zero))) local in
	let job = job#with_state { job#state with locals = local::(List.tl job#state.locals); } in
	(job, addrof_block)

let libc___builtin_alloca job retopt exps errors =
	let job, b_size, errors = Expression.rval job (List.hd exps) errors in
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
	let job, bytes = libc___builtin_alloca_size job size bytes (Job.get_loc job) in
	let job, errors = set_return_value job retopt bytes errors in
	let job = end_function_call job in
	(Job.Active job, errors)

(* allocates on the heap *)
let libc_malloc_size job size bytes loc =
	let name = FormatPlus.sprintf "%s(%d)#%d/%a%s"
		(List.hd job#state.callstack).svar.vname
		size
		(Counter.next libc___builtin_alloca__id)
		Printcil.loc loc
		(MemOp.state__trace job)
	in
	let block = block__make name size Block_type_Heap in
	let addrof_block = make_Bytes_Address (block, bytes__zero) in
	let job = MemOp.state__add_block job block bytes in
	(job, addrof_block)

let libc_malloc job retopt exps errors =
	let job, b_size, errors = Expression.rval job (List.hd exps) errors in
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
	let job, bytes = libc_malloc_size job size bytes (Job.get_loc job) in
	let job, errors = set_return_value job retopt bytes errors in
	let job = end_function_call job in
	(Job.Active job, errors)


let otter_given job retopt exps errors =
	if List.length exps <> 2 then
		failwith "__GIVEN takes 2 arguments"
	else
		let truthvalue =
			let job, given, errors = Expression.rval job (List.nth exps 0) errors in
			let job, rv, errors = Expression.rval job (List.nth exps 1 ) errors in
			let truth = MemOp.eval (given::job#state.path_condition) rv in
			match truth with
				| Ternary.True -> int_to_bytes 1
				| Ternary.False -> int_to_bytes 0
				| Ternary.Unknown ->bytes__symbolic (bitsSizeOf intType / 8)
		in
		let job, errors = set_return_value job retopt truthvalue errors in
		let job = end_function_call job in
		(Job.Active job, errors)


let otter_truth_value job retopt exps errors =
	let truthvalue =
		int_to_bytes
		begin
			if List.length exps = 0 then 0
			else
				let job, rv, errors = Expression.rval job (List.hd exps) errors in
				let truth = MemOp.eval job#state.path_condition rv in
				match truth with
					| Ternary.True -> 1
					| Ternary.False -> -1
					| Ternary.Unknown -> 0
		end
	in
	let job, errors = set_return_value job retopt truthvalue errors in
	let job = end_function_call job in
	(Job.Active job, errors)


let libc_exit job retopt exps errors =
	Output.set_mode Output.MSG_MUSTPRINT;
	let exit_code, errors =
		match exps with
			| exp1::_ ->
				let _, bytes, errors = Expression.rval job exp1 errors in
				Output.printf "exit() called with code@ @[%a@]@\n" BytesPrinter.bytes bytes;
				let _, bytes, errors = Expression.rval job exp1 errors in
				(Some (bytes), errors)
			| [] ->
				Output.printf "exit() called with code (NONE)@\n";
				(None, errors)
	in
	(Job.Complete (Job.Exit (exit_code, job)), errors)


let otter_evaluate job retopt exps errors =
	let job, bytes, errors = eval_join_exps job exps Cil.LAnd errors in
	Output.set_mode Output.MSG_MUSTPRINT;
	Output.printf "Evaluates to@ @[%a@]@\n" BytesPrinter.bytes bytes;
	let job = end_function_call job in
	(Job.Active job, errors)


let otter_evaluate_string job retopt exps errors =
	let exp = List.hd exps in
	let sizeexp = List.nth exps 1 in
	let job, addr_bytes, errors = Expression.rval job exp errors in
	Output.set_mode Output.MSG_MUSTPRINT;
	let job, errors = match addr_bytes with
		| Bytes_Address(block, offset) ->
			let job, size_bytes, errors = Expression.rval job sizeexp errors in
			let size =
				try
					bytes_to_int_auto size_bytes
				with Failure s ->
					Output.printf "%s@\n" s; 32
			in
			let job, bytes = MemOp.state__deref job (conditional__lval_block (block, offset), size) in
			begin match bytes with
				| Bytes_ByteArray bytearray ->
					Output.printf "Evaluates to string:@ \"@[%a@]\"@\n" BytesPrinter.bytestring bytearray
				| Bytes_Constant (CInt64 (i, _, _)) ->
					Output.printf "Evaluates to non-string constant: %Ld@\n" i
				| _ ->
					Output.printf "Evaluates to a complicated string.@\n"
			end;
			(job, errors)
		| _ ->
			Output.printf "Evaluates to an invalid string.@\n";
			(job, errors)
	in
	let job = end_function_call job in
	(Job.Active job, errors)


let otter_symbolic_state job retopt exps errors =
	let job = MemoryBlockMap.fold
		begin fun block _ job ->
			(* TODO: what about deferred bytes? *)
			(* TODO: handle pointers with an alias analysis *)
			let job, bytes = MemOp.state__get_bytes_from_block job block in
				match bytes with
					| Bytes_FunPtr(_) ->
						job
					| _ ->
						MemOp.state__add_block job block (bytes__symbolic (bytes__length bytes))
		end
		job#state.block_to_bytes
		job
	in
	let job = end_function_call job in
	(Job.Active job, errors)


exception Unsat
(** [otter_assume job _ exps errors] assumes that each of exps is true.
		@param job the job in which to assume the list of expressions
		@param exps the list of expressions to assume
		@return an empty [Fork []] if the assumptions are unsatisfiable; the input job (unchanged) if the assumptions are valid; and the updated job otherwise
*)
let otter_assume job _ exps errors =
    try
        let check_exp (job, errors) exp =
            let job, query, errors = Expression.rval job exp errors in
            match Stp.query_stp job#state.path_condition guard__true (guard__bytes query) with
                | Ternary.True -> (job, errors) (* Ignore true assumptions *)
                | Ternary.False -> raise Unsat (* Stop on false assumptions *)
                | Ternary.Unknown -> (MemOp.state__add_path_condition job query false, errors) (* Add possible assumptions to path condition *)
        in
        let job, errors = List.fold_left check_exp (job, errors) exps in
        let job = end_function_call job in
        (Job.Active job, errors)
    with Unsat ->
        (* Use [Fork []] to make this job disappear. Among other things, this
           means that we ignore coverage from this job, and we also ignore any
           errors that occurred while evaluating the arguments to __ASSUME. *)
        Output.printf "Impossible assumption; eliminating job";
        (Job.Fork [], errors)


let otter_path_condition job retopt exps errors =
	Output.set_mode Output.MSG_MUSTPRINT;
	if job#state.path_condition = [] then
		Output.printf "(nil)@\n"
	else
		Output.printf "@[%a@]@\n" (FormatPlus.pp_print_list BytesPrinter.bytes "@\n  AND@\n") job#state.path_condition;
	let job = end_function_call job in
	(Job.Active job, errors)


(* __FAILURE()
 * It models "failure" by providing the 'reason `FailureReached, in order to distinguish from
 * general `Failure.
 * TODO: make __FAILURE take a string of failure description. *)
let otter_failure job retopt exps errors =
    let job_state = Job.Complete (Job.Abandoned (`FailureReached, job)) in
    job_state, errors

let otter_assert job retopt exps errors =
    let exp =
        match exps with
            [ exp ] -> exp
          | _ -> failwith "__ASSERT takes exactly one argument"
    in
    let job, assertion, errors = Expression.rval job exp errors in
    let active j = Job.Active (end_function_call j) in
    let failure () = (* Construct the failing job_state directly; there's no need to use 'errors' here. *)
        let job = MemOp.state__add_path_condition job (logicalNot assertion) true in
        Job.Complete (Job.Abandoned (`AssertionFailure exp, job))
    in
    match MemOp.eval job#state.path_condition assertion with
      | Ternary.True -> (active job, errors) (* assertion passes; do nothing *)
      | Ternary.False -> (failure (), errors) (* assertion definitely fails *)
      | Ternary.Unknown -> (* assertion can fail and can pass. Record the error, but also assume it passes and continue *)
            let job = MemOp.state__add_path_condition job assertion true in
            (Job.Fork [ active job; failure () ], errors)

let otter_if_then_else job retopt exps errors =
	let job, bytes0, errors = Expression.rval job (List.nth exps 0) errors in
	let job, bytes1, errors = Expression.rval job (List.nth exps 1) errors in
	let job, bytes2, errors = Expression.rval job (List.nth exps 2) errors in
	let c = IfThenElse (
		guard__bytes bytes0, conditional__bytes bytes1, conditional__bytes bytes2
	) in
	let rv = make_Bytes_Conditional c in
	let job, errors = set_return_value job retopt rv errors in
	let job = end_function_call job in
	(Job.Active job, errors)


let otter_boolean_op binop job retopt exps errors =
	let job, rv, errors = eval_join_exps job exps binop errors in
	let job, errors = set_return_value job retopt rv errors in
	let job = end_function_call job in
	(Job.Active job, errors)


let otter_boolean_not job retopt exps errors =
	let job, rv, errors = Expression.rval job (UnOp(Cil.LNot, List.hd exps, Cil.voidType)) errors in
	let job, errors = set_return_value job retopt rv errors in
	let job = end_function_call job in
	(Job.Active job, errors)


let otter_break_pt job retopt exps errors =
	Output.set_mode Output.MSG_REG;
	Output.printf "Option (h for help):@\n";
	ignore (Scanf.scanf "%d\n"
		begin fun p1->
			Output.printf "sth\n";
			(job, errors)
		end);
	let job = end_function_call job in
	(Job.Active job, errors)


let otter_print_state job retopt exps errors =
	Output.set_mode Output.MSG_MUSTPRINT;
	let module BOSMap = Map.Make (struct
		type t = memory_block * bytes * int  (* block, offset, size *)
		let compare = Pervasives.compare
	end) in
	let bosmap = ref BOSMap.empty in
	let rec printVarFieldsBytes varname typ bytes off =
		(* break down each local by its fields *)
		(* canonicalize concrete values by their array rep*)
		match unrollType typ with
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
						| Bytes_ByteArray ba -> BytesPrinter.bytes ff (make_Bytes_ByteArray(ImmutableArray.sub ba off size))
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
					begin match (MemoryBlockMap.find block job#state.block_to_bytes) with
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
	VarinfoMap.iter printVar job#state.global;
	Output.printf "#Locals:@\n";
	VarinfoMap.iter printVar (List.hd job#state.locals);
	Output.printf "#Formals:@\n";
	VarinfoMap.iter printVar (List.hd job#state.formals);
	(* explore only one level of memory *)
	bosmap := BOSMap.mapi begin fun (block,off,size) des -> match des with
		| Some _ -> des
		| None -> Some (snd(MemOp.state__deref job (conditional__lval_block (block, off), size)))
	end (!bosmap);
	Output.printf "#Memory: (one level)@\n";
	BOSMap.iter (fun (block,off,size) des ->
		match des with
			| None -> Output.printf "@[%a@]@ -> None@\n" BytesPrinter.bytes (make_Bytes_Address(block, off))
			| Some b -> Output.printf "@[%a@]@ -> @[%a@]@\n" BytesPrinter.bytes (make_Bytes_Address(block, off)) BytesPrinter.bytes b
	)
	(!bosmap);
	Output.printf "#END PRINTSTATE@\n";
	let job = end_function_call job in
	(Job.Active job, errors)


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
	match job#instrList with
		| Cil.Call(retopt, Cil.Lval(Cil.Var({vname = "__SYMBOLIC"}), Cil.NoOffset), exps, loc)::_ ->
			let errors = [] in
			let job, errors = match exps with
				| [AddrOf (Var varinf, NoOffset as cil_lval)]
				| [CastE (_, AddrOf (Var varinf, NoOffset as cil_lval))] ->
					let exHist = job#exHist in

					let job, (_, size as lval), errors = Expression.lval job cil_lval errors in
					let symbBytes = bytes__symbolic size in
					Output.set_mode Output.MSG_MUSTPRINT;
					Output.printf "%s@ = @[%a@]@\n" varinf.vname BytesPrinter.bytes symbBytes;

					let exHist = { exHist with Job.bytesToVars = (symbBytes,varinf)::exHist.Job.bytesToVars } in
					let job = MemOp.state__assign job lval symbBytes in

					(end_function_call (job#with_exHist exHist), errors)

				| _ ->
					begin match retopt with
						| None -> failwith "Incorrect usage of __SYMBOLIC(): symbolic value generated and ignored"
						| Some lval ->
							let job, (_, size as lval), errors = Expression.lval job lval errors in
							let job, size = match exps with
								| [] -> (job, size)
								| [CastE (_, h)] | [h] ->
									let job, bytes, errors = Expression.rval job h errors in
									let newsize = bytes_to_int_auto bytes in
									(job, if newsize <= 0 then size else newsize)
								| _ -> failwith "__SYMBOLIC takes at most one argument"
							in
							let job = MemOp.state__assign job lval (bytes__symbolic size) in
							(end_function_call job, errors)
					end
			in
			if errors = [] then
				(Job.Active job, job_queue)
			else
				let abandoned_job_states = Statement.errors_to_abandoned_list job errors in
				(Job.Fork ((Job.Active job)::abandoned_job_states), job_queue)

		| _ ->
			let job_state, job_queue = interceptor job job_queue in
			(job_state, job_queue)


let libc_setjmp job retopt exps errors =
	match exps with
		| [Lval cil_lval]
		| [CastE (_, Lval cil_lval)] ->
			(* assign value to the environment argument *)
			let job, (_, size as lval), errors = Expression.lval job cil_lval errors in
			let stmtPtrAddr = (Counter.next libc___builtin_alloca__id) in
			let job = MemOp.state__assign job lval (int_to_bytes stmtPtrAddr) in

			(* create statemet pointer to the set_jmp call *)
			let nextStmt =
				(* [stmt] is an [Instr] which doesn't end with a call to a
					 [noreturn] function, so it has exactly one successor. *)
				match job#stmt.succs with
					| [h] -> h
					| _ -> assert false
			in
			let stmtPtr = Source (retopt, job#stmt, (List.hd job#instrList), nextStmt) in
			let job = job#with_state { job#state with stmtPtrs = State.IndexMap.add stmtPtrAddr stmtPtr job#state.stmtPtrs; } in

			let job, errors = set_return_value job retopt bytes__zero errors in
			let job = end_function_call job in
			(Job.Active job, errors)
		| _ -> failwith "setjmp invalid arguments"

let libc_longjmp job retopt exps errors =
	match exps with
		| [Lval cil_lval; value]
		| [CastE (_, Lval cil_lval); value] ->
		begin
			(* get the return value to send to setjmp *)
			let job, bytes, errors =
				match value with
					| CastE (_, v) | v -> Expression.rval job v errors
			in

			(* get the statment pointer *)
			let job, lval, errors =
				try
					Expression.lval job cil_lval errors
				with
					| _ -> failwith "longjmp with invalid statement pointer"
			in
			let job, stmtPtrAddrBytes = MemOp.state__deref job lval in
			let fold_func acc pre leaf =
				(bytes_to_int_auto leaf)::acc
			in
			let stmtPtrAddrs =
				match stmtPtrAddrBytes with
					| Bytes_Constant _
					| Bytes_ByteArray _ -> [bytes_to_int_auto stmtPtrAddrBytes]
					| Bytes_Read(bytes2, offset, len) ->
						let sp = BytesUtility.expand_read_to_conditional bytes2 offset len in
						conditional__fold ~test:(fun acc pre guard -> (acc, Stp.query_stp job#state.path_condition pre guard)) fold_func [] sp
					| Bytes_Conditional(c) ->
						conditional__fold fold_func [] c
					| _ -> failwith "Non-constant statement ptr not supported"
			in

			let process_stmtPtr job stmtPtrAddr errors =
				let stmtPtr = State.IndexMap.find stmtPtrAddr job#state.stmtPtrs in
				let retopt, stmt =
					match stmtPtr with
						| Source (r, _, _, s) -> r, s
						| Runtime
						| NoReturn _ -> failwith "Impossible"
				in

				(* find correct stack frame to jump to *)
				let rec unwind_stack job errors =
					match retopt with
						| None -> (* try to use sentinal value to find stack fram by detecting when it is missing *)
							begin
								try
									let job, _ = MemOp.state__deref job lval in
									unwind_stack (MemOp.state__end_fcall job) errors
								with _ ->
									(job, errors)
							end
						| Some _ -> (* try to use return lval to find stack frame by detecting when it appears *)
							begin
								try
									set_return_value job retopt bytes__zero errors
								with _ ->
									unwind_stack (MemOp.state__end_fcall job) errors
							end
				in
				let job, errors = unwind_stack job errors in

				(* Update coverage. *)
				let exHist = job#exHist in
				let exHist =
					(* We didn't add the outgoing edge in exec_stmt because the
						 call might have never returned. Since there isn't an
						 explicit return (because we handle the call internally), we
						 have to add the edge now. *)
					if job#inTrackedFn && !Executeargs.arg_line_coverage then
						let loc = Job.get_loc job in
						{ exHist with Job.coveredLines = Job.LineSet.add (loc.Cil.file, loc.Cil.line) exHist.Job.coveredLines; }
					else
						exHist
				in
				let exHist =
					(* Update edge coverage. *)
					if job#inTrackedFn && !Executeargs.arg_edge_coverage then
						let fn = (List.hd job#state.callstack).svar.vname in
						let edge = (
							{ Job.siFuncName = fn; Job.siStmt = Coverage.stmtAtEndOfBlock job#stmt; },
							{ Job.siFuncName = fn; Job.siStmt = Coverage.stmtAtEndOfBlock stmt; }
						) in
						{ exHist with Job.coveredEdges = Job.EdgeSet.add edge exHist.Job.coveredEdges; }
					else
						exHist
				in

                (* TODO (martin): update job.decision_path *)
				(* Update the state, program counter, and coverage  *)
				let job, errors = set_return_value job retopt bytes errors in
				let job = job#with_stmt stmt in
				let job = job#with_exHist exHist in
				let job = job#with_instrList [] in
				(job, errors)
			in


			let jobs, errors = (job : #Info.t)#fork begin fun job arg (jobs, errors) ->
				let job, errors = process_stmtPtr job arg errors in
                (* TODO: update jid_unique and jid_parent as well *)
				((Job.Active job)::jobs, errors)
			end stmtPtrAddrs ([], errors) in
			match jobs with
				| _::_::_ -> (Job.Fork jobs, errors)
				| [a] -> (a, errors)
				| [] -> failwith "No valid jongjmp target found!"
		end

		| _ -> failwith "longjmp invalid arguments"


let otter_get_allocated_size job retopt exps errors =
    let exp = get_lone_arg exps in
    let job, bytes, errors = Expression.rval job exp errors in
    let job, lvals, errors = Expression.deref job bytes (Cil.typeOf exp) errors in
    let size = make_Bytes_Conditional (conditional__map
        (fun (x, y) -> Unconditional (int_to_bytes x.memory_block_size))
        lvals)
    in
    let job, errors = set_return_value job retopt size errors in
    let job = end_function_call job in
    (Job.Active job, errors)

let otter_mute job retopt exps errors =
	Output.arg_print_mute := !Output.arg_print_mute + 1;
	let job = end_function_call job in
	(Job.Active job, errors)

let otter_voice job retopt exps errors =
	Output.arg_print_mute := !Output.arg_print_mute - 1;
	let job = end_function_call job in
	(Job.Active job, errors)

(* A global map that maps integer indices to Instruction.t *)
module IndexMap = State.IndexMap
let instructions_map = ref IndexMap.empty

let otter_instr_mark job retopt exps errors =
    let exp = get_lone_arg exps in
    let index = get_constant_int exp in
    let instruction = Job.get_instruction job in
    instructions_map := IndexMap.add index instruction (!instructions_map);
    let job = end_function_call job in
    (Job.Active job, errors)

let otter_distance_from_mark job retopt exps errors =
    let exp = get_lone_arg exps in
    let index = get_constant_int exp in
    let distance = 
        try
            let instruction_src = IndexMap.find index (!instructions_map) in
            let instruction_des = Job.get_instruction job in
            OtterCFG.DistanceToTargets.find instruction_src [instruction_des] 
        with Not_found -> max_int (* denotes infinite distance, same as DistanceToReturn *)
    in
    let ret = Bytes.int_to_bytes distance in
    let job, errors = set_return_value job retopt ret errors in
    let job = end_function_call job in
    (Job.Active job, errors)

let noop job _ _ errors =
    Job.Active (end_function_call job), errors

let interceptor job job_queue interceptor = Profiler.global#call "BuiltinFunctions.interceptor" begin fun () ->
	try
		(
		(* intercept builtin functions *)
		(                                  (*"__SYMBOLIC"*)            intercept_symbolic) @@
		(intercept_function_by_name_internal (!Executeargs.arg_failurefn) otter_failure) @@

        (* libc functions that are built-in *)
		(intercept_function_by_name_internal "__otter_get_allocated_size" otter_get_allocated_size) @@
		(intercept_function_by_name_internal "__libc_setjmp"           libc_setjmp) @@
		(intercept_function_by_name_internal "__libc_longjmp"          libc_longjmp) @@
		(intercept_function_by_name_internal "malloc"                  libc_malloc) @@
		(intercept_function_by_name_internal "free"                    libc_free) @@
		(intercept_function_by_name_internal "alloca"                  libc___builtin_alloca) @@
		(intercept_function_by_name_internal "__builtin_alloca"        libc___builtin_alloca) @@
		(intercept_function_by_name_internal "__builtin_va_arg_fixed"  libc___builtin_va_arg) @@
		(intercept_function_by_name_internal "__builtin_va_arg"        libc___builtin_va_arg) @@
		(intercept_function_by_name_internal "__builtin_va_copy"       libc___builtin_va_copy) @@
		(intercept_function_by_name_internal "__builtin_va_end"        libc___builtin_va_end) @@
		(intercept_function_by_name_internal "__builtin_va_start"      libc___builtin_va_start) @@
		(* These default to the C implementation on failure *)
		(try_with_job_abandoned_interceptor
			(intercept_function_by_name_internal "memcpy"                  libc_memcpy)) @@
		(try_with_job_abandoned_interceptor
			(intercept_function_by_name_internal "memset"                  libc_memset)) @@
		(try_with_job_abandoned_interceptor
			(intercept_function_by_name_internal "memmove"                  libc_memmove)) @@
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
		(intercept_function_by_name_internal "__BREAKPT"               otter_break_pt) @@
		(intercept_function_by_name_internal "__PRINT_STATE"           otter_print_state) @@
		(intercept_function_by_name_internal "__otter_mute"            otter_mute) @@
		(intercept_function_by_name_internal "__otter_voice"           otter_voice) @@
		(intercept_function_by_name_internal "__otter_instr_mark"          otter_instr_mark) @@
		(intercept_function_by_name_internal "__otter_distance_from_mark"  otter_distance_from_mark) @@

		(* pass on the job when none of those match *)
		interceptor

		) job job_queue
	with Failure msg ->
		if !Executeargs.arg_failfast then failwith msg;
		(Job.Complete (Job.Abandoned (`Failure msg, job)), job_queue) (* TODO (see above) *)
end

let libc_interceptor job job_queue interceptor = Profiler.global#call "BuiltinFunctions.libc_interceptor" begin fun () ->
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

		(* grp.h *)
		(intercept_function_by_name_external "getgrgid"                "__otter_libc_getgrgid") @@
		(intercept_function_by_name_external "getgrnam"                "__otter_libc_getgrnam") @@
		(intercept_function_by_name_external "getgrgid_r"              "__otter_libc_getgrgid_r") @@
		(intercept_function_by_name_external "getgrnam_r"              "__otter_libc_getgrnam_r") @@
		(intercept_function_by_name_external "getgrent"                "__otter_libc_getgrent") @@
		(intercept_function_by_name_external "endgrent"                "__otter_libc_endgrent") @@
		(intercept_function_by_name_external "setgrent"                "__otter_libc_setgrent") @@

		(* setjmp.h: both setjmp and longjmp are built-in functions *)

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
		(intercept_function_by_name_external "calloc"                  "__otter_libc_calloc") @@
		(intercept_function_by_name_external "realloc"                 "__otter_libc_realloc") @@
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
		(intercept_function_by_name_external "stpcpy"                  "__otter_libc_stpcpy") @@
		(intercept_function_by_name_external "strncpy"                 "__otter_libc_strncpy") @@
		(intercept_function_by_name_external "strncmp"                 "__otter_libc_strncmp") @@
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
		(intercept_function_by_name_external "getgid"                  "__otter_libc_getgid") @@
		(intercept_function_by_name_external "setgid"                  "__otter_libc_setgid") @@
		(intercept_function_by_name_external "getegid"                 "__otter_libc_getegid") @@
		(intercept_function_by_name_external "geteuid"                 "__otter_libc_geteuid") @@
		(intercept_function_by_name_external "dup"                     "__otter_libc_dup") @@
		(intercept_function_by_name_external "dup2"                    "__otter_libc_dup2") @@
		(intercept_function_by_name_external "getpagesize"             "__otter_libc_getpagesize") @@
		(intercept_function_by_name_external "sysconf"                 "__otter_libc_sysconf") @@
		(intercept_function_by_name_external "setsid"                  "__otter_libc_setsid") @@
		(intercept_function_by_name_external "getpid"                  "__otter_libc_getpid") @@
		(intercept_function_by_name_external "getppid"                 "__otter_libc_getppid") @@
		(intercept_function_by_name_external "getpgid"                 "__otter_libc_getpgid") @@
		(intercept_function_by_name_external "getpgrp"                 "__otter_libc_getpgrp") @@
		(intercept_function_by_name_external "getgroups"               "__otter_libc_getgroups") @@
		(intercept_function_by_name_external "chdir"                   "__otter_libc_chdir") @@
		(intercept_function_by_name_external "fchdir"                  "__otter_libc_fchdir") @@
		(intercept_function_by_name_external "chroot"                  "__otter_libc_chroot") @@
		(intercept_function_by_name_external "alarm"                   "__otter_libc_alarm") @@

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
		(intercept_function_by_name_external "bind"                    "__otter_libc_bind") @@
		(intercept_function_by_name_external "listen"                  "__otter_libc_listen") @@
		(intercept_function_by_name_external "accept"                  "__otter_libc_accept") @@
		(intercept_function_by_name_external "connect"                 "__otter_libc_connect") @@
		(intercept_function_by_name_external "getsockopt"              "__otter_libc_getsockopt") @@
		(intercept_function_by_name_external "setsockopt"              "__otter_libc_setsockopt") @@
		(intercept_function_by_name_external "shutdown"                "__otter_libc_shutdown") @@

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

		(* sys/syslog *)
		(intercept_function_by_name_external "closelog"                "__otter_libc_closelog") @@
		(intercept_function_by_name_external "openlog"                 "__otter_libc_openlog") @@
		(intercept_function_by_name_external "setlogmask"              "__otter_libc_setlogmask") @@
		(intercept_function_by_name_external "syslog"                  "__otter_libc_syslog") @@
		(intercept_function_by_name_external "vsyslog"                 "__otter_libc_vsyslog") @@

		(* sys/uio.h *)
		(intercept_function_by_name_external "readv"                   "__otter_libc_readv") @@
		(intercept_function_by_name_external "writev"                  "__otter_libc_writev") @@

		(* pass on the job when none of those match *)
		interceptor

		) job job_queue
	with Failure msg ->
		if !Executeargs.arg_failfast then failwith msg;
		(Job.Complete (Job.Abandoned (`Failure msg, job)), job_queue)
end
