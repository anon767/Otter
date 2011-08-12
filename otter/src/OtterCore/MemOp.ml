open DataStructures
open OcamlUtilities
open CilUtilities
open Cil
open OtterBytes
open Bytes
open BytesUtility
open State
open Operator

(* Track Stp calls *)
let timed_query_stp name =
    fun job pre guard ->
        (job : _ #Info.t)#profile_call name (fun job -> (job, BytesSTP.query_stp (PathCondition.clauses job#state.path_condition) pre guard))

(**
 *	memory frame
 *)

let frame__varinfo_to_lval_block frame varinfo =
	VarinfoMap.find varinfo frame


let frame__add_varinfo frame block_to_bytes varinfo init_method block_type = Profiler.global#call "MemOp.frame__add_varinfo" begin fun () ->
    if VarinfoMap.mem varinfo frame then FormatPlus.invalid_arg "MemOp.frame__add_varinfo: %a already exist" CilPrinter.varinfo varinfo;
    let size = (Cil.bitsSizeOf varinfo.vtype) / 8 in
    let size = if size <= 0 then 1 else size in
    let block = block__make (FormatPlus.as_string CilPrinter.varinfo varinfo) (int_to_bytes size) block_type in
    let bytes = bytes__make_default size (init_method ()) in
    let frame = VarinfoMap.add varinfo (Deferred.Immediate (conditional__lval_block (block, bytes__zero))) frame in
    let block_to_bytes = MemoryBlockMap.add block (Deferred.Immediate bytes) block_to_bytes in
    (frame, block_to_bytes, block)
end


let frame__add_varinfos frame block_to_bytes varinfos init_method block_type = 
    List.fold_left begin fun (frame, block_to_bytes) varinfo ->
        let frame, block_to_bytes, _ = frame__add_varinfo frame block_to_bytes varinfo init_method block_type in
        (frame, block_to_bytes)
    end (frame, block_to_bytes) varinfos


let frame__clear_varinfos frame block_to_bytes =
	(* only de-allocate stack variables allocated during symbolic execution *)
	let remove_locals = conditional__fold begin fun block_to_bytes _ (block, _) ->
		match block.memory_block_type with
			| Block_type_Local -> MemoryBlockMap.remove block block_to_bytes
			| _                -> block_to_bytes
	end in
	VarinfoMap.fold begin fun varinfo deferred block_to_bytes -> match deferred with
		| Deferred.Immediate lvals -> remove_locals block_to_bytes lvals
		| _ -> block_to_bytes
	end frame block_to_bytes


(**
 *	anonymous const table
 *)

module BytesMap = Map.Make (struct
    type t = bytes
    let compare = Pervasives.compare
end)


let const_to_block = ref BytesMap.empty
let block_to_const = ref MemoryBlockMap.empty


let const_table__find bytes =
    try
        BytesMap.find bytes !const_to_block
    with Not_found ->
        let block = block__make (FormatPlus.sprintf "@@const:%a" BytesPrinter.bytes bytes) (int_to_offset_bytes (bytes__length bytes)) Block_type_Const in
        const_to_block := BytesMap.add bytes block !const_to_block;
        block_to_const := MemoryBlockMap.add block bytes !block_to_const;
        block


let const_table__get block =
    if block.memory_block_type <> Block_type_Const then raise Not_found;
    MemoryBlockMap.find block !block_to_const


let const_table__mem block =
    block.memory_block_type = Block_type_Const && MemoryBlockMap.mem block !block_to_const


(** Vargs table
 *)
let vargs_table__add job byteslst =
	let key = bytes__symbolic (bitsSizeOf (TBuiltin_va_list []) / 8) in
	let va_arg_map2 = VargsMap.add key byteslst job#state.va_arg_map in
	(job#with_state { job#state with va_arg_map = va_arg_map2; },key)


let vargs_table__get_list job key : bytes list =
	VargsMap.find key job#state.va_arg_map


let vargs_table__get job key =
	let byteslst = vargs_table__get_list job key in
	match byteslst with
		| [] -> failwith "va_list has run to the end"
		| hd::tl -> (job#with_state { job#state with va_arg_map = (VargsMap.add key tl job#state.va_arg_map); }, hd)


let vargs_table__remove job key =
	job#with_state { job#state with va_arg_map = (VargsMap.remove key job#state.va_arg_map); }


(**
 *	state
 *)

let state__has_block job block =
    const_table__mem block || MemoryBlockMap.mem block job#state.block_to_bytes


let state__add_global job varinfo =
    if not varinfo.Cil.vglob then FormatPlus.invalid_arg "MemOp.state__add_global: %a is not a global variable" CilPrinter.varinfo varinfo;
    let block_type = if CilData.CilVar.is_const varinfo then Block_type_Const else Block_type_Global in
    let global, block_to_bytes, block = frame__add_varinfo job#state.global job#state.block_to_bytes varinfo !InitBytes.init_global block_type in
    let job = job#with_state { job#state with global = global; block_to_bytes = block_to_bytes } in
    (job, block)


let state__varinfo_to_lval_block ?pre job varinfo = Profiler.global#call "MemOp.state__varinfo_to_lval_block" begin fun () ->
	(* lookup varinfo in locals, formals and globals, prune and update the store, and return the result *)
	if varinfo.Cil.vglob then
		let global = job#state.global in
		if VarinfoMap.mem varinfo global then
			let deferred = frame__varinfo_to_lval_block global varinfo in
			let job, lval = Deferred.force job deferred in
			let job, lval = conditional__prune ~test:(timed_query_stp "query_stp/state__varinfo_to_lval_block/globals") ?pre job lval in
			let global = VarinfoMap.add varinfo (Deferred.Immediate lval) global in
			(job#with_state { job#state with global=global }, lval)
		else (* varinfo may be a function *)
			failwith ("Varinfo "^(varinfo.vname)^" not found.")
	else
		let local = List.hd job#state.locals in
		if VarinfoMap.mem varinfo local then
			let deferred = frame__varinfo_to_lval_block local varinfo in
			let job, lval = Deferred.force job deferred in
			let job, lval = conditional__prune ~test:(timed_query_stp "query_stp/state__varinfo_to_lval_block/locals") ?pre job lval in
			let local = VarinfoMap.add varinfo (Deferred.Immediate lval) local in
			(job#with_state { job#state with locals=local::List.tl job#state.locals }, lval)
		else
			let formal = List.hd job#state.formals in
			if VarinfoMap.mem varinfo formal then
				let deferred = frame__varinfo_to_lval_block formal varinfo in
				let job, lval = Deferred.force job deferred in
				let job, lval = conditional__prune ~test:(timed_query_stp "query_stp/state__varinfo_to_lval_block/formals") ?pre job lval in
				let formal = VarinfoMap.add varinfo (Deferred.Immediate lval) formal in
				(job#with_state { job#state with formals=formal::List.tl job#state.formals }, lval)
			else (* varinfo may be a function *)
				failwith ("Varinfo "^(varinfo.vname)^" not found.")
end


let state__add_block job block bytes =
	job#with_state { job#state with
		block_to_bytes = MemoryBlockMap.add block (Deferred.Immediate bytes) job#state.block_to_bytes;
	}


let state__add_deferred_block job block deferred =
	job#with_state { job#state with
		block_to_bytes = MemoryBlockMap.add block (Deferred.Deferred deferred) job#state.block_to_bytes;
	}


let state__remove_block job block=
	job#with_state { job#state with
		block_to_bytes = MemoryBlockMap.remove block job#state.block_to_bytes;
	}


let state__get_bytes_from_block job block =
    try
        (job, const_table__get block)
    with Not_found ->
        let deferred = MemoryBlockMap.find block job#state.block_to_bytes in
        let job, bytes = Deferred.force job deferred in
        (state__add_block job block bytes, bytes)


let state__get_deferred_from_block job block =
    try
        Deferred.Immediate (const_table__get block)
    with Not_found ->
        MemoryBlockMap.find block job#state.block_to_bytes


let state__deref job (lvals, size) =
    (* prune the lval as well as the values at the lvals to avoid loading from obsolete locations or loading obsolete
     * values which may be inconsistent with the path condition, which may lead to spurious warnings *)
    let deref job pre (block, offset) =
        let job, bytes = state__get_bytes_from_block job block in
        let job, bytes = bytes__read ~test:(timed_query_stp "query_stp/state__deref/2") ~pre job bytes offset size in
        (job, conditional__bytes bytes)
    in
    let job, c =
        conditional__fold_map
            ~test:(timed_query_stp "query_stp/state__deref/1")
            deref job lvals
    in
    (job, make_Bytes_Conditional c)


let state__assign job (lvals, size) bytes = Profiler.global#call "MemOp.state__assign" begin fun () -> 
    (* prune the lval as well as the values at the lvals to avoid loading from obsolete locations or loading obsolete
     * values which may be inconsistent with the path condition, which may lead to spurious warnings *)
    let assign job pre (block, offset) =
        (* TODO: provide some way to report partial error *)

        (* C99 6.7.3.5: If an attempt is made to modify an object defined with a const-qualified type through use
         * of an lvalue with non-const-qualified type, the behavior is undefined. If an attempt is made to refer
         * to an object defined with a volatile-qualified type through use of an lvalue with non-volatile-qualified
         * type, the behavior is undefined. *)
        if block.memory_block_type = Block_type_Const then FormatPlus.failwith "Write to a const: %s" block.memory_block_name;

        let job, oldbytes = Deferred.force job (MemoryBlockMap.find block job#state.block_to_bytes) in

        (* TODO: pruning the conditional bytes here leads to repeated work if it is subsequently read via state__deref;
         * however, not pruning leads to O(k^(2^n)) leaves in the conditional bytes for n consecutive assignments. *)
        let job, newbytes = bytes__write ~test:(timed_query_stp "query_stp/state__assign/2") ~pre job oldbytes offset size bytes in

        (* Morris' axiom of assignment *)
        let newbytes = match pre with
            | Guard_True -> newbytes
            | _          -> make_Bytes_Conditional ( IfThenElse (pre, conditional__bytes newbytes, conditional__bytes oldbytes) )
        in
        Output.set_mode Output.MSG_ASSIGN;
        Output.printf "@[Assign@ @[%a@]@ to @[%a@], @[%a@]@]@." BytesPrinter.bytes bytes BytesPrinter.memory_block block BytesPrinter.bytes offset;
        state__add_block job block newbytes
    in
    conditional__fold
        ~test:(timed_query_stp "query_stp/state__assign/1")
        assign job lvals
end


(* start a new function call frame *)
let state__start_fcall job callContext fundec argvs = Profiler.global#call "MemOp.state__start_fcall" begin fun () -> 
    (* set up the new stack frame *)
	let block_to_bytes = job#state.block_to_bytes in
	let formal, block_to_bytes = frame__add_varinfos VarinfoMap.empty block_to_bytes fundec.Cil.sformals !InitBytes.init_local Block_type_Local in
	let local, block_to_bytes = frame__add_varinfos VarinfoMap.empty block_to_bytes fundec.Cil.slocals !InitBytes.init_local Block_type_Local in
	let job = job#with_state { job#state with
		formals = formal::job#state.formals;
		locals = local::job#state.locals;
		callstack = fundec::job#state.callstack;
		block_to_bytes = block_to_bytes;
		callContexts = callContext::job#state.callContexts;
	} in
    let job = job#push_caller_list in
    (* assign arguments to parameters *)
	let rec assign_argvs job pars argvs = match pars, argvs with
		| par::pars, argv::argvs ->
			let job, lval_block = state__varinfo_to_lval_block job par in
			let size = (Cil.bitsSizeOf par.Cil.vtype)/8 in
			let job = state__assign job (lval_block, size) argv in
			assign_argvs job pars argvs
		| [], va_arg ->
			if va_arg <> [] then (
				(* If there are extra arguments but the function is not a vararg function, raise an error *)
				if (match fundec.svar.vtype with TFun(_, _, false, _) -> true | _ -> false) then (
					failwith ("Too many arguments to non-vararg function " ^ fundec.svar.vname)
				);
				Output.set_mode Output.MSG_FUNC;
				Output.printf "@[Rest of args:@ @[%a@]@]@." (FormatPlus.pp_print_list BytesPrinter.bytes ",@ ") va_arg;
			);
			job#with_state { job#state with va_arg = va_arg::job#state.va_arg }
		| _, [] ->
			failwith ("Not enough arguments to function " ^ fundec.svar.vname)
	in
	assign_argvs job fundec.Cil.sformals argvs
end


let state__end_fcall job =
    (* TODO: move this to Statement *)
	Output.set_mode Output.MSG_FUNC;
	Output.printf "@[Exit function %a@]@." CilPrinter.fundec (List.hd job#state.callstack);
	let block_to_bytes = job#state.block_to_bytes in
	let block_to_bytes = frame__clear_varinfos (List.hd job#state.locals) block_to_bytes in
	let block_to_bytes = frame__clear_varinfos (List.hd job#state.formals) block_to_bytes in
    let job = job#pop_caller_list in
	job#with_state { job#state with
		formals = List.tl job#state.formals;
		locals = List.tl job#state.locals;
		callstack = List.tl job#state.callstack;
		block_to_bytes = block_to_bytes;
		va_arg = List.tl job#state.va_arg;
		callContexts = List.tl job#state.callContexts;
	}


let state__get_callContext job = List.hd job#state.callContexts


let state__add_path_condition job bytes tracked=
	job#with_state { job#state with
		path_condition = PathCondition.add bytes tracked job#state.path_condition;
	}


let state__trace job =
	FormatPlus.as_string (Printer.callingContext_list "/") (List.rev job#state.callContexts)


(** Compare two states. Return true if they are the same; false otherwise. *)
let cmp_states job1 job2 =
	(* Compare blocks (memory allocations) that both states have *)
	let sharedBlocksComparison =
		let f block deferred1 result =
			(* TODO: should the forced state of job1 be propagated? *)
			let _, bytes1 = Deferred.force job1 deferred1 in
			let typ = block.memory_block_type in
			if typ!=Block_type_Global && typ!=Block_type_Heap then result else (* only care about globals and heap content *)
	          try
	    		let deferred2 = MemoryBlockMap.find block job2#state.block_to_bytes in
				(* TODO: should the forced state of job2 be propagated? *)
				let _, bytes2 = Deferred.force job2 deferred2 in
	    		if bytes__equal bytes1 bytes2 then
					result
				else begin
	    			Output.printf "@[ >> %s@ = @[%a@]@]@." (block.memory_block_name) BytesPrinter.bytes bytes1;
	    			Output.printf "@[ << %s@ = @[%a@]@]@." (block.memory_block_name) BytesPrinter.bytes bytes2;
					false
				end
	          with Not_found -> result
		in
		MemoryBlockMap.fold f job1#state.block_to_bytes true
	in
	(* List blocks (memory allocations) that only one of the states has *)
	let unsharedBlocksComparison =
	  let h prefix job1 job2 block1 deferred1 result =
			let _, bytes1 = Deferred.force job1 deferred1 in
			let typ = block1.memory_block_type in
				if typ!=Block_type_Global && typ!=Block_type_Heap then result else (* only care about globals and heap content *)
	        if MemoryBlockMap.mem block1 job2#state.block_to_bytes then result else (
	    			Output.printf "@[ %s %s@ = @[%a@]@]@." prefix (block1.memory_block_name) BytesPrinter.bytes bytes1;
						false
					)
	  in
	  MemoryBlockMap.fold (h "(>>)" job1 job2) job1#state.block_to_bytes true &&
			MemoryBlockMap.fold (h "(<<)" job2 job1) job2#state.block_to_bytes true
	in
	let callStackComparison =
		let rec cmpCallStack cs1 cs2 =
			match cs1,cs2 with
				| [],[] -> true
				| h1::cs1',h2::cs2' when h1==h2 -> cmpCallStack cs1' cs2'
				| _ -> false
		in
		cmpCallStack job1#state.callstack job2#state.callstack
	in
	sharedBlocksComparison && unsharedBlocksComparison && callStackComparison


(*
let function_stat: int Cilutility.FundecMap.t ref = ref Cilutility.FundecMap.empty
let function_stat_increment fundec =
	let count = if Cilutility.FundecMap.mem fundec (!function_stat)
	then
		Cilutility.FundecMap.find fundec (!function_stat)
	else 0
	in
		function_stat := Cilutility.FundecMap.add fundec (count+1) (!function_stat)


let function_stat_get fundec = Cilutility.FundecMap.find fundec (!function_stat)
*)


(** Is a value true, false, or unknown in the context of a path condition?
    @param pc a path condition : bytes list
    @param bytes the value to query : bytes
    @return whether pc implies that bytes is nonzero : Ternary.t *)
let rec eval pc bytes =
  let nontrivial () =
    Output.set_mode Output.MSG_REG;
    Output.printf "Ask STP...@.";
    BytesSTP.query_bytes (PathCondition.clauses pc) bytes

  in
	let operation_of op = match op with
		| OP_LT -> lt
		| OP_GT -> gt
		| OP_LE -> le
		| OP_GE -> ge
		| OP_SLT -> signed_lt
		| OP_SGT -> signed_gt
		| OP_SLE -> signed_le
		| OP_SGE -> signed_ge
		| OP_EQ -> eq
		| OP_NE -> ne
		| _ -> failwith "operation_of: operation is not comparison"
	in
    (* Some cases are simple enough to not consult STP *)
    match bytes with
      | Bytes_Constant (CInt64(n,_,_)) -> Ternary.of_bool (n <> 0L)

      | Bytes_ByteArray (bytearray) ->
            begin
                try Ternary.of_bool (bytes_to_bool bytes) (* TODO:need to use int64 *)
                with Failure _ -> nontrivial()
            end

      (* Pointers are always true *)
      | Bytes_Address _
      | Bytes_FunPtr _ -> Ternary.True

      | Bytes_Op (OP_LNOT, [ b1 ]) -> Ternary.not (eval pc b1)

      (* Comparison of two pointers *)
      | Bytes_Op ((OP_LT|OP_GT|OP_LE|OP_GE|OP_SLT|OP_SGT|OP_SLE|OP_SGE|OP_EQ|OP_NE as op),
                 [ Bytes_Address (block1, offset1); Bytes_Address (block2, offset2) ]) ->
            if block1!=block2 then
                (if op==OP_EQ then Ternary.False
                 else if op==OP_NE then Ternary.True
                 else nontrivial()
                 (* TODO: FormatPlus.warn "Inequality comparison between two unrelated pointers: %a" BytesPrinter.bytes bytes *)
                )
            else
                eval pc ((operation_of op) [ offset1; offset2 ] !Cil.upointType)

      (* Operation on two function pointers. Only equality and disequality are allowed. *)
      | Bytes_Op (op, [ Bytes_FunPtr f1; Bytes_FunPtr f2 ]) ->
            begin match op with
                | OP_EQ -> Ternary.of_bool (CilData.CilVar.equal f1 f2)
                | OP_NE -> Ternary.of_bool (not (CilData.CilVar.equal f1 f2))
                | _ -> FormatPlus.failwith "Invalid operation on function pointers: %a" BytesPrinter.bytes bytes
            end

      (* Operation on (normal) pointer and function pointer. This is an error *)
      | Bytes_Op (_, [ Bytes_FunPtr _; Bytes_Address _ ])
      | Bytes_Op (_, [ Bytes_Address _; Bytes_FunPtr _ ]) ->
            FormatPlus.failwith "Operation involving normal pointer and function pointer: %a" BytesPrinter.bytes bytes

      (* Comparison of pointer and something. *)
      | Bytes_Op (op, [ (Bytes_Address _ | Bytes_FunPtr _); bytes2 ])
      | Bytes_Op (op, [ bytes2; (Bytes_Address _ | Bytes_FunPtr _) ]) ->
            if not (isConcrete_bytes bytes2)
            then nontrivial () (* Just ask STP in this case *)
            else (
                (* Comparison of a pointer and an integer. Only (p == 0) and (p != 0) are allowed, and they return [False] and [True] respectively. *)
                if bytes__equal bytes2 bytes__zero
                then match op with
                    | OP_NE -> Ternary.True
                    | OP_EQ -> Ternary.False
                    | _ -> FormatPlus.failwith "Invalid operation: (ptr op 0) where 'op' is neither '==' or '!='\n%a" BytesPrinter.bytes bytes
                else FormatPlus.failwith "Invalid operation: (ptr op x) where x is a nonzero integer: %a" BytesPrinter.bytes bytes
            )

      | Bytes_Op (OP_LAND, [ bytes1; bytes2 ]) ->
            begin match eval pc bytes1 with
              | Ternary.False -> Ternary.False
              | Ternary.True -> eval pc bytes2
              | Ternary.Unknown -> if eval (PathCondition.add bytes1 true pc) bytes2 = Ternary.False then Ternary.False else Ternary.Unknown
            end
      | Bytes_Op (OP_LOR, [ bytes1 ; bytes2 ]) ->
            begin match eval pc bytes1 with
              | Ternary.True -> Ternary.True
              | Ternary.False -> eval pc bytes2
              | Ternary.Unknown -> if eval (PathCondition.add (logicalNot bytes1) true pc) bytes2 = Ternary.True then Ternary.True else Ternary.Unknown
            end
      (* Consult STP *)
      | _ ->
            nontrivial()

