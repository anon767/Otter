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
let timed_query_stp name pc pre guard = Timer.time name (fun () -> Stp.query_stp pc pre guard) ()

(**
 *	memory frame
 *)

let frame__varinfo_to_lval_block frame varinfo =
	VarinfoMap.find varinfo frame


let frame__add_varinfo frame block_to_bytes varinfo zero block_type =
    if VarinfoMap.mem varinfo frame then FormatPlus.invalid_arg "MemOp.frame__add_varinfo: %a already exist" CilPrinter.varinfo varinfo;
    let size = (Cil.bitsSizeOf varinfo.vtype) / 8 in
    let size = if size <= 0 then 1 else size in
    let block = block__make (FormatPlus.as_string CilPrinter.varinfo varinfo) size block_type in
    let bytes = bytes__make_default size (if zero then byte__zero else byte__undef) in
    let frame = VarinfoMap.add varinfo (Deferred.Immediate (conditional__lval_block (block, bytes__zero))) frame in
    let block_to_bytes = MemoryBlockMap.add block (Deferred.Immediate bytes) block_to_bytes in
    (frame, block_to_bytes, block)


let frame__add_varinfos frame block_to_bytes varinfos zero block_type =
    List.fold_left begin fun (frame, block_to_bytes) varinfo ->
        let frame, block_to_bytes, _ = frame__add_varinfo frame block_to_bytes varinfo zero block_type in
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
        let block = block__make (FormatPlus.sprintf "@@const:%a" BytesPrinter.bytes bytes) (bytes__length bytes) Block_type_Const in
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
let vargs_table__add state byteslst =
	let key = bytes__symbolic (bitsSizeOf (TBuiltin_va_list []) / 8) in
	let va_arg_map2 = VargsMap.add key byteslst state#state.va_arg_map in
	(state#with_state { state#state with va_arg_map = va_arg_map2; },key)


let vargs_table__get_list state key : bytes list =
	VargsMap.find key state#state.va_arg_map


let vargs_table__get state key =
	let byteslst = vargs_table__get_list state key in
	match byteslst with
		| [] -> failwith "va_list has run to the end"
		| hd::tl -> (state#with_state { state#state with va_arg_map = (VargsMap.add key tl state#state.va_arg_map); }, hd)


let vargs_table__remove state key =
	state#with_state { state#state with va_arg_map = (VargsMap.remove key state#state.va_arg_map); }


(**
 *	state
 *)

let state__has_block state block =
    const_table__mem block || MemoryBlockMap.mem block state#state.block_to_bytes


let state__add_global state varinfo =
    if not varinfo.Cil.vglob then FormatPlus.invalid_arg "MemOp.state__add_global: %a is not a global variable" CilPrinter.varinfo varinfo;
    let block_type = if CilData.CilVar.is_const varinfo then Block_type_Const else Block_type_Global in
    let global, block_to_bytes, block = frame__add_varinfo state#state.global state#state.block_to_bytes varinfo true block_type in
    let state = state#with_state { state#state with global = global; block_to_bytes = block_to_bytes } in
    (state, block)


let state__varinfo_to_lval_block ?pre state varinfo =
	(* lookup varinfo in locals, formals and globals, prune and update the store, and return the result *)
	if varinfo.Cil.vglob then
		let global = state#state.global in
		if VarinfoMap.mem varinfo global then
			let deferred = frame__varinfo_to_lval_block global varinfo in
			let state, lval = Deferred.force state deferred in
			let lval = conditional__prune ~test:(timed_query_stp "query_stp/state__varinfo_to_lval_block/globals" state#state.path_condition) ?pre lval in
			let global = VarinfoMap.add varinfo (Deferred.Immediate lval) global in
			(state#with_state { state#state with global=global }, lval)
		else (* varinfo may be a function *)
			failwith ("Varinfo "^(varinfo.vname)^" not found.")
	else
		let local = List.hd state#state.locals in
		if VarinfoMap.mem varinfo local then
			let deferred = frame__varinfo_to_lval_block local varinfo in
			let state, lval = Deferred.force state deferred in
			let lval = conditional__prune ~test:(timed_query_stp "query_stp/state__varinfo_to_lval_block/locals" state#state.path_condition) ?pre lval in
			let local = VarinfoMap.add varinfo (Deferred.Immediate lval) local in
			(state#with_state { state#state with locals=local::List.tl state#state.locals }, lval)
		else
			let formal = List.hd state#state.formals in
			if VarinfoMap.mem varinfo formal then
				let deferred = frame__varinfo_to_lval_block formal varinfo in
				let state, lval = Deferred.force state deferred in
				let lval = conditional__prune ~test:(timed_query_stp "query_stp/state__varinfo_to_lval_block/formals" state#state.path_condition) ?pre lval in
				let formal = VarinfoMap.add varinfo (Deferred.Immediate lval) formal in
				(state#with_state { state#state with formals=formal::List.tl state#state.formals }, lval)
			else (* varinfo may be a function *)
				failwith ("Varinfo "^(varinfo.vname)^" not found.")


let state__add_block state block bytes =
	state#with_state { state#state with
		block_to_bytes = MemoryBlockMap.add block (Deferred.Immediate bytes) state#state.block_to_bytes;
	}


let state__add_deferred_block state block deferred =
	state#with_state { state#state with
		block_to_bytes = MemoryBlockMap.add block (Deferred.Deferred deferred) state#state.block_to_bytes;
	}


let state__remove_block state block=
	state#with_state { state#state with
		block_to_bytes = MemoryBlockMap.remove block state#state.block_to_bytes;
	}


let state__get_bytes_from_block state block =
    try
        (state, const_table__get block)
    with Not_found ->
        let deferred = MemoryBlockMap.find block state#state.block_to_bytes in
        let state, bytes = Deferred.force state deferred in
        (state__add_block state block bytes, bytes)


let state__get_deferred_from_block state block =
    try
        Deferred.Immediate (const_table__get block)
    with Not_found ->
        MemoryBlockMap.find block state#state.block_to_bytes


let state__deref ?pre state (lvals, size) =
    let deref state pre (block, offset) =
        let state, bytes = state__get_bytes_from_block state block in
        (state, conditional__bytes (bytes__read ~test:(timed_query_stp "query_stp/state__deref" state#state.path_condition) ~pre bytes offset size))
    in
    let state, c = conditional__fold_map ?pre deref state lvals in
    (state, make_Bytes_Conditional c)


let rec state__assign state (lvals, size) bytes =
	let assign state pre (block, offset) =
		(* TODO: provide some way to report partial error *)

		(* C99 6.7.3.5: If an attempt is made to modify an object defined with a const-qualified type through use
		 * of an lvalue with non-const-qualified type, the behavior is undefined. If an attempt is made to refer
		 * to an object defined with a volatile-qualified type through use of an lvalue with non-volatile-qualified
		 * type, the behavior is undefined. *)
		if block.memory_block_type = Block_type_Const then FormatPlus.failwith "Write to a const: %s" block.memory_block_name;

		let state, oldbytes = Deferred.force state (MemoryBlockMap.find block state#state.block_to_bytes) in

		(* TODO: pruning the conditional bytes here leads to repeated work if it is subsequently read via state__deref;
		 * however, not pruning leads to O(k^(2^n)) leaves in the conditional bytes for n consecutive assignments. *)
		let newbytes = bytes__write ~test:(timed_query_stp "query_stp/state__assign" state#state.path_condition) ~pre oldbytes offset size bytes in

		(* Morris' axiom of assignment *)
		let newbytes = match pre with
			| Guard_True -> newbytes
			| _          -> make_Bytes_Conditional ( IfThenElse (pre, conditional__bytes newbytes, conditional__bytes oldbytes) )
		in
		Output.set_mode Output.MSG_ASSIGN;
		Output.printf "Assign@ @[%a@]@ to @[%a@], @[%a@]@\n" BytesPrinter.bytes bytes BytesPrinter.memory_block block BytesPrinter.bytes offset;
		state__add_block state block newbytes
	in
	conditional__fold assign state lvals


(* start a new function call frame *)
let state__start_fcall state callContext fundec argvs =
    (* set up the new stack frame *)
	let block_to_bytes = state#state.block_to_bytes in
	let formal, block_to_bytes = frame__add_varinfos VarinfoMap.empty block_to_bytes fundec.Cil.sformals !Executeargs.arg_init_local_zero Block_type_Local in
	let local, block_to_bytes = frame__add_varinfos VarinfoMap.empty block_to_bytes fundec.Cil.slocals !Executeargs.arg_init_local_zero Block_type_Local in
	let state = state#with_state { state#state with
		formals = formal::state#state.formals;
		locals = local::state#state.locals;
		callstack = fundec::state#state.callstack;
		block_to_bytes = block_to_bytes;
		callContexts = callContext::state#state.callContexts;
	} in
    (* assign arguments to parameters *)
	let rec assign_argvs state pars argvs = match pars, argvs with
		| par::pars, argv::argvs ->
			let state, lval_block = state__varinfo_to_lval_block state par in
			let size = (Cil.bitsSizeOf par.Cil.vtype)/8 in
			let state = state__assign state (lval_block, size) argv in
			assign_argvs state pars argvs
		| [], va_arg ->
			if va_arg <> [] then (
				(* If there are extra arguments but the function is not a vararg function, raise an error *)
				if (match fundec.svar.vtype with TFun(_, _, false, _) -> true | _ -> false) then (
					failwith ("Too many arguments to non-vararg function " ^ fundec.svar.vname)
				);
				Output.set_mode Output.MSG_FUNC;
				Output.printf "Rest of args:@ @[%a@]@\n" (FormatPlus.pp_print_list BytesPrinter.bytes ",@ ") va_arg;
			);
			state#with_state { state#state with va_arg = va_arg::state#state.va_arg }
		| _, [] ->
			failwith ("Not enough arguments to function " ^ fundec.svar.vname)
	in
	assign_argvs state fundec.Cil.sformals argvs


let state__end_fcall state =
    (* TODO: move this to Statement *)
	Output.set_mode Output.MSG_FUNC;
	Output.printf "@[Exit function %a@]@\n" CilPrinter.fundec (List.hd state#state.callstack);
	let block_to_bytes = state#state.block_to_bytes in
	let block_to_bytes = frame__clear_varinfos (List.hd state#state.locals) block_to_bytes in
	let block_to_bytes = frame__clear_varinfos (List.hd state#state.formals) block_to_bytes in
	state#with_state { state#state with
		formals = List.tl state#state.formals;
		locals = List.tl state#state.locals;
		callstack = List.tl state#state.callstack;
		block_to_bytes = block_to_bytes;
		va_arg = List.tl state#state.va_arg;
		callContexts = List.tl state#state.callContexts;
	}


let state__get_callContext state = List.hd state#state.callContexts

let state__extract_path_condition state bytes =
(**
  *   remove pc \in PC if bytes -> pc
  *   This should be faster
  *)
  (*
  let bytes_implies_pc bytes pc =
    match Stp.query_bytes [bytes] pc with
      | Ternary.True -> true
      | _ -> false
  in
  *)
(**
  *   remove pc \in PC if bytes&&(PC\pc) -> pc
  *   This can be very slow in some situations
  *
  *   TODO: maybe we can have a combination of these 2 methods?
  *)
  let bytes_and_others_implies_pc bytes_lst pc =
    match Stp.query_bytes bytes_lst pc with
      | Ternary.True -> true
      | _ -> false
  in
  let rec impl pc_lst pct_lst = match pc_lst,pct_lst with
    | pc::pc_lst' , pct::pct_lst' ->
        let pc_lst'',pct_lst'' = impl pc_lst' pct_lst' in
          (*if bytes_implies_pc bytes pc then*)
          if bytes_and_others_implies_pc (bytes::pc_lst'') pc then
            pc_lst'',pct_lst''
          else
            pc::pc_lst'',pct::pct_lst''
    | [] , [] -> [],[]
    | _ -> failwith "Error in state__extract_path_condition"
  in
    impl state#state.path_condition state#state.path_condition_tracked


let state__add_path_condition state bytes tracked=
	let path_condition, path_condition_tracked =
		if !Executeargs.arg_simplify_path_condition then
			Stats.time "Simplify PC" (state__extract_path_condition state) bytes
		else
			(state#state.path_condition, state#state.path_condition_tracked)
	in
	state#with_state { state#state with
		path_condition = bytes::path_condition;
		path_condition_tracked = tracked::path_condition_tracked;
	}


let state__trace state =
	FormatPlus.as_string (Printer.callingContext_list "/") (List.rev state#state.callContexts)


(** Compare two states. Return true if they are the same; false otherwise. *)
let cmp_states s1 s2 =
	(* Compare blocks (memory allocations) that both states have *)
	let sharedBlocksComparison =
		let f block deferred1 result =
			(* TODO: should the forced state of s1 be propagated? *)
			let _, bytes1 = Deferred.force s1 deferred1 in
			let typ = block.memory_block_type in
			if typ!=Block_type_Global && typ!=Block_type_Heap then result else (* only care about globals and heap content *)
	          try
	    		let deferred2 = MemoryBlockMap.find block s2#state.block_to_bytes in
				(* TODO: should the forced state of s2 be propagated? *)
				let _, bytes2 = Deferred.force s2 deferred2 in
	    		if bytes__equal bytes1 bytes2 then
					result
				else begin
	    			Output.printf " >> %s@ = @[%a@]@\n" (block.memory_block_name) BytesPrinter.bytes bytes1;
	    			Output.printf " << %s@ = @[%a@]@\n" (block.memory_block_name) BytesPrinter.bytes bytes2;
					false
				end
	          with Not_found -> result
		in
		MemoryBlockMap.fold f s1#state.block_to_bytes true
	in
	(* List blocks (memory allocations) that only one of the states has *)
	let unsharedBlocksComparison =
	  let h prefix state1 state2 block1 deferred1 result =
			let _, bytes1 = Deferred.force state1 deferred1 in
			let typ = block1.memory_block_type in
				if typ!=Block_type_Global && typ!=Block_type_Heap then result else (* only care about globals and heap content *)
	        if MemoryBlockMap.mem block1 state2#state.block_to_bytes then result else (
	    			Output.printf " %s %s@ = @[%a@]@\n" prefix (block1.memory_block_name) BytesPrinter.bytes bytes1;
						false
					)
	  in
	  MemoryBlockMap.fold (h "(>>)" s1 s2) s1#state.block_to_bytes true &&
			MemoryBlockMap.fold (h "(<<)" s2 s1) s2#state.block_to_bytes true
	in
	let callStackComparison =
		let rec cmpCallStack cs1 cs2 =
			match cs1,cs2 with
				| [],[] -> true
				| h1::cs1',h2::cs2' when h1==h2 -> cmpCallStack cs1' cs2'
				| _ -> false
		in
		cmpCallStack s1#state.callstack s2#state.callstack
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
    Output.printf "Ask STP...@\n";
    Stp.query_bytes pc bytes

  in
	let operation_of op = match op with
		| OP_LT -> lt
		| OP_GT -> gt
		| OP_LE -> le
		| OP_GE -> ge
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

      | Bytes_Op(OP_LNOT,[(b1,_)]) -> Ternary.not (eval pc b1)

      (* Comparison of two pointers *)
      | Bytes_Op((OP_LT|OP_GT|OP_LE|OP_GE|OP_EQ|OP_NE as op),
                 [(Bytes_Address(block1,offset1),_); (Bytes_Address(block2,offset2),_)]) ->
            if block1!=block2 then
                (if op==OP_EQ then Ternary.False else if op==OP_NE then Ternary.True else nontrivial())
            else
                eval pc ((operation_of op) [(offset1,Cil.intType);(offset2,Cil.intType)])

      (* Comparison of pointer and integer *)
      | Bytes_Op((OP_EQ | OP_NE) as op,[(Bytes_Address(block,offset1),_); (bytes2,_)])
              when isConcrete_bytes bytes2 ->
            Ternary.of_bool (op = OP_NE)

      | Bytes_Op(OP_LAND, [(bytes1, _); (bytes2, _)]) ->
            begin match eval pc bytes1 with
              | Ternary.False -> Ternary.False
              | Ternary.True -> eval pc bytes2
              | Ternary.Unknown -> if eval (bytes1 :: pc) bytes2 = Ternary.False then Ternary.False else Ternary.Unknown
            end
      | Bytes_Op(OP_LOR, [(bytes1, _); (bytes2, _)]) ->
            begin match eval pc bytes1 with
              | Ternary.True -> Ternary.True
              | Ternary.False -> eval pc bytes2
              | Ternary.Unknown -> if eval (logicalNot bytes1 :: pc) bytes2 = Ternary.True then Ternary.True else Ternary.Unknown
            end
      (* Consult STP *)
      | _ ->
            nontrivial()
