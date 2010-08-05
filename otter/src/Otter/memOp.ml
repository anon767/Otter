open OcamlUtilities
open Cil
open Bytes
open BytesUtility
open Types
open Operation
open Ternary

(**
 *	memory frame
 *)

let frame__empty = VarinfoMap.empty

let frame__varinfo_to_lval_block frame varinfo =
	VarinfoMap.find varinfo frame


let frame__add_varinfo frame block_to_bytes varinfo bytes_opt block_type =
	let size = (Cil.bitsSizeOf varinfo.vtype) / 8 in
	let block = block__make (To_string.varinfo varinfo) size block_type in
	let bytes = match bytes_opt with
		| Some bytes -> bytes
		| None -> bytes__make_default size byte__undef (* initially the symbolic 'undef' byte *)
(*		| None -> make_Bytes_ByteArray ({ImmutableArray.empty with ImmutableArray.length = size}) in (* initially undefined (so any accesses will crash the executor) *) *)
(*		| None -> bytes__symbolic size in (* initially symbolic *) *)
	in
	let frame = VarinfoMap.add varinfo (Immediate (conditional__lval_block (block, bytes__zero))) frame in
	let block_to_bytes = MemoryBlockMap.add block (Immediate bytes) block_to_bytes in
	(frame, block_to_bytes)


let frame__add_varinfos frame block_to_bytes varinfos block_type =
	List.fold_left begin fun (frame, block_to_bytes) varinfo ->
		frame__add_varinfo frame block_to_bytes varinfo None block_type
	end (frame, block_to_bytes) varinfos


let frame__clear_varinfos frame block_to_bytes =
	(* only de-allocate stack variables allocated during symbolic execution *)
	let remove_locals = conditional__fold begin fun block_to_bytes _ (block, _) ->
		match block.memory_block_type with
			| Block_type_Local -> MemoryBlockMap.remove block block_to_bytes
			| _                -> block_to_bytes
	end in
	VarinfoMap.fold begin fun varinfo deferred block_to_bytes -> match deferred with
		| Immediate lvals -> remove_locals block_to_bytes lvals
		| _ -> block_to_bytes
	end frame block_to_bytes


(**
 *	string table
 *)
let string_table__add bytes : memory_block =
	let block = block__make_string_literal (FormatPlus.sprintf "@@literal:%a" BytesPrinter.bytes bytes) (bytes__length bytes) in
	let string_table2 = MemoryBlockMap.add block bytes (!string_table) in
		string_table := string_table2;
		block


let string_table__get block =
	MemoryBlockMap.find block (!string_table)


let string_table__mem block =
	MemoryBlockMap.mem block (!string_table)


(** Vargs table
 *)
let vargs_table__add state byteslst : state*bytes =
	let key = bytes__symbolic (bitsSizeOf (TBuiltin_va_list []) / 8) in
	let va_arg_map2 = VargsMap.add key byteslst state.va_arg_map in
		({state with va_arg_map = va_arg_map2;},key)


let vargs_table__get_list state key : bytes list =
	VargsMap.find key state.va_arg_map


let vargs_table__get state key : state*bytes =
	let byteslst = vargs_table__get_list state key in
	match byteslst with
		| [] -> failwith "va_list has run to the end"
		| hd::tl ->
			({state with va_arg_map = (VargsMap.add key tl state.va_arg_map);},	hd)


let vargs_table__remove state key : state =
	{state with va_arg_map = (VargsMap.remove key state.va_arg_map);}


(**
 *	state
 *)
let state__empty =
	{
		global = frame__empty;
		formals = [frame__empty];
		locals = [frame__empty]; (* permit global init with another global *)
		extra = VarinfoMap.empty;
		malloc = VarinfoMap.empty;
		callstack = [];
		block_to_bytes = MemoryBlockMap.empty;
		path_condition = [];
		path_condition_tracked = [];
		(*return = None;*)
		callContexts = [];
		stmtPtrs = Types.IndexMap.empty;
		va_arg = [];
		va_arg_map = VargsMap.empty;
		bytes_eval_cache = BytesMap.empty;
	}


let state__force state = function
	| Immediate x -> (state, x)
	| Deferred f -> f state


let state__force_with_update state update = function
	| Immediate x ->
		(state, x)
	| Deferred f ->
		(* update with the forced value *)
		let state, x = f state in
		(update state x, x)


let state__has_block state block =
	if block.memory_block_type == Block_type_StringLiteral then
		string_table__mem block
	else
		MemoryBlockMap.mem block state.block_to_bytes


let state__add_global state varinfo init =
	let new_global, new_block_to_bytes =
		frame__add_varinfo state.global state.block_to_bytes varinfo (Some init) Block_type_Global
	in
	{	state with
		global = new_global;
		block_to_bytes = new_block_to_bytes;
	}


let state__add_formal state varinfo init =
	let new_formal, new_block_to_bytes =
		frame__add_varinfo (List.hd state.formals) state.block_to_bytes varinfo (Some init) Block_type_Local
	in
	{	state with
		formals = new_formal::(List.tl state.formals);
		block_to_bytes = new_block_to_bytes;
	}


let state__add_frame state =
  {state with
    formals = frame__empty::(state.formals);
    locals = frame__empty::(state.locals);
  }


	
let state__varinfo_to_lval_block state varinfo =
	let local = List.hd state.locals in
	if VarinfoMap.mem varinfo local then
		let deferred = frame__varinfo_to_lval_block local varinfo in
		let update state lval =
			let local = VarinfoMap.add varinfo (Immediate lval) local in
			{ state with locals=local::List.tl state.locals }
		in
		state__force_with_update state update deferred
	else
		let formal = List.hd state.formals in
		if VarinfoMap.mem varinfo formal then
			let deferred = frame__varinfo_to_lval_block formal varinfo in
			let update state lval =
				let formal = VarinfoMap.add varinfo (Immediate lval) formal in
				{ state with formals=formal::List.tl state.formals }
			in
			state__force_with_update state update deferred
		else
			let global = state.global in
			if VarinfoMap.mem varinfo global then
				let deferred = frame__varinfo_to_lval_block global varinfo in
				let update state lval =
					let global = VarinfoMap.add varinfo (Immediate lval) global in
					{ state with global=global }
				in
				state__force_with_update state update deferred
			else (* varinfo may be a function *)
				failwith ("Varinfo "^(varinfo.vname)^" not found.")


let state__add_block state block bytes =
	{ state with
		block_to_bytes = MemoryBlockMap.add block (Immediate bytes) state.block_to_bytes;
	}


let state__add_deferred_block state block deferred =
	{ state with
		block_to_bytes = MemoryBlockMap.add block (Deferred deferred) state.block_to_bytes;
	}


let state__remove_block state block=
	{ state with
		block_to_bytes = MemoryBlockMap.remove block state.block_to_bytes;
	}


let state__get_bytes_from_block state block =
	if block.memory_block_type == Block_type_StringLiteral then
		(state, string_table__get block)
	else
		let deferred = MemoryBlockMap.find block state.block_to_bytes in
		state__force_with_update state (fun state bytes -> state__add_block state block bytes) deferred


let state__get_deferred_from_block state block =
	if block.memory_block_type == Block_type_StringLiteral then
		Immediate (string_table__get block)
	else
		MemoryBlockMap.find block state.block_to_bytes


let state__deref ?pre state (lvals, size) =
	let deref state pre (block, offset) =
		let state, bytes = state__get_bytes_from_block state block in
		(state, conditional__bytes (bytes__read ~test:(Stp.query_guard state.path_condition) ~pre bytes offset size))
	in
	let state, c = conditional__map_fold ?pre deref state lvals in
	(state, make_Bytes_Conditional c)


let rec state__assign state (lvals, size) bytes =
	let assign state pre (block, offset) =
		(* TODO: provide some way to report partial error *)
		if block.memory_block_type == Block_type_StringLiteral then
			failwith "Error: write to a constant string literal"
		else

		let state, oldbytes = state__force state (MemoryBlockMap.find block state.block_to_bytes) in

		(* TODO: pruning the conditional bytes here leads to repeated work if it is subsequently read via state__deref;
		 * however, not pruning leads to O(k^(2^n)) leaves in the conditional bytes for n consecutive assignments. *)
		let newbytes = bytes__write ~test:(Stp.query_guard state.path_condition) ~pre oldbytes offset size bytes in

		(* Morris' axiom of assignment *)
		let newbytes = match pre with
			| Guard_True -> newbytes
			| _          -> make_Bytes_Conditional ( IfThenElse (pre, conditional__bytes newbytes, conditional__bytes oldbytes) )
		in
		Output.set_mode Output.MSG_ASSIGN;
		Output.printf "Assign@ @[%a@]@ to @[%a, %a@]@\n" BytesPrinter.bytes bytes BytesPrinter.memory_block block BytesPrinter.bytes offset;
		state__add_block state block newbytes
	in
	conditional__fold assign state lvals


(* start a new function call frame *)
let state__start_fcall state callContext fundec argvs =

    Output.set_mode Output.MSG_FUNC;
    (* Output.print_endline (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
     *)

    Output.printf "Enter function %s@\n" (To_string.fundec fundec);
    (* set up the new stack frame *)
	let block_to_bytes = state.block_to_bytes in
	let formal, block_to_bytes = frame__add_varinfos frame__empty block_to_bytes fundec.Cil.sformals Block_type_Local in
	let local, block_to_bytes = frame__add_varinfos frame__empty block_to_bytes fundec.Cil.slocals Block_type_Local in
	let state = { state with formals = formal::state.formals;
	                         locals = local::state.locals;
	                         callstack = fundec::state.callstack;
	                         block_to_bytes = block_to_bytes;
	                         callContexts = callContext::state.callContexts } in
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
			{ state with va_arg = va_arg::state.va_arg }
		| _, [] ->
			failwith ("Not enough arguments to function " ^ fundec.svar.vname)
	in
	assign_argvs state fundec.Cil.sformals argvs


let state__end_fcall state =
	Output.set_mode Output.MSG_FUNC;
	Output.printf "Exit function %s@\n" (To_string.fundec (List.hd state.callstack));
	(* Output.print_endline ("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<");
     *)
	let block_to_bytes = state.block_to_bytes in
	let block_to_bytes = frame__clear_varinfos (List.hd state.locals) block_to_bytes in
	let block_to_bytes = frame__clear_varinfos (List.hd state.formals) block_to_bytes in
    { state with formals = List.tl state.formals;
                 locals = List.tl state.locals;
                 callstack = List.tl state.callstack;
                 block_to_bytes = block_to_bytes;
                 va_arg = List.tl state.va_arg;
                 callContexts = List.tl state.callContexts }


let state__get_callContext state = List.hd state.callContexts

let state__extract_path_condition state bytes = 
(**
  *   remove pc \in PC if bytes -> pc
  *   This should be faster
  *)
  (*
  let bytes_implies_pc bytes pc =
    match Stp.consult_stp [bytes] pc with
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
    match Stp.consult_stp bytes_lst pc with
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
    impl state.path_condition state.path_condition_tracked


let state__add_path_condition state bytes tracked=
  let path_condition,path_condition_tracked =
    if Executeargs.run_args.Executeargs.arg_simplify_path_condition then
      Stats.time "Simplify PC" (state__extract_path_condition state) bytes
    else
      state.path_condition,state.path_condition_tracked
  in
	{ state with
		path_condition = bytes::path_condition;
		path_condition_tracked = tracked::path_condition_tracked;
	}


let state__add_bytes_eval_cache state bytes boolval =
  if true then state else
  if BytesMap.mem bytes state.bytes_eval_cache then
    failwith "state__add_bytes_eval_cache: value of bytes already set"
  else
	{ state with
       bytes_eval_cache = BytesMap.add bytes boolval state.bytes_eval_cache;
	}


let bytes_eval_cache_hits = ref 0
let bytes_eval_cache_misses = ref 0

let state__get_bytes_eval_cache state bytes =
    begin
      try
        let ret = Some (BytesMap.find bytes state.bytes_eval_cache) in
          incr bytes_eval_cache_hits; ret
      with Not_found -> 
        incr bytes_eval_cache_misses; None
    end


let state__trace state: string =
	List.fold_left begin fun str context -> match context with
		| Runtime            -> Format.sprintf "%s/Runtime" str
		| Source (_,_,instr,_) -> Format.sprintf "%s/%s" str (To_string.location (Cil.get_instrLoc instr))
		| NoReturn instr     -> Format.sprintf "%s/NoReturn@@%s" str (To_string.location (Cil.get_instrLoc instr))
	end "" state.callContexts


(** map address to state (!) *)
let index_to_state: state Types.IndexMap.t ref = ref (Types.IndexMap.empty)
let index_to_state__add index state = 
	index_to_state := Types.IndexMap.add index state (!index_to_state)

let index_to_state__get index = 
	Types.IndexMap.find index (!index_to_state)



(** Compare two states. Return true if they are the same; false otherwise. *)
let cmp_states (s1:state) (s2:state) =
	(* Compare blocks (memory allocations) that both states have *)
	let sharedBlocksComparison =
		let f block deferred1 result =
			(* TODO: should the forced state of s1 be propagated? *)
			let _, bytes1 = state__force s1 deferred1 in
			let typ = block.memory_block_type in
			if typ!=Block_type_Global && typ!=Block_type_Heap then result else (* only care about globals and heap content *)
	          try
	    		let deferred2 = MemoryBlockMap.find block s2.block_to_bytes in
				(* TODO: should the forced state of s2 be propagated? *)
				let _, bytes2 = state__force s2 deferred2 in
	    		if bytes__equal bytes1 bytes2 then
					result
				else begin
	    			Output.printf " >> %s@ = @[%a@]@\n" (block.memory_block_name) BytesPrinter.bytes bytes1;
	    			Output.printf " << %s@ = @[%a@]@\n" (block.memory_block_name) BytesPrinter.bytes bytes2;
					false
				end
	          with Not_found -> result
		in
		MemoryBlockMap.fold f s1.block_to_bytes true
	in
	(* List blocks (memory allocations) that only one of the states has *)
	let unsharedBlocksComparison =
	  let h prefix state1 state2 block1 deferred1 result =
			let _, bytes1 = state__force state1 deferred1 in
			let typ = block1.memory_block_type in
				if typ!=Block_type_Global && typ!=Block_type_Heap then result else (* only care about globals and heap content *)
	        if MemoryBlockMap.mem block1 state2.block_to_bytes then result else (
	    			Output.printf " %s %s@ = @[%a@]@\n" prefix (block1.memory_block_name) BytesPrinter.bytes bytes1;
						false
					)
	  in
	  MemoryBlockMap.fold (h "(>>)" s1 s2) s1.block_to_bytes true &&
			MemoryBlockMap.fold (h "(<<)" s2 s1) s2.block_to_bytes true
	in
	let callStackComparison =
		let rec cmpCallStack cs1 cs2 =
			match cs1,cs2 with
				| [],[] -> true
				| h1::cs1',h2::cs2' when h1==h2 -> cmpCallStack cs1' cs2'
				| _ -> false
		in
		cmpCallStack s1.callstack s2.callstack
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

let rec state__eval state pc bytes =
	(*
	if not Executeargs.args.Executeargs.arg_print_queries then () else
	Output.print_endline ("Is the following not equal to zero? \n"^(To_string.bytes bytes));*)
  let nontrivial () = 
    Output.set_mode Output.MSG_REG;
    Output.printf "Ask STP...@\n";
    (state,Stats.time "STP" (Stp.consult_stp pc) bytes)

  in
	let is_comparison op = match op with	
		| OP_LT -> true
		| OP_GT -> true

		| OP_LE -> true
		| OP_GE -> true
		| OP_EQ -> true
		| OP_NE -> true
		| _ -> false
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
   let state,return_bytes = 
     match bytes with
       (* The following cases are simple enough to not consult STP *)
       | Bytes_Constant (CInt64(n,_,_)) -> state,if n = 0L then False else True			

       | Bytes_ByteArray (bytearray) ->
           begin try
             let b = bytes_to_bool bytes in  (* TODO:need to use int64 *)
               state,
                                             if b = false then False else True
           with Failure(_) -> nontrivial()
           end
         | Bytes_Address (_,_) -> state,True

             (* nullity check *)
             | Bytes_Op(OP_LNOT,(b1,_)::[]) -> 
                 let state,bb = state__eval state pc b1 in
                   state,ternary_not bb

             (* Comparison of (ptr+i) and (ptr+j) *)
             | Bytes_Op(op,(Bytes_Address(block1,offset1),_)::(Bytes_Address(block2,offset2),_)::[]) 
                 when is_comparison op ->
                 if block1!=block2 then 
                   (if op==OP_EQ then state,False else if op==OP_NE then state,True else nontrivial())
                 else  
                   state__eval state pc (run (operation_of op) [(offset1,Cil.intType);(offset2,Cil.intType)])

             (* Comparison of (ptr+i) and c (usually zero) *)
             | Bytes_Op(op,(Bytes_Address(block,offset1),_)::(bytes2,_)::[]) 
                 when is_comparison op  &&  isConcrete_bytes bytes2 ->
                 if op==OP_EQ then state,False else if op==OP_NE then state,True else nontrivial()
             (* Function pointer is always true *)
             | Bytes_FunPtr(_,_) -> state,True
	   	(* Consult STP *)
	   	| _ -> 
	   		nontrivial()
   in
     state,return_bytes

let eval_with_cache state pc bytes =
	state__eval state pc bytes

