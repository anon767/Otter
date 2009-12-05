open Cil
open Bytes
open Types

(**
 *	memory frame
 *)

let frame__empty = VarinfoMap.empty;;

let frame__varinfo_to_lval_block frame varinfo =
	VarinfoMap.find varinfo frame
;;

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
;;

let frame__add_varinfos frame block_to_bytes varinfos block_type =
	List.fold_left begin fun (frame, block_to_bytes) varinfo ->
		frame__add_varinfo frame block_to_bytes varinfo None block_type
	end (frame, block_to_bytes) varinfos
;;

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
;;

(**
 *	string table
 *)
let string_table__add bytes : memory_block =
	let block = block__make_string_literal ("@literal:"^(To_string.bytes bytes)) (bytes__length bytes) in
	let string_table2 = MemoryBlockMap.add block bytes (!string_table) in
		string_table := string_table2;
		block
;;

let string_table__get block =
	MemoryBlockMap.find block (!string_table)
;;

let string_table__mem block =
	MemoryBlockMap.mem block (!string_table)
;;

(** Vargs table
 *)
let vargs_table__add state byteslst : state*bytes =
	let key = bytes__symbolic 4 in
	let va_arg_map2 = VargsMap.add key byteslst state.va_arg_map in
		({state with va_arg_map = va_arg_map2;},key)
;;

let vargs_table__get_list state key : bytes list =
	VargsMap.find key state.va_arg_map
;;

let vargs_table__get state key : state*bytes =
	let byteslst = vargs_table__get_list state key in
	match byteslst with
		| [] -> failwith "va_list has run to the end"
		| hd::tl ->
			({state with va_arg_map = (VargsMap.add key tl state.va_arg_map);},	hd)
;;

let vargs_table__remove state key : state =
	{state with va_arg_map = (VargsMap.remove key state.va_arg_map);}
;;

let loc_table__has state loc =
	LocMap.mem loc state.loc_map
;;
let loc_table__add state loc bytes : state =
	{state with loc_map = LocMap.add loc bytes state.loc_map;}
;;
let loc_table__get state loc : bytes =
	LocMap.find loc state.loc_map
;;

(**
 *	state
 *)
let state__empty =
	{
		global = frame__empty;
		formals = [frame__empty];
		locals = [frame__empty]; (* permit global init with another global *)
		extra = VarinfoMap.empty;
		callstack = [];
		block_to_bytes = MemoryBlockMap.empty;
		path_condition = [];
		path_condition_tracked = [];
		(*return = None;*)
		callContexts = [];
		va_arg = [];
		va_arg_map = VargsMap.empty;
		loc_map = LocMap.empty;
        bytes_eval_cache = BytesMap.empty;
	}
;;

let state__force state = function
	| Immediate x -> (state, x)
	| Deferred f -> f state
;;

let state__force_with_update state update = function
	| Immediate x ->
		(state, x)
	| Deferred f ->
		(* update with the forced value *)
		let state, x = f state in
		(update state x, x)
;;

let state__has_block state block =
	if block.memory_block_type == Block_type_StringLiteral then
		string_table__mem block
	else
		MemoryBlockMap.mem block state.block_to_bytes
;;

let state__add_global state varinfo init = 
	let new_global, new_block_to_bytes =
		frame__add_varinfo state.global state.block_to_bytes varinfo (Some init) Block_type_Global
	in
	{	state with
		global = new_global;
		block_to_bytes = new_block_to_bytes;
	}
;;
	
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
;;

let state__add_block state block bytes =
	{ state with
		block_to_bytes = MemoryBlockMap.add block (Immediate bytes) state.block_to_bytes;
	}
;;

let state__add_deferred_block state block deferred =
	{ state with
		block_to_bytes = MemoryBlockMap.add block (Deferred deferred) state.block_to_bytes;
	}
;;

let state__remove_block state block=
	{ state with
		block_to_bytes = MemoryBlockMap.remove block state.block_to_bytes;
	}
;;

let state__get_bytes_from_block state block =
	if block.memory_block_type == Block_type_StringLiteral then
		(state, string_table__get block)
	else
		let deferred = MemoryBlockMap.find block state.block_to_bytes in
		state__force_with_update state (fun state bytes -> state__add_block state block bytes) deferred
;;

let state__get_deferred_from_block state block =
	if block.memory_block_type == Block_type_StringLiteral then
		Immediate (string_table__get block)
	else
		MemoryBlockMap.find block state.block_to_bytes
;;

let state__deref ?pre state (lvals, size) =
	let deref state pre (block, offset) =
		let state, bytes = state__get_bytes_from_block state block in
		(state, conditional__bytes (bytes__read ~test:(Stp.query_guard state.path_condition) ~pre bytes offset size))
	in
	let state, c = conditional__map_fold ?pre deref state lvals in
	(state, make_Bytes_Conditional c)
;;

let rec state__assign state (lvals, size) bytes =
	let assign state pre (block, offset) =
		(* TODO: provide some way to report partial error *)
		if block.memory_block_type == Block_type_StringLiteral then
			failwith "Error: write to a constant string literal"
		else

		let state, oldbytes = state__force state (MemoryBlockMap.find block state.block_to_bytes) in

		let newbytes = bytes__write oldbytes offset size bytes in
		(* Morris' axiom of assignment *)
		let newbytes = match pre with
			| Guard_True -> newbytes
			| _          -> make_Bytes_Conditional ( IfThenElse (pre, conditional__bytes newbytes, conditional__bytes oldbytes) )
		in
		Output.set_mode Output.MSG_ASSIGN;
		Output.print_endline ("    Assign "^(To_string.bytes bytes)^" to "^(To_string.memory_block block)^","^(To_string.bytes offset));
		state__add_block state block newbytes
	in
	conditional__fold assign state lvals
;;

(* start a new function call frame *)
let state__start_fcall state callContext fundec argvs =
    Output.set_mode Output.MSG_FUNC;
    Output.print_endline (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
    Output.print_endline ("Enter function " ^ (To_string.fundec fundec));
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
			Output.set_mode Output.MSG_FUNC;
			Output.print_endline ("Rest of args: "^(Utility.print_list To_string.bytes va_arg " , "));
			{ state with va_arg = va_arg::state.va_arg }
		| _, [] ->
			failwith "Unreachable init_argvs"
	in
	assign_argvs state fundec.Cil.sformals argvs
;;

let state__end_fcall state =
	Output.set_mode Output.MSG_FUNC;
	Output.print_endline ("Exit function "^(To_string.fundec (List.hd state.callstack)));
	Output.print_endline ("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<");
	let block_to_bytes = state.block_to_bytes in
	let block_to_bytes = frame__clear_varinfos (List.hd state.locals) block_to_bytes in
	let block_to_bytes = frame__clear_varinfos (List.hd state.formals) block_to_bytes in
    { state with formals = List.tl state.formals; 
                 locals = List.tl state.locals;
                 callstack = List.tl state.callstack;
                 block_to_bytes = block_to_bytes;
                 va_arg = List.tl state.va_arg;
                 callContexts = List.tl state.callContexts }
;;

let state__get_callContext state = List.hd state.callContexts;;

let state__add_path_condition state bytes tracked=
	{ state with
		path_condition = bytes::(state.path_condition);
		path_condition_tracked = tracked::(state.path_condition_tracked);
	}
;;

let state__add_bytes_eval_cache state bytes boolval =
  if true then state else
  if BytesMap.mem bytes state.bytes_eval_cache then
    failwith "state__add_bytes_eval_cache: value of bytes already set"
  else
	{ state with
       bytes_eval_cache = BytesMap.add bytes boolval state.bytes_eval_cache;
	}
;;

let bytes_eval_cache_hits = ref 0;;
let bytes_eval_cache_misses = ref 0;;

let state__get_bytes_eval_cache state bytes =
  if not Executeargs.run_args.Executeargs.arg_opt_bytes_eval_cache then None else
    begin
      try
        let ret = Some (BytesMap.find bytes state.bytes_eval_cache) in
          Utility.increment bytes_eval_cache_hits; ret
      with Not_found -> 
        Utility.increment bytes_eval_cache_misses; None
    end
;;

let state__trace state: string = 
	List.fold_left begin fun str context -> match context with
		| Runtime            -> Format.sprintf "%s/Runtime" str
		| Source (_,_,instr,_) -> Format.sprintf "%s/%s" str (To_string.location (Cil.get_instrLoc instr))
		| NoReturn instr     -> Format.sprintf "%s/NoReturn@%s" str (To_string.location (Cil.get_instrLoc instr))
	end "" state.callContexts
;;

(* 
 *    clone the structure of bytes
 *    newbytes =  bytes[$i->$i'] for all i
 *    Let PC be the path condition. 
 *    Then newPC = PC && PC[$i->$i'] for all i
 *)
(* 
 * This function was introduced to implement abstract set. Maybe we don't need
 * it anymore?
 * *)
(*
let state__clone_bytes state bytes =
    let rec 
    traverse_bytes bytes process merge =
      match bytes with
        | Bytes_Constant(_) ->  (merge [],bytes)
        | Bytes_ByteArray(arr) -> 
            let (fact,arr') = process arr in
              (fact,make_Bytes_ByteArray(arr'))
        | Bytes_Address(blkOpt,offset) ->
            let (fact,offset') = traverse_bytes offset process merge in
              (fact,make_Bytes_Address(blkOpt,offset'))
        | Bytes_Op(op,lst) -> 
            let (fact,lst') = traverse_bytes_list lst process merge in
              (fact,make_Bytes_Op(op,lst'))
        | Bytes_Read (content,offset,size) -> 
            let (fact1,content') = traverse_bytes content process merge in
            let (fact2,offset') = traverse_bytes offset process merge in
              (merge [fact1;fact2],make_Bytes_Read(content',offset',size))
        | Bytes_Write(oldbytes,offset,size,newbytes) -> 
            let (fact1,oldbytes') = traverse_bytes oldbytes process merge in
            let (fact2,offset') = traverse_bytes offset process merge in
            let (fact3,newbytes') = traverse_bytes newbytes process merge in
              (merge [fact1;fact2;fact3],make_Bytes_Write(oldbytes',offset',size,newbytes'))
        | Bytes_FunPtr (_) -> (merge [],bytes)
    and
    traverse_bytes_list bytesTypLst process merge =
      let (fact,lst) = 
      List.fold_left 
        (fun (fact,lst) (bytes_elm,typ) ->
            let (fact2,bytes_elm') = traverse_bytes bytes_elm process merge in
              (merge [fact;fact2], (bytes_elm',typ)::lst)
        )   
        (merge [],[]) 
        bytesTypLst
      in (fact,List.rev lst)
    in
    let extract_mapping arr =  
      let (mapping,lst) = 
      ImmutableArray.fold_left 
          (fun ((mfrom,mto),lst) byte ->
             match byte with 
               | Byte_Concrete(_) -> ((mfrom,mto),byte::lst)
               | Byte_Symbolic(s) -> 
                   let new_s = symbol__next () in
                    ((s::mfrom,new_s::mto),(make_Byte_Symbolic(new_s))::lst)
               | Byte_Bytes(_,_) -> failwith "state__clone_bytes: Byte_Bytes not supported"
          )
          (([],[]),[])
          arr
      in (mapping,ImmutableArray.of_list (List.rev lst))
    and extract_merge lst = 
      List.fold_left
         ( fun (af,at) (bf,bt) -> (List.rev_append af bf,List.rev_append at bt))
         ([],[])
         lst
    in
    let (mapping,cloned_bytes) = traverse_bytes bytes extract_mapping extract_merge in
    let rec apply_mapping arr = 
      let (truth,lst) = 
      ImmutableArray.fold_left 
          (fun (truth,lst) byte ->
             match byte with 
               | Byte_Concrete(_) -> (truth,byte::lst)
               | Byte_Symbolic(s) -> 
                     let rec impl mapping = 
                     match mapping with
                       | ([],[]) -> (truth,byte::lst)
                       | (h_from::t_from,h_to::t_to) -> 
                           if (s==h_from) then (true,(make_Byte_Symbolic(h_to))::lst)
                           else impl (t_from,t_to)
                       | _ -> failwith "state__clone_bytes: unreachable"
                     in impl mapping
               | Byte_Bytes(_,_) -> failwith "state__clone_bytes: Byte_Bytes not supported"
          )
          (false,[])
          arr
      in (truth,ImmutableArray.of_list (List.rev lst))
    and apply_merge lst =
      match lst with [] -> false | h::t -> h || (apply_merge t)
    in
    let added_pc = List.fold_left 
                     ( fun a b ->
                         let (hasChanged,b') = (traverse_bytes b apply_mapping apply_merge) in 
                           if hasChanged then b'::a else a
                     )
                     [] state.path_condition in
    let state2 = 
      { state with
            path_condition = List.rev_append added_pc state.path_condition;
      }
    in
      (state2,cloned_bytes)
;;
 *)

(** map address to state (!) *)
let index_to_state: state Utility.IndexMap.t ref = ref (Utility.IndexMap.empty);;
let index_to_state__add index state = 
	index_to_state := Utility.IndexMap.add index state (!index_to_state)
;;
let index_to_state__get index = 
	Utility.IndexMap.find index (!index_to_state)
;;


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
	    			Output.print_endline (Format.sprintf " >> %s = %s" (block.memory_block_name) (To_string.bytes bytes1));
	    			Output.print_endline (Format.sprintf " << %s = %s" (block.memory_block_name) (To_string.bytes bytes2));
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
	    			Output.print_endline (Format.sprintf " %s %s = %s" prefix (block1.memory_block_name) (To_string.bytes bytes1));
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
;;
	
(*
let function_stat: int Cilutility.FundecMap.t ref = ref Cilutility.FundecMap.empty;;
let function_stat_increment fundec =
	let count = if Cilutility.FundecMap.mem fundec (!function_stat) 
	then
		Cilutility.FundecMap.find fundec (!function_stat)
	else 0
	in
		function_stat := Cilutility.FundecMap.add fundec (count+1) (!function_stat)
;;

let function_stat_get fundec = Cilutility.FundecMap.find fundec (!function_stat);;
*)
