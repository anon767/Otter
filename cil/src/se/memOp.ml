open Cil
open Types

(**
 *  Symbol
 *)
(* negative id is used as special symbolic values.
   0 is used for the symbolic byte representing uninitialized memory *)
let symbol__currentID = ref 1;;
let symbol__next () = 
	{	
		symbol_id = Utility.next_id symbol__currentID; 
	} ;;

let char__random () = Char.chr ((Random.int 255)+1);;

(**
 *	byte 
 *)
let byte__make c = Byte_Concrete c;;
let byte__zero = byte__make ('\000');;
let byte__111 = byte__make ('\255');;
let byte__random () = byte__make (char__random ());;
let byte__symbolic () = Byte_Symbolic (symbol__next ());;
(*
let byte__symbolic_with_id id isWritable = 
	assert (id>0);
	Byte_Symbolic ({symbol_id = -id; })
;;*)

(*
let rec

byte__get_concrete byte : char = 
	match byte with
		| Byte_Concrete (c) -> c
		| Byte_Symbolic (s) ->
			begin match s.symbol_concrete with
				| None -> 
					let c = char__random () in
						s.symbol_concrete <- Some(c);
						c
				| Some(c) -> c
			end
		| Byte_Bytes (bytes,i) -> bytes__get_concrete bytes i

and

bytes__get_concrete bytes i : char =
	match bytes with 
		| Bytes_Constant (constant) -> 
			let bytearray = Convert.constant_to_bytearray constant in
				bytes__get_concrete (Bytes_ByteArray(bytearray)) i
		| Bytes_ByteArray (bytearray) -> 
			let byte = ImmutableArray.get bytearray i in
				byte__get_concrete byte
		| Bytes_Address (_,_)-> word__size 
		| Bytes_Op (op,(bytes2,typ)::tail) -> bytes__length bytes2
		| Bytes_Op (op,[]) -> 0 (* reachable from diff_bytes *)
		| Bytes_Write(bytes2,_,_,_) -> bytes__length bytes2
		| Bytes_Read(_,_,len) -> len
		| Bytes_FunPtr(_) -> word__size
;;
*)

(**
 *	bytes
 *)
(*let bytes__zero n = Bytes_ByteArray (ImmutableArray.make n byte__zero) ;;*)
let bytes__zero = Bytes_Constant(Cil.CInt64(Int64.zero,IInt,None));;
let bytes__of_list (lst: byte list) =	Bytes_ByteArray (ImmutableArray.of_list lst) ;;
let bytes__make_default n byte = Bytes_ByteArray(ImmutableArray.make n byte);;
let bytes__make n = bytes__make_default n byte__zero;;
let bytes__random n =
	let rec impl i arr =
		if i>=n then arr else
			impl (i+1) (ImmutableArray.set arr i (byte__random ()))
		in
	Bytes_ByteArray(impl 0 (ImmutableArray.make n byte__zero))
;;

let bytes__symbolic n =
	let rec impl len = 
		if len <= 0 then [] else (byte__symbolic ())::(impl (len-1))
	in
		bytes__of_list (impl n)
;;


let rec bytes__length bytes =
	match bytes with
		| Bytes_Constant (constant) -> (Cil.bitsSizeOf (Cil.typeOf (Const(constant))))/8
		| Bytes_ByteArray (bytearray) -> ImmutableArray.length bytearray 
		| Bytes_Address (_,_)-> word__size 
		| Bytes_Op (op,(bytes2,typ)::tail) -> bytes__length bytes2
		| Bytes_Op (op,[]) -> 0 (* reachable from diff_bytes *)
		| Bytes_Write(bytes2,_,_,_) -> bytes__length bytes2
		| Bytes_Read(_,_,len) -> len
		| Bytes_FunPtr(_) -> word__size
;;

let rec diff_bytes bytes1 bytes2 = 
	if (bytes__length bytes1 <> bytes__length bytes2) then true else
	match bytes1,bytes2 with
		| Bytes_Constant(c1),_ -> diff_bytes (Convert.constant_to_bytes c1) bytes2
		| _,Bytes_Constant(c2) -> diff_bytes bytes1 (Convert.constant_to_bytes c2)
		| Bytes_ByteArray(a1),Bytes_ByteArray(a2) -> 
			ImmutableArray.fold2_left 
			(fun t e1 e2 -> let t' = match e1,e2 with
				| Byte_Concrete(c1),Byte_Concrete(c2) -> c1<>c2
				| Byte_Symbolic(s1),Byte_Symbolic(s2) -> s1.symbol_id<>s2.symbol_id (* Don't try to do hard compare *)
				| Byte_Bytes(b1,off1),Byte_Bytes(b2,off2) -> (diff_bytes b1 b2) || (off1<>off2)
				| _,_ -> true
				in t || t'
			) 
			false a1 a2
		| Bytes_Address(Some(b1),off1),Bytes_Address(Some(b2),off2) -> 
			(b1!=b2) || (diff_bytes off1 off2)
		| Bytes_Address(None,off1),Bytes_Address(None,off2) -> 
			(diff_bytes off1 off2)
		| Bytes_Op(op1,[]),Bytes_Op(op2,[]) -> false
		| Bytes_Op(op1,(b1,_)::operands1),Bytes_Op(op2,(b2,_)::operands2) -> 
			(op1!=op2) || (diff_bytes b1 b2) ||	(diff_bytes (Bytes_Op(op1,operands1)) (Bytes_Op(op2,operands2)))
		| Bytes_Read(b1,off1,s1),Bytes_Read(b2,off2,s2) -> 
			(diff_bytes b1 b2) || (diff_bytes off1 off2) || (s1<>s2)
		| Bytes_Write(old1,off1,s1,new1),Bytes_Write(old2,off2,s2,new2) -> 
			(diff_bytes old1 old2) || (diff_bytes off1 off2) || (s1<>s2) || (diff_bytes new1 new2)
		| Bytes_FunPtr(f1,addr1),Bytes_FunPtr(f2,addr2) -> 
			(f1!=f2)
		| _,_ ->  true
;;
let same_bytes b1 b2 = not (diff_bytes b1 b2);;

let rec bytes__get_byte bytes i : byte =
	match bytes with
		| Bytes_Constant (constant) ->  bytes__get_byte (Convert.constant_to_bytes constant) i
		| Bytes_ByteArray (bytearray) -> ImmutableArray.get bytearray i 
		| _ -> Byte_Bytes(bytes,i)
;;

let rec bytes__read bytes off len =
	if (bytes__length bytes) = len then bytes else
	let worst_case = Bytes_Read (bytes,off,len) in
	let ret_bytes = 
		begin match bytes,off with
			| Bytes_ByteArray(array),Bytes_Constant(CInt64(i64,k,_)) -> 
					let i = Int64.to_int i64 in
					Bytes_ByteArray (ImmutableArray.sub array i len)
			| Bytes_Constant(constant),Bytes_Constant(CInt64(i64,k,_)) -> 
                    let converted_bytes = Convert.constant_to_bytes constant in
                      begin match converted_bytes with
                        | Bytes_ByteArray(array) ->
					        let i = Int64.to_int i64 in
					        Bytes_ByteArray (ImmutableArray.sub array i len)
                        | _ -> worst_case
                      end
			| Bytes_Write (bytes2,off2,len2,newbytes),_ -> 
				if off2 = off && len2 = len then
					newbytes (* being a bit tricky... *)
				else (* CAUTION: assume [off2,len2] and [off,len] don't overlap.  *)
					worst_case
			| _ -> worst_case
		end
		in
		(* try to inflate any Bytes_ByteArray of Byte_Bytes *)
		match ret_bytes with
			| Bytes_ByteArray(bytearray) ->
					begin match ImmutableArray.get bytearray 0 with
						| Byte_Bytes(condensed_bytes,0) when
								(* Make sure length agrees, and that each byte is the
									 correct one. *)
								bytes__length condensed_bytes = len &&
								(let n = ref 0 in
								 ImmutableArray.for_all
									 (function
												Byte_Bytes(b,i) when i = !n && b == condensed_bytes ->
													incr n; true
											| _ -> false)
									 bytearray)
								-> condensed_bytes
						| _ -> ret_bytes
					end
			| _ -> ret_bytes
;;

let bytes__write bytes off len newbytes =
	let rec do_write bytes off len newbytes =
		match bytes,off,newbytes with
			(* Optimize for memset 
			*)
			| Bytes_ByteArray(oldarray),Bytes_Constant(CInt64(i64,k,_)),Bytes_ByteArray(newarray) ->
				(* from j = 0 to len-1 do oldarray[i+j] = newarray[j] *)
				(* EXPERIMENT: if contents from oldarray is unwritable, then pass *)
				let i = Int64.to_int i64 in
				let rec impl j array =
					if j<0 then array else
						let array2 = impl (j-1) array in
						(*
						let oldbyte = ImmutableArray.get array2 (i+j) in
						match oldbyte with
							| Byte_Symbolic(s) when s.symbol_writable=false -> warning();array2
							| _ ->	
						*)
							ImmutableArray.set array2 (i+j) (ImmutableArray.get newarray j)
				in
					Bytes_ByteArray(impl (len-1) oldarray)
					
			| Bytes_ByteArray(oldarray),Bytes_Constant(CInt64(i64,k,_)),Bytes_Constant(const) ->
				do_write bytes off len (Convert.constant_to_bytes const)
				
			| Bytes_ByteArray(oldarray),Bytes_Constant(CInt64(i64,k,_)),_(* anything *) ->
				let rec impl arr i =
					if i>=word__size then arr else
						impl (ImmutableArray.set arr i (Byte_Bytes(newbytes,i))) (i+1)
				in
					do_write bytes off len (Bytes_ByteArray(impl (ImmutableArray.make word__size byte__zero) 0))			
			
			| Bytes_ByteArray(oldarray),_,_
				when Convert.isConcrete_bytes off ->
					let n_off = Convert.bytes_to_constant off Cil.intType in
					do_write bytes (Bytes_Constant(n_off)) len newbytes

			| _ -> Bytes_Write (bytes,off,len,newbytes)
	in
	if (bytes__length bytes)=len && (Convert.isConcrete_bytes off) && (Convert.bytes_to_int_auto off = 0) then 
      newbytes 
	else
		do_write bytes off len newbytes

;;

let bytes__resize bytes newlen = 
	let oldlen = bytes__length bytes in
		if oldlen=newlen then bytes 
		else if oldlen>newlen then bytes (* don't care if longer *)
		else
			bytes__write (bytes__make newlen) bytes__zero oldlen bytes
;;

(**
 *	memory block
 *)
let block__current_id = ref 1;;
let block__make name n t =
	{
		memory_block_name = name;
		memory_block_id = Utility.next_id block__current_id;
		memory_block_size = n;
		memory_block_addr = bytes__random word__size;
		memory_block_type = t;
	}
;;
let block__make_string_literal name n =
	let block = block__make name n Block_type_StringLiteral in
	{block with
		memory_block_type = Block_type_StringLiteral;
	}
;;

(*
(**
 *  memory heap
 *)
let heap__empty = { address_to_block = AddressMap.empty;} ;;

let heap__address_to_block heap address = 
	AddressMap.find address heap.address_to_block
;;

let heap__map_address_to_block heap address block = 
	{ address_to_block = AddressMap.add address block heap.address_to_block; }
;;

let heap__add_address heap block_to_bytes address size =
	let	block = block__make ("@"^(To_string.bytes address)) size in
	let bytes = bytes__symbolic size in (* initially symbolic *)
	let heap2 = heap__map_address_to_block heap address block in
	let block_to_bytes2 = MemoryBlockMap.add block bytes block_to_bytes in
	(heap2, block_to_bytes2)
;;	

let heap__remove_address heap block_to_bytes address =
	()(* TODO *)
;;
*)
(**
 *	memory frame
 *)
let frame__empty = { varinfo_to_block = VarinfoMap.empty;} ;;

let frame__varinfo_to_block frame varinfo =
	VarinfoMap.find varinfo frame.varinfo_to_block
;;

let frame__map_varinfo_to_block frame varinfo block =
	{	varinfo_to_block = VarinfoMap.add varinfo block frame.varinfo_to_block;}
;;

let frame__add_varinfo_initialized frame block_to_bytes varinfo init block =
	let bytes = init in 
	let frame2 = frame__map_varinfo_to_block frame varinfo block in
	let block_to_bytes2 = MemoryBlockMap.add block bytes block_to_bytes in
	(frame2, block_to_bytes2)
;;

let frame__add_varinfo frame block_to_bytes varinfo =
	let size = (Cil.bitsSizeOf varinfo.vtype) / 8 in
	(* This is only called for local variables. Globals are handled by state__add_global. *)
	let fresh_block = block__make (To_string.varinfo varinfo) size Block_type_Local in
(*	let bytes = Bytes_ByteArray ({ImmutableArray.empty with ImmutableArray.length = size}) in (* initially undefined (so any accesses will crash the executor) *) *)
(*	let bytes = bytes__make_default size byte__undef in (* initially the symbolic 'undef' byte *) *)
	let bytes = bytes__symbolic size in (* initially symbolic *)
		frame__add_varinfo_initialized frame block_to_bytes varinfo bytes fresh_block
;;

let rec frame__add_varinfos frame block_to_bytes varinfos =
	match varinfos with
		| [] -> (frame, block_to_bytes)
		| varinfo:: tail ->
			let (frame2, block_to_bytes2) = frame__add_varinfo frame block_to_bytes varinfo in
			frame__add_varinfos frame2 block_to_bytes2 tail
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
		locals = [frame__empty]; (* permit global init with another global *)
		(*heap = heap__empty;*)
		callstack = [];
		block_to_bytes = MemoryBlockMap.empty;
		path_condition = [];
		(*return = None;*)
		callContexts = [];
		va_arg = [];
		va_arg_map = VargsMap.empty;
		loc_map = LocMap.empty;
	}
;;

let state__add_global state varinfo init = 
	let size = (Cil.bitsSizeOf varinfo.vtype) / 8 in
	let	block =
		try
			VarinfoMap.find varinfo state.global.varinfo_to_block
		with Not_found ->
			block__make (To_string.varinfo varinfo) size Block_type_Global
	in
	let (new_global,new_block_to_bytes) = frame__add_varinfo_initialized state.global state.block_to_bytes varinfo init block in
	{	state with
		global = new_global;
		block_to_bytes = new_block_to_bytes;
	}
;;
	
let state__varinfo_to_block state varinfo =
	let local = List.hd state.locals in
	let global = state.global in
	if VarinfoMap.mem varinfo local.varinfo_to_block then
		frame__varinfo_to_block local varinfo
	else if VarinfoMap.mem varinfo global.varinfo_to_block then
		frame__varinfo_to_block global varinfo
	else (* varinfo may be a function *)
		failwith ("Varinfo "^(varinfo.vname)^" not found.")
;;

let state__assign state (block, offset, size) bytes = (* have problem *)
	if block.memory_block_type == Block_type_StringLiteral then 
		failwith "Error: write to a constant string literal"
	else
	let oldbytes = MemoryBlockMap.find block state.block_to_bytes in
	(*Output.print_endline (To_string.bytes oldbytes);*)
	let newbytes = bytes__write oldbytes offset size bytes in
	(*Output.print_endline (To_string.bytes newbytes);*)
	(*(*TMP*) (if block.memory_block_type = 3 then 
		Output.set_mode Output.MSG_MUSTPRINT
		else		*)
	Output.set_mode Output.MSG_ASSIGN;
	Output.print_endline ("    Assign "^(To_string.bytes bytes)^" to "^(To_string.memory_block block)^","^(To_string.bytes offset));
	{ state with
		block_to_bytes = MemoryBlockMap.add block newbytes state.block_to_bytes;
	}
;;

let state__start_fcall state fundec callContext =
	Output.set_mode Output.MSG_FUNC;
	Output.print_endline (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
	Output.print_endline ("Enter function " ^ (To_string.fundec fundec));
	let vars = List.append fundec.Cil.sformals fundec.Cil.slocals in
	let (frame, block_to_bytes2) = 
		frame__add_varinfos frame__empty state.block_to_bytes vars in
	{ state with
		locals = frame:: state.locals;
		callstack = fundec:: state.callstack;
		block_to_bytes = block_to_bytes2;
		callContexts = callContext::state.callContexts;
	}	;;

let state__end_fcall state =
	Output.set_mode Output.MSG_FUNC;
	Output.print_endline ("Exit function "^(To_string.fundec (List.hd state.callstack)));
	Output.print_endline ("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<");

	{	state with
		locals = List.tl state.locals;
		callstack = List.tl state.callstack;
		callContexts = List.tl state.callContexts;
		va_arg = List.tl state.va_arg;
	}
;;

let state__get_callContext state = List.hd state.callContexts;;

let state__get_bytes_from_block state block =
	let source = 
		if block.memory_block_type == Block_type_StringLiteral 
		then
			string_table__get block
		else
			MemoryBlockMap.find block state.block_to_bytes 
	in
	source
;;

let state__get_bytes_from_lval state (block, offset, size) =
	let source = state__get_bytes_from_block state block 
	in
		bytes__read source offset size
;;

let state__add_path_condition state bytes =
	{ state with
		path_condition = bytes::(state.path_condition);
	}
;;

(*
let state__return state bytesopt = 
	{ state with
		return = bytesopt;	(* TO BE DELETED *)
	}
;;
*)
let state__add_block state block bytes =
	{ state with
		block_to_bytes = MemoryBlockMap.add block bytes state.block_to_bytes;
	}
;;
let state__remove_block state block=
	{ state with
		block_to_bytes = MemoryBlockMap.remove block state.block_to_bytes;
	}
;;

let state__trace state: string = 
	List.fold_left (fun str (_,instr,_) ->
		str^(Printf.sprintf "/%s" (To_string.location (Cil.get_instrLoc instr)))
		) "" state.callContexts
;;

(* 
 *    clone the structure of bytes
 *    newbytes =  bytes[$i->$i'] for all i
 *    Let PC be the path condition. 
 *    Then newPC = PC && PC[$i->$i'] for all i
 *)
let state__clone_bytes state bytes =
    let rec 
    traverse_bytes bytes process merge =
      match bytes with
        | Bytes_Constant(_) ->  (merge [],bytes)
        | Bytes_ByteArray(arr) -> 
            let (fact,arr') = process arr in
              (fact,Bytes_ByteArray(arr'))
        | Bytes_Address(blkOpt,offset) ->
            let (fact,offset') = traverse_bytes offset process merge in
              (fact,Bytes_Address(blkOpt,offset'))
        | Bytes_Op(op,lst) -> 
            let (fact,lst') = traverse_bytes_list lst process merge in
              (fact,Bytes_Op(op,lst'))
        | Bytes_Read (content,offset,size) -> 
            let (fact1,content') = traverse_bytes content process merge in
            let (fact2,offset') = traverse_bytes offset process merge in
              (merge [fact1;fact2],Bytes_Read(content',offset',size))
        | Bytes_Write(oldbytes,offset,size,newbytes) -> 
            let (fact1,oldbytes') = traverse_bytes oldbytes process merge in
            let (fact2,offset') = traverse_bytes offset process merge in
            let (fact3,newbytes') = traverse_bytes newbytes process merge in
              (merge [fact1;fact2;fact3],Bytes_Write(oldbytes',offset',size,newbytes'))
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
                    ((s::mfrom,new_s::mto),(Byte_Symbolic(new_s))::lst)
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
                           if (s==h_from) then (true,(Byte_Symbolic(h_to))::lst)
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

(** map address to state (!) *)
let index_to_state: state Utility.IndexMap.t ref = ref (Utility.IndexMap.empty);;
let index_to_state__add index state = 
	index_to_state := Utility.IndexMap.add index state (!index_to_state)
;;
let index_to_state__get index = 
	Utility.IndexMap.find index (!index_to_state)
;;


(** Compare two states *)
let cmp_states (s1:state) (s2:state) =
	(* Compare blocks (memory allocations) that both states have *)
	let cmpSharedBlocks b2b1 b2b2 =
		let f block bytes1 str =
			let typ = block.memory_block_type in
			if typ!=Block_type_Global && typ!=Block_type_Heap then str else (* only care about globals and heap content *)
	          try
	    		let bytes2 = MemoryBlockMap.find block b2b2 in
	    		if  diff_bytes bytes1 bytes2 
	    		then
	    			let output1 = Printf.sprintf " >> %s = %s\n" (block.memory_block_name) (To_string.bytes bytes1) in
	    			let output2 = Printf.sprintf " << %s = %s\n" (block.memory_block_name) (To_string.bytes bytes2) in
	    				 str^(output1^output2)
	    		else
				str
	          with Not_found -> str
		in
		MemoryBlockMap.fold f b2b1 ""		
	in
	(* List blocks (memory allocations) that only one of the states has *)
	let cmpUnsharedBlocks b2b1 b2b2 =
	  let h prefix b2b block bytes str =
			let typ = block.memory_block_type in
			if typ!=Block_type_Global && typ!=Block_type_Heap then str else (* only care about globals and heap content *)
	        if MemoryBlockMap.mem block b2b then str else
	    	let output = Printf.sprintf " %s %s = %s\n" prefix (block.memory_block_name) (To_string.bytes bytes) in
	            str^output
	      in
	        (MemoryBlockMap.fold (h "(>>)" b2b2) b2b1 "")^
	        (MemoryBlockMap.fold (h "(<<)" b2b1) b2b2 "")
	in
	let rec cmpCallStack cs1 cs2 =
		match cs1,cs2 with
			| [],[] -> ""
			| h1::cs1',h2::cs2' when h1==h2 -> cmpCallStack cs1' cs2'
			| _ -> "Call stacks unequal"
	in
		(cmpSharedBlocks s1.block_to_bytes s2.block_to_bytes)^
		(cmpUnsharedBlocks s1.block_to_bytes s2.block_to_bytes)^
		(cmpCallStack s1.callstack s2.callstack)
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
