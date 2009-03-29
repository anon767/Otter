open Types
open Cil

type truth = True | False | Unknown;;

let not truth = match truth with
	| True -> False
	| False -> True
	| Unknown -> Unknown
;;

let rec eval pc bytes =
	(*
	if not Executeargs.args.Executeargs.arg_print_queries then () else
	Output.print_endline ("Is the following not equal to zero? \n"^(To_string.bytes bytes));*)
	let nontrivial () = 
			ignore (Utility.next_id Types.stp_count);
			Output.set_mode Output.MSG_REG;
			Output.print_endline "Ask STP...";
			Stats.time "STP" (consult_stp pc) bytes
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
		| OP_LT -> Operation.lt
		| OP_GT -> Operation.gt
		| OP_LE -> Operation.le
		| OP_GE -> Operation.ge
		| OP_EQ -> Operation.eq
		| OP_NE -> Operation.ne
		| _ -> failwith "operation_of: operation is not comparison"
	in
	match bytes with
		(* The following cases are simple enough to not consult STP *)
		| Bytes_Constant (CInt64(n,_,_)) -> if n = 0L then False else True			
		| Bytes_ByteArray (bytearray) ->
				begin try
					let b = Convert.bytes_to_bool bytes in  (* TODO:need to use int64 *)
						if b = false then False else True
				with Failure(_) -> nontrivial()
				end
		| Bytes_Address (None, offset) -> 
			eval pc offset
		| Bytes_Address (Some(_),_) -> True
		(* nullity check *)
		| Bytes_Op(OP_LNOT,(b1,_)::[]) -> not (eval pc b1)
		
		(* Comparison of (ptr+i) and (ptr+j) *)
		| Bytes_Op(op,(Bytes_Address(Some(block1),offset1),_)::(Bytes_Address(Some(block2),offset2),_)::[]) 
			when is_comparison op ->
				if block1!=block2 then (if op==OP_EQ then False else if op==OP_NE then True else Unknown)
				else  eval pc (Operation.run (operation_of op) [(offset1,Cil.intType);(offset2,Cil.intType)])
		
		(* Comparison of (ptr+i) and c (usually zero) *)
		| Bytes_Op(op,(Bytes_Address(blockopt,offset1),_)::(bytes2,_)::[]) 
			when is_comparison op  &&  Convert.isConcrete_bytes bytes2 ->
				begin match blockopt with 
					| None -> eval pc (Operation.run (operation_of op) [(offset1,Cil.intType);(bytes2,Cil.intType)])
					| Some(_) -> (if op==OP_EQ then False else if op==OP_NE then True else Unknown)
				end
		(* Function pointer is always true *)
		| Bytes_FunPtr(_,_) -> True
		(* Consult STP *)
		| _ -> 
			nontrivial()


						
and

consult_stp pc bytes =
    let vc = doassert pc in
	let equal_zero = query vc bytes true in
	if equal_zero then False else
		let not_equal_zero = query vc bytes false in
		if not_equal_zero then True else
			Unknown
	
and

(** return (True) False if bytes (not) evaluates to all zeros. Unknown otherwise.
 *) 


doassert pc =
(*
	let rec getRelevantAssumptions acc symbols pc' =
		match List.partition
			(fun b -> SymbolSet.is_empty (SymbolSet.inter symbols (allSymbols b)))
			pc'
		with
			| [],_ -> pc (* Everything is relevant *)
			| _,[] -> acc (* Nothing else is relevant *)
			| subPC,relevant ->
					getRelevantAssumptions (relevant @ acc)
						(SymbolSet.union symbols (allSymbolsList relevant))
						subPC
	in
	let relevantAssumptions =
		getRelevantAssumptions [] (allSymbols bytes) pc
	in
*)
	let vc = Stpc.create_validity_checker () in
    (*Stpc.e_push vc;*)
	
	Output.set_mode Output.MSG_STP;
	Output.print_endline "%%%%%%%%%%%%%%%%%%";
	Output.print_endline "%% STP Program: %%";
	Output.print_endline "%%%%%%%%%%%%%%%%%%";
		
	let rec do_assert pc = match pc with
		| [] -> ()
		| head::tail -> 
			let (bv, len) = to_stp_bv vc head in
			let a = Stpc.e_not vc (Stpc.e_eq vc bv (Stpc.e_bv_of_int vc len 0)) in
			Stpc.do_assert vc a;
			Output.set_mode Output.MSG_STP;
			Output.print_endline ("ASSERT("^(Stpc.to_string a)^");");
			do_assert tail
	in
		Stats.time "STP assert" do_assert pc ;	
(*		Stats.time "STP assert" do_assert relevantAssumptions;*)	
    vc

and

query vc bytes equal_zero =
    Stpc.e_push vc;
		
	let (bv, len) = Stats.time "convert conditional" (to_stp_bv vc) bytes in
	let q = Stpc.e_eq vc bv (Stpc.e_bv_of_int vc len 0) in
	let q = if equal_zero then q else Stpc.e_not vc q in
	
	Output.set_mode Output.MSG_STP;
	Output.print_endline ("QUERY("^(Stpc.to_string q)^");");
(*	
	let bool = Stpc.query vc q  in
		bool
*)
	let return = Stats.time "STP query" (Stpc.query vc) q in
      Stpc.e_pop vc;
      return

(* (* This commented-out chunk is trying to merge query and consult_stp.
	 Unfortunately, STP hangs if I try to make more than one query with
	 the same vc. Why? *)
	print_endline "Trying to prove it false";
	if Stpc.query vc q then (* Ask if [bytes] *must* be false. *)
		False (* If so, return False *)
	else (
		let q = (Stpc.e_not vc q) in
		Output.print_endline ("QUERY("^(Stpc.to_string q)^");");
		print_endline "Now trying to prove it true";
		if Stpc.query vc q then (* Otherwise, ask if [bytes] *must* be true. *)
			True (* If so, return True *)
		else
			Unknown (* [bytes] might be true and might be false *)
	)
*)
and


(** return (bv,len)
    bv: a 1-d bitvector that describe the bytes
		len: length (bits) of bv
		
		note: e_bv_of_int (with) (content)  -> bv
 *)
to_stp_bv vc bytes =
	match bytes with
		| Bytes_Constant (constant) ->
			let bytes2 = Convert.constant_to_bytes constant in
			to_stp_bv vc bytes2
		| Bytes_ByteArray (bytearray) ->
				let len = ImmutableArray.length bytearray in
				let bv8 = begin match ImmutableArray.get bytearray 0 with
					|	Byte_Concrete(c) -> Stpc.e_bv_of_int vc 8 (Char.code c)
					|	Byte_Symbolic(s) -> (* Here is where we catch attempts to use the undefined symbolic byte *)
							if s.symbol_id = 0 then failwith "Conditional depends on undefined value"
							else
								Stpc.e_var vc (make_var s) (Stpc.bitvector_t vc 8)
					| Byte_Bytes(b,i) -> 
						let (bv_condensed,l_condensed) = to_stp_bv vc b in
						(*let right_i = l_condensed * 8 in*)
						let right_i = i * 8 in
						let left_i = right_i+7 in
							Stpc.e_bvextract vc bv_condensed left_i right_i 
				end in
				if len = 1 then
					(bv8, 8)
				else
					let (bv, l) = to_stp_bv vc (Bytes_ByteArray (ImmutableArray.sub bytearray 1 (len - 1))) in
					(*(Stpc.e_bvconcat vc bv bv8, l + 8) (* little endian *)*)
					(Stpc.e_bvconcat vc bv8 bv, l + 8) (* big endian *)
 
		| Bytes_Address (blockopt, offset) ->
			let (bv_offset,l_offset) = to_stp_bv vc offset in
			let (bv_blockaddr,l_blockaddr) = 
			match blockopt with
				| None -> 
					(Stpc.e_bv_of_int vc 32 0, 32)
				| Some (block) -> 
					(*
					Output.set_mode Output.MSG_DEBUG;
					Output.print_endline ("STP encodes addr of "^ block.memory_block_name ^ " to :"^(To_string.bytes block.memory_block_addr));
					*)
					to_stp_bv vc block.memory_block_addr 
			in
			let len = l_blockaddr in
				(Stpc.e_bvplus vc len bv_blockaddr bv_offset,len)
			
		| Bytes_Op(op, [(bytes1,typ1);(bytes2,typ2)]) -> (* BINOP *)
				(* typ info maybe added to the stp formula later *)
				let (bv1, len1) = to_stp_bv vc bytes1 in
				let (bv2, len2) = to_stp_bv vc bytes2 in
				let len_of_1_0 = 32 in
				let bv_1 = (Stpc.e_bv_of_int vc len_of_1_0 1) in
				let bv_0 = (Stpc.e_bv_of_int vc len_of_1_0 0) in
				let isSigned = match Cilutility.unrollType typ1 with
					| TInt (ikind,_) -> Cil.isSigned ikind
					| _ -> false
				in
				let op_func = fun bv1 len1 bv2 len2 ->
					begin match op with
						| OP_PLUS ->	(Stpc.e_bvplus vc len1 bv1 bv2, len1)
						| OP_SUB -> (Stpc.e_bvminus vc len1 bv1 bv2, len1)
						| OP_MULT ->	(Stpc.e_bvmult vc len1 bv1 bv2, len1)
						| OP_DIV -> (Stpc.e_bvdiv vc len1 bv1 bv2, len1) (* TODO: add sign support *)
						| OP_MOD -> (Stpc.e_bvmod vc len1 bv1 bv2, len1)
						(* for left shift, many need to resize the len *)
(*						| OP_LSL -> (Stpc.e_bvextract vc (Stpc.e_bvshiftleft vc len1 bv1 len2 bv2) (len1-1) 0, len1)				*)
						| OP_LSL -> (Stpc.e_bvshiftleft vc len1 bv1 len2 bv2, len1)				
						| OP_LSR -> (Stpc.e_bvshiftright vc len1 bv1 len2 bv2, len1)		
						
						| OP_LT -> (Stpc.e_ite vc (Stpc.e_bvlt isSigned vc len1 bv1 bv2) bv_1 bv_0, len_of_1_0)
						| OP_GT -> (Stpc.e_ite vc (Stpc.e_bvgt isSigned vc len1 bv1 bv2) bv_1 bv_0, len_of_1_0)
						| OP_LE -> (Stpc.e_ite vc (Stpc.e_bvle isSigned vc len1 bv1 bv2) bv_1 bv_0, len_of_1_0)
						| OP_GE -> (Stpc.e_ite vc (Stpc.e_bvge isSigned vc len1 bv1 bv2) bv_1 bv_0, len_of_1_0)
						
						| OP_EQ -> (Stpc.e_ite vc (Stpc.e_eq vc bv1 bv2) bv_1 bv_0, len_of_1_0)
						| OP_NE -> (Stpc.e_ite vc (Stpc.e_eq vc bv1 bv2) bv_0 bv_1, len_of_1_0)
						| OP_BAND -> (Stpc.e_bvand vc bv1 bv2, len1)
						| OP_BXOR -> (Stpc.e_bvxor vc bv1 bv2, len1)
						| OP_BOR ->  (Stpc.e_bvor vc bv1 bv2, len1)
						| OP_LAND -> 
							(Stpc.e_ite vc 
								(Stpc.e_or vc 
									(Stpc.e_eq vc bv1 (Stpc.e_bv_of_int vc len1 0)) 
									(Stpc.e_eq vc bv2 (Stpc.e_bv_of_int vc len2 0))
								) 
							bv_0 bv_1 , len_of_1_0)
						| OP_LOR ->
							(Stpc.e_ite vc 
								(Stpc.e_and vc 
									(Stpc.e_eq vc bv1 (Stpc.e_bv_of_int vc len1 0)) 
									(Stpc.e_eq vc bv2 (Stpc.e_bv_of_int vc len2 0))
								) 
							bv_0 bv_1 , len_of_1_0)
						| OP_SX -> (* here bv2 must be constant *)
							failwith "not implemented"
						| _ -> failwith ((To_string.operation op) ^ " is not a binary operator")
					end in
					op_func bv1 len1 bv2 len2
		| Bytes_Op(op, [(bytes1,typ1)]) -> (* UNOP *)
				let (bv1, len1) = to_stp_bv vc bytes1 in
				let len_of_1_0 = 32 in
				let bv_1 = (Stpc.e_bv_of_int vc len_of_1_0 1) in
				let bv_0 = (Stpc.e_bv_of_int vc len_of_1_0 0) in
				let op_func = fun bv1 len1 ->
					begin match op with 
						| OP_UMINUS -> (Stpc.e_bvneg vc bv1, len1)
						| OP_BNOT ->(Stpc.e_bvnot vc bv1, len1)
						| OP_LNOT ->
							(Stpc.e_ite vc 
								(Stpc.e_not vc 
									(Stpc.e_eq vc bv1 (Stpc.e_bv_of_int vc len1 0)) 
								) 
							bv_0 bv_1 , len_of_1_0)
						| _ -> failwith ((To_string.operation op) ^ " is not a unary operator")
					end in
					op_func bv1 len1 
		| Bytes_Op(OP_LAND, bytesTypList) -> (* Let AND be variadic *)
				let bvLenList =
					List.map (fun (bytes,_) -> to_stp_bv vc bytes) bytesTypList
				in
				let len_of_1_0 = 32 in
				let bv_1 = (Stpc.e_bv_of_int vc len_of_1_0 1) in
				let bv_0 = (Stpc.e_bv_of_int vc len_of_1_0 0) in
				(* If any is false, then 0; otherwise 1. *)
				(Stpc.e_ite vc
					 (List.fold_left
							(fun expr (bv,len) ->
								 Stpc.e_or vc expr
									 (Stpc.e_cfalse vc len bv))
							(Stpc.e_false vc)
							bvLenList)
					 bv_0 bv_1 , len_of_1_0)
		| Bytes_Op(op, _) ->
				failwith ("Invalid number of operands for " ^ (To_string.operation op))

		| Bytes_Read (content,offset,len) ->
			let arr = new_array vc content in
			let array_content = to_stp_array vc arr content in
			let (bv_offset,len_offset) = to_stp_bv vc offset in (* assert(len_offset=32) *)
			let rec read bv_offset len =
				if len < 1 then failwith "Bytes_Read len < 1" 
				else if len = 1 then (Stpc.e_read vc array_content bv_offset,8) 
				else
					let (bv_head,len_head) = (Stpc.e_read vc array_content bv_offset,8)  in
					let (bv_tail,len_tail) = read (Stpc.e_bvplus vc 32 bv_offset (Stpc.e_bv_of_int vc 32 1)) (len-1) in
					(Stpc.e_bvconcat vc bv_head bv_tail,len_head+len_tail) 
			in
				read bv_offset len 
				
		| Bytes_FunPtr (fundec,f_addr) ->
			(*
					Output.set_mode Output.MSG_DEBUG;
					Output.print_endline ("STP encodes addr of "^ (To_string.fundec fundec) ^ " to :"^(To_string.bytes f_addr));
*)
			to_stp_bv vc f_addr
		
		| _ ->
			let len = MemOp.bytes__length bytes in
			let arr = to_stp_array vc (new_array vc bytes) bytes in
			let rec flatten bv_offset len =
			  if len = 1 then (Stpc.e_read vc arr bv_offset,8) 
				else
					let (bv_head,len_head) = (Stpc.e_read vc arr bv_offset,8)  in
					let (bv_tail,len_tail) = flatten (Stpc.e_bvplus vc 32 bv_offset (Stpc.e_bv_of_int vc 32 1)) (len-1) in
					(Stpc.e_bvconcat vc bv_head bv_tail,len_head+len_tail) 
			in
				flatten (Stpc.e_bv_of_int vc 32 0) len 			
						
and

to_stp_array vc arr bytes =	
	match bytes with
		| Bytes_Constant(constant) ->
			let bytes2 = Convert.constant_to_bytes constant in
				to_stp_array vc arr bytes2
		| Bytes_ByteArray(bytearray)->

			let len = ImmutableArray.length bytearray in
			
				(*	Output.print_endline ("bytearray "^(string_of_int len)); *)

				let bv8 = begin match ImmutableArray.get bytearray (len-1) with
					|	Byte_Concrete(c) -> Stpc.e_bv_of_int vc 8 (Char.code c)
					|	Byte_Symbolic(s) -> (* Here is where we catch attempts to use the undefined symbolic byte *)
							if s.symbol_id = 0 then failwith "Conditional depends on undefined value"
							else
								Stpc.e_var vc (make_var s) (Stpc.bitvector_t vc 8)
					| Byte_Bytes(b,i) -> 
						let (bv_condensed,l_condensed) = to_stp_bv vc b in
						let right_i = l_condensed * 8 in
						let left_i = right_i+7 in
							Stpc.e_bvextract vc bv_condensed left_i right_i 
				end in
				if len = 1 then
					Stpc.e_write vc arr (Stpc.e_bv_of_int vc 32 0) bv8
				else
					let arr2 = to_stp_array vc arr (Bytes_ByteArray (ImmutableArray.sub bytearray 0 (len - 1))) in
						Stpc.e_write vc arr2 (Stpc.e_bv_of_int vc 32 (len-1)) bv8
		
		| Bytes_Read (content,offset,len) -> 
			let array_content = to_stp_array vc (new_array vc content) content in
			let (bv_offset,len_offset) = to_stp_bv vc offset in 

			let rec read bv_offset len array =
				if len < 1 then failwith "Bytes_Read len < 1" 
				else if len = 1 then Stpc.e_write vc array (Stpc.e_bv_of_int vc 32 0) (Stpc.e_read vc array_content bv_offset)
				else
					let array2 = read bv_offset (len-1) array in
					let bv_offset2 = Stpc.e_bv_of_int vc 32 (len-1) in
						Stpc.e_write vc array2 bv_offset2 (Stpc.e_read vc array_content (Stpc.e_bvplus vc 32 bv_offset bv_offset2))
			in
				read bv_offset len (new_array vc bytes)			
				
		| Bytes_Write (content,offset,len,newbytes) -> 
			let (bv_offset,len_offset) = to_stp_bv vc offset in 
			let array_source = to_stp_array vc (new_array vc newbytes) newbytes in
			let rec write array_target bv_offset len  =
				if len < 1 then failwith "Bytes_Read len < 1" 
				else if len = 1 then Stpc.e_write vc array_target bv_offset (Stpc.e_read vc array_source (Stpc.e_bv_of_int vc 32 0))
				else
					let array_target2 = write array_target bv_offset (len-1)  in
					let bv_offset2 = Stpc.e_bv_of_int vc 32 (len-1) in
						Stpc.e_write vc array_target2 (Stpc.e_bvplus vc 32 bv_offset bv_offset2) (Stpc.e_read vc array_source bv_offset2)
			in
				write (to_stp_array vc  (new_array vc content) content) bv_offset len 			
					
		| _ -> 
			let (bv,len) = to_stp_bv vc bytes in
			let rec write array arr_len index = (* arr[index:(index-7)] *)
				if index<0 then array 
				else
					let array2 = Stpc.e_write vc array (Stpc.e_bv_of_int vc 32 arr_len) (Stpc.e_bvextract vc bv index (index-7)) in
						write array2 (arr_len+1) (index-8)	
			in
				write (new_array vc bytes) 0 (len-1)
			
			
and

new_array vc bytes = 
	let name = 
		("bytes_"^(string_of_int (Hashtbl.hash bytes))) (* may clash *)
	in
	let arr_typ = Stpc.array_t vc (Stpc.bitvector_t vc 32) (Stpc.bitvector_t vc 8) in
		Stpc.e_var vc name arr_typ
		
and

make_var symbol =
	"symbol_"^(string_of_int symbol.symbol_id)
;;

(** Given a path condition and a list of symbols, return an
		association list of (symbol,char)s, where each char is the value
		STP gives to that symbol to make the path condition true. *)
let getValues pathCondition symbolList =
	let vc = Stpc.create_validity_checker () in
	(* To get values for the symbolic values which make the path
		 condition true, we need to query for its *negation* because STP
		 gives counterexamples (not satisfying assignments).
		 The path condition is a list which is to be interpreted as a
		 conjunction:
		 A /\ B /\ C
		 which really means
		 (A != 0) /\ (B != 0) /\ (C != 0)
		 The negation of that is
		 (A == 0) \/ (B == 0) \/ (C == 0)
		 which is the formula we ask STP about. (Actually, we add an
		 '\/ false' to the formula as the base case in the fold_left.
		 Think about it.) *)
	let negatedPcExpr =
		List.fold_left
			(fun expr bytes ->
				 let (bv, len) = to_stp_bv vc bytes in
				 Stpc.e_or vc expr
					 (Stpc.e_cfalse vc len bv))
			(Stpc.e_false vc)
			pathCondition
	in
	if Stpc.query vc negatedPcExpr
	then failwith ("The path condition is unsatisfiable!\n" ^
									 (To_string.bytes_list pathCondition));
	(* Extract the value of a symbol from STP's counterexample *)
	let getOneVal s =
		let bv = Stpc.e_var vc (make_var s) (Stpc.bitvector_t vc 8) in
		(s, Char.chr (Stpc.int_of_e (Stpc.get_counterexample vc bv)))
	in
	List.map getOneVal symbolList
;;
