open Types
open Cil

type truth = True | False | Unknown;;

let not truth = match truth with
	| True -> False
	| False -> True
	| Unknown -> Unknown
;;

let rec bytes_length bytes =
	match bytes with
		| Bytes_Constant (constant) -> (Cil.bitsSizeOf (Cil.typeOf (Const(constant))))/8
		| Bytes_ByteArray (bytearray) -> ImmutableArray.length bytearray
		| Bytes_Address (_,_)-> word__size
		| Bytes_MayBytes (_,b,_) -> bytes_length b (* all bytes in MayBytes have the same length *)
		| Bytes_Op (op,(bytes2,typ)::tail) -> bytes_length bytes2
		| Bytes_Op (op,[]) -> 0 (* reachable from diff_bytes *)
		| Bytes_Write(bytes2,_,_,_) -> bytes_length bytes2
		| Bytes_Read(_,_,len) -> len
		| Bytes_FunPtr(_) -> word__size
;;

(** Return a SymbolSet of all symbols in the given Bytes *)
let rec allSymbols = function
	| Bytes_Constant const -> SymbolSet.empty
	| Bytes_ByteArray bytearray ->
			ImmutableArray.fold_left
				(fun symbSet byte -> match byte with
					 | Byte_Concrete _ -> symbSet
					 | Byte_Symbolic symb -> SymbolSet.add symb symbSet
					 | Byte_Bytes (bytes,_) -> SymbolSet.union symbSet (allSymbols bytes))
				SymbolSet.empty
				bytearray
	| Bytes_Address (memBlockOpt,bytes) -> (
			let partialAnswer = allSymbols bytes in
			match memBlockOpt with
					None -> partialAnswer
				| Some memBlock ->
						SymbolSet.union partialAnswer (allSymbols memBlock.memory_block_addr)
		)
	| Bytes_MayBytes (_, bytes1, bytes2) ->
			SymbolSet.union (allSymbols bytes1) (allSymbols bytes2)
	| Bytes_Op (_,bytes_typ_list) ->
			List.fold_left
				(fun symbSet (b,_) -> SymbolSet.union symbSet (allSymbols b))
				SymbolSet.empty
				bytes_typ_list
	| Bytes_Read (bytes1,bytes2,_) ->
			SymbolSet.union (allSymbols bytes1) (allSymbols bytes2)
	| Bytes_Write (bytes1,bytes2,_,bytes3) ->
			SymbolSet.union
				(allSymbols bytes3)
				(SymbolSet.union (allSymbols bytes1) (allSymbols bytes2))
	| Bytes_FunPtr (_,bytes) -> allSymbols bytes

(** Return a SymbolSet of all symbols in the given list of Bytes *)
let allSymbolsInList byteslist =
	List.fold_left
		(fun symbSet b -> SymbolSet.union symbSet (allSymbols b))
		SymbolSet.empty
		byteslist

type symbolOrIndicator = Symb of symbol | Indic of int

module SymbolAndIndicatorSet = Set.Make
	(struct
		 type t = symbolOrIndicator
		 (* I arbitrarily chose to make symbols less than indicators *)
		 let compare x y = match x,y with
			 | Symb s1, Symb s2 -> Pervasives.compare s1.symbol_id s2.symbol_id
			 | Indic i1, Indic i2 -> Pervasives.compare i1 i2
			 | Symb _, Indic _ -> -1
			 | Indic _, Symb _ -> 1
	 end)
(* Abbreviating the name *)
module SISet = SymbolAndIndicatorSet

let rec allIndicators = function
	| Indicator i -> SISet.singleton (Indic i)
	| Indicator_Not i -> allIndicators i
	| Indicator_And (i1,i2) ->
			SISet.union (allIndicators i1) (allIndicators i2)

(** Return an SISet of all symbols and indicators in the given Bytes *)
let rec allSymbolsAndIndicators = function
	| Bytes_Constant const -> SISet.empty
	| Bytes_ByteArray bytearray ->
			ImmutableArray.fold_left
				(fun symbSet byte -> match byte with
					 | Byte_Concrete _ -> symbSet
					 | Byte_Symbolic symb ->
							 SISet.add (Symb symb) symbSet
					 | Byte_Bytes (bytes,_) ->
							 SISet.union symbSet (allSymbolsAndIndicators bytes))
				SISet.empty
				bytearray
	| Bytes_Address (memBlockOpt,bytes) -> (
			let partialAnswer = allSymbolsAndIndicators bytes in
			match memBlockOpt with
					None -> partialAnswer
				| Some memBlock ->
						SISet.union partialAnswer
							(allSymbolsAndIndicators memBlock.memory_block_addr)
		)
	| Bytes_MayBytes (indicator, bytes1, bytes2) ->
			SISet.union (allIndicators indicator)
				(SISet.union (allSymbolsAndIndicators bytes1)
					 (allSymbolsAndIndicators bytes2))
	| Bytes_Op (_,bytes_typ_list) ->
			List.fold_left
				(fun symbSet (b,_) -> SISet.union symbSet (allSymbolsAndIndicators b))
				SISet.empty
				bytes_typ_list
	| Bytes_Read (bytes1,bytes2,_) ->
			SISet.union (allSymbolsAndIndicators bytes1) (allSymbolsAndIndicators bytes2)
	| Bytes_Write (bytes1,bytes2,_,bytes3) ->
			SISet.union
				(allSymbolsAndIndicators bytes3)
				(SISet.union (allSymbolsAndIndicators bytes1) (allSymbolsAndIndicators bytes2))
	| Bytes_FunPtr (_,bytes) -> allSymbolsAndIndicators bytes

(** Return a SISet of all symbols in the given list of Bytes *)
let allSymbolsAndIndicatorsInList byteslist =
	List.fold_left
		(fun symbSet b -> SISet.union symbSet (allSymbolsAndIndicators b))
		SISet.empty
		byteslist

(* This implements 'constraint independence': it picks out only those
	 clauses from the pc which can have an influence on the given
	 symbols. It grabs the clauses that mention any of the symbols, and
	 then iterates with the symbols present in these clauses, and so on,
	 until nothing else mentions any of the symbols we care about. (This
	 will happen eventually, because eventually pc will become empty.) *)
let rec getRelevantAssumptions_aux acc symbols pc =
	match List.partition (* See what clauses mention any of the symbols *)
		(fun b -> SISet.is_empty (SISet.inter symbols (allSymbolsAndIndicators b)))
		pc
	with
		| _,[] -> acc (* Nothing else is relevant *)
		| subPC,relevant ->
				getRelevantAssumptions_aux
					(List.rev_append relevant acc) (* Does using rev_append, rather than (@), mess with the order in a way that's bad for the cache? I think not, but I'm not sure. *)
					(allSymbolsAndIndicatorsInList relevant)
					subPC

let getRelevantAssumptions pc query =
	getRelevantAssumptions_aux [] (allSymbolsAndIndicators query) pc

let rec listCompare l1 l2 =
	match l1,l2 with
			_ when l1 == l2 -> 0 (* This includes [],[] *)
		| [],_ -> -1
		| _,[] -> 1
 		| h1::t1,h2::t2 ->
				if h1 == h2 then compare t1 t2
				else let x = compare h1 h2 in
				if x <> 0 then x else compare t1 t2

(* Map a pc and a query *)
module StpCache = Map.Make
	(struct
		 type t = bytes list * bytes (* pc, query *)
		 (* It might be better to have this be set-based equality rather
				than list-based. *)
		 let compare ((pc1,cov1):t) ((pc2,cov2):t) = (* Type annotation to remove polymorphism *)
			 let result1 = listCompare pc1 pc2 in
			 if result1 = 0
			 then compare cov1 cov2
			 else result1
	 end)

let cacheHits = ref 0
let cacheMisses = ref 0
let stpCacheRef = ref StpCache.empty

let rec eval pc bytes =
	(*
	if not Executeargs.args.Executeargs.arg_print_queries then () else
	Output.print_endline ("Is the following not equal to zero? \n"^(To_string.bytes bytes));*)
	let nontrivial () = 
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

(* TODO: refactor consult_stp/doassert/query/query_indicator, since query_indicator similar in purpose to consult_stp *)
consult_stp pc bytes =
	let pc = getRelevantAssumptions pc bytes in
	try
		let ans = StpCache.find (pc,bytes) !stpCacheRef in
		incr cacheHits;
		ans
	with Not_found ->
		incr cacheMisses;
    let vc = doassert pc in
		let answer =
			if query vc bytes true then
				False
			else if query vc bytes false then
				True
			else
				Unknown
		in
		stpCacheRef := StpCache.add (pc,bytes) answer !stpCacheRef;
		answer

and

(** return (True) False if bytes (not) evaluates to all zeros. Unknown otherwise.
 *) 


doassert pc =
	let vc = Stpc.create_validity_checker () in
	
	Output.set_mode Output.MSG_STP;
	Output.print_endline "%%%%%%%%%%%%%%%%%%";
	Output.print_endline "%% STP Program: %%";
	Output.print_endline "%%%%%%%%%%%%%%%%%%";
		
	let rec do_assert pc = match pc with
		| [] -> ()
		| head::tail -> 
			let (bv, len) = to_stp_bv vc head in
			Stpc.assert_ctrue vc len bv;
			Output.set_mode Output.MSG_STP;
			Output.print_endline ("ASSERT("^(Stpc.to_string bv)^"!=0);");
			do_assert tail
	in
	(*Stats.time "STP assert" do_assert relevantAssumptions;*)
	Stats.time "STP assert" do_assert pc;
	vc
and

query vc bytes equal_zero =
    Stpc.e_push vc;
		
	let (bv, len) = Stats.time "convert conditional" (to_stp_bv vc) bytes in
	let q = Stpc.e_eq vc bv (Stpc.e_bv_of_int vc len 0) in
	let q = if equal_zero then q else Stpc.e_not vc q in
	
	Output.set_mode Output.MSG_STP;
	Output.print_endline ("QUERY("^(Stpc.to_string q)^");");
	incr Types.stp_count;
	let return = Stats.time "STP query" (Stpc.query vc) q in
      Stpc.e_pop vc;
      return

and

query_indicator pc indicator =
    let vc = doassert pc in
	let indicator_exp = Stats.time "convert conditional" (to_stp_indicator vc) indicator in

	Output.set_mode Output.MSG_STP;
	let query exp =
		Stpc.e_push vc;
		let result = Stats.time "STP query_indicator" (Stpc.query vc) exp in
		Stpc.e_pop vc;
		result
	in
	if query indicator_exp then
		True
	else if query (Stpc.e_not vc indicator_exp) then
		False
	else
		Unknown
and

to_stp_indicator vc = function
	| Indicator i -> Stpc.e_var vc (Format.sprintf "indicator(%d)" i) (Stpc.bool_t vc)
	| Indicator_Not s -> Stpc.e_not vc (to_stp_indicator vc s)
	| Indicator_And (s1, s2) -> Stpc.e_and vc (to_stp_indicator vc s1) (to_stp_indicator vc s2)

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
					| Byte_Concrete(c) -> Stpc.e_bv_of_int vc 8 (Char.code c)
					| Byte_Symbolic(_) as b when b = byte__undef -> (* Here is where we catch attempts to use the undefined symbolic byte *)
						failwith "Conditional depends on undefined value"
					| Byte_Symbolic(s) ->
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

		| Bytes_MayBytes (indicator, bytes1, bytes2) ->
			(* TODO: should check for indicator.symbol_id = 0? *)
			let cond = to_stp_indicator vc indicator in
			let bv1, len1 = to_stp_bv vc bytes1 in
			let bv2, len2 = to_stp_bv vc bytes2 in
			assert (len1 = len2);
			(Stpc.e_ite vc cond bv1 bv2, len1)

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
			let len = bytes_length bytes in
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
					| Byte_Concrete(c) -> Stpc.e_bv_of_int vc 8 (Char.code c)
					| Byte_Symbolic(_) as b when b = byte__undef -> (* Here is where we catch attempts to use the undefined symbolic byte *)
						failwith "Conditional depends on undefined value"
					| Byte_Symbolic(s) ->
						Stpc.e_var vc (make_var s) (Stpc.bitvector_t vc 8)
					| Byte_Bytes(b,i) -> 
						let (bv_condensed,l_condensed) = to_stp_bv vc b in
						let right_i = l_condensed * 8 in (* This differs from the parallel case in to_stp_bv, and is independent of i. How can this be right? *)
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
