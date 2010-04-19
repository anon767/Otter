(**
	This module should define all functions of type operator_action.
	E.g. 
	let plus (operands: (bytes*typ) list) : bytes =
		... 
 *)
open Cil
open Bytes
open Ternary

let run op operands = op operands;;


let unop op_conc (*bytearray->bytearray*) op_symb operands : bytes  = 
	let (bytes1, typ1) = List.nth operands 0 in
	let rec impl bytes typ =
		match bytes with
			| Bytes_Constant(const) -> impl (constant_to_bytes const) typ
			| Bytes_ByteArray(bytearray) -> 
				if isConcrete_bytearray bytearray 
				then
					make_Bytes_ByteArray(op_conc bytearray typ)
				else 
					make_Bytes_Op(op_symb,operands)
			| _ -> (make_Bytes_Op(op_symb, operands))
	in 
		impl bytes1 typ1
;;

let neg operands =
	let op_conc arr typ = 
		(* Negation is flipping the bits and adding 1. To implement this,
			 keep track of [carry], the value to add to each byte. We have
			 to add 1 to the lowest byte (which is first because we are
			 little-endian), and we must continue carrying as long as the
			 original byte was '\000'. Once we hit a non-null byte, we stop
			 carrying. *)
		let carry = ref 1 in
		ImmutableArray.map (fun byte -> match byte with
			| Byte_Concrete (c) ->
				let c' = Char.chr ( (!carry + (lnot (Char.code c))) land 255) in
					if c <> '\000' then carry := 0;
					make_Byte_Concrete(c')
			| _ -> failwith "neg: unreachable"
		) arr 
	in
	unop op_conc OP_UMINUS operands 
;;

let bnot operands =
	let op_conc arr typ = 
		ImmutableArray.map (fun byte -> match byte with
			| Byte_Concrete (c) ->
				let c' = Char.chr ( (lnot (Char.code c)) land 255)  in
					make_Byte_Concrete(c')
			| _ -> failwith "bnot: unreachable"
		) arr 
	in
	unop op_conc OP_BNOT operands 
;;

let lnot operands = (* should return int (32-bit) *)
	let op_conc arr typ = 
		if ImmutableArray.fold_left (fun a byte -> match byte with
			| Byte_Concrete (c) -> a || (Char.code c <>0)
			| _ -> failwith "lnot: unreachable"
		) false arr 
		then (* 0 *)
			ImmutableArray.of_list [make_Byte_Concrete('\000');make_Byte_Concrete('\000');make_Byte_Concrete('\000');make_Byte_Concrete('\000')]
		else (* 1 *)
			ImmutableArray.of_list [make_Byte_Concrete('\001');make_Byte_Concrete('\000');make_Byte_Concrete('\000');make_Byte_Concrete('\000')]
	in
	unop op_conc OP_LNOT operands ;;

let ikind_of_TInt = function
		TInt (ikind,_) -> ikind
	| t -> invalid_arg ("Trying to get ikind from something other than a TInt: " ^ (Pretty.sprint 50 (d_type()t)))

(* TODO: each op must also have typ of par as arg.

 *)
(* binop. suitable for binop with operands of equal type *)
let rec binop op_const op_symb operands : bytes (* * typ *)=
	let (bytes1, typ1) = List.nth operands 0 in
	let (bytes2, typ2) = List.nth operands 1 in

    let isReducableArithmetic opout opin = match opout,opin with
      | OP_PLUS,OP_PLUS | OP_PLUS,OP_SUB 
      | OP_SUB,OP_PLUS | OP_SUB,OP_SUB
      | OP_MULT,OP_MULT
      (*| OP_MULT,OP_PLUS | OP_MULT,OP_SUB*)
        -> true
      | _,_ -> false
    in 
    let rec atLeastOneConstant = function
      | [] -> false
      | (Bytes_Constant(_),_)::_ -> true
      | _::t -> atLeastOneConstant t
    in
    let rec 
    reducedArithmetic (b1:bytes) (op,args) normal =
	    let (rab1, rat1) = List.nth args 0 in
	    let (rab2, rat2) = List.nth args 1 in
			let size1 = sizeOf typ1 in
			assert(size1 = sizeOf typ2 && size1 = sizeOf rat1 && size1 = sizeOf rat2);
        match op_symb,op with   (* normal: b1 op_symb (rab1 op rab2) , reversed: (rab1 op rab2) op_symb b1  *)
          | OP_PLUS,OP_PLUS -> (* a+(b+c) *)
              begin
                match rab1,rab2 with
                  | Bytes_Constant(_),_ -> plus ((plus ((b1,typ1)::(rab1,rat1)::[]),typ1)::(rab2,rat2)::[])
                  | _,Bytes_Constant(_) -> plus ((plus ((b1,typ1)::(rab2,rat2)::[]),typ1)::(rab1,rat1)::[])
                  | _,_ -> failwith "unreachable"
              end
          | OP_PLUS,OP_SUB ->(* a+(b-c) *)
              begin
                match rab1,rab2 with
                  | Bytes_Constant(_),_ -> minus ((plus ((b1,typ1)::(rab1,rat1)::[]),typ1)::(rab2,rat2)::[])
                  | _,Bytes_Constant(_) -> minus ((rab1,rat1)::(minus ((rab2,rat2)::(b1,typ1)::[]),rat2)::[])
                  | _,_ -> failwith "unreachable"
              end
          | OP_SUB,OP_PLUS when normal ->(* a-(b+c) *)
              begin
                match rab1,rab2 with
                  | Bytes_Constant(_),_ -> minus ((minus ((b1,typ1)::(rab1,rat1)::[]),typ1)::(rab2,rat2)::[])
                  | _,Bytes_Constant(_) -> minus ((minus ((b1,typ1)::(rab2,rat2)::[]),typ1)::(rab1,rat1)::[]) 
                  | _,_ -> failwith "unreachable"
              end
          | OP_SUB,OP_SUB when normal ->(* a-(b-c) *)
              begin
                match rab1,rab2 with
                  | Bytes_Constant(_),_ -> plus ((minus ((b1,typ1)::(rab1,rat1)::[]),typ1)::(rab2,rat2)::[])
                  | _,Bytes_Constant(_) -> minus ((plus ((b1,typ1)::(rab2,rat2)::[]),typ1)::(rab1,rat1)::[]) 
                  | _,_ -> failwith "unreachable"
              end
          | OP_SUB,OP_PLUS ->(* (b+c)-a *)
              begin
                match rab1,rab2 with
                  | Bytes_Constant(_),_ -> plus ((minus ((rab1,rat1)::(b1,typ1)::[]),typ1)::(rab2,rat2)::[])
                  | _,Bytes_Constant(_) -> plus ((minus ((rab2,rat2)::(b1,typ1)::[]),typ1)::(rab1,rat1)::[]) 
                  | _,_ -> failwith "unreachable"
              end
          | OP_SUB,OP_SUB ->(* (b-c)-a *)
              begin
                match rab1,rab2 with
                  | Bytes_Constant(_),_ -> minus ((minus ((rab1,rat1)::(b1,typ1)::[]),typ1)::(rab2,rat2)::[])
                  | _,Bytes_Constant(_) -> minus ((rab1,rat1)::(plus ((rab2,rat2)::(b1,typ1)::[]),rat2)::[])
                  | _,_ -> failwith "unreachable"
              end
          | OP_MULT,OP_MULT ->(* a*(b*c) *)
              begin
                match rab1,rab2 with
                  | Bytes_Constant(_),_ -> mult ((mult ((b1,typ1)::(rab1,rat1)::[]),typ1)::(rab2,rat2)::[])
                  | _,Bytes_Constant(_) -> mult ((mult ((b1,typ1)::(rab2,rat2)::[]),typ1)::(rab1,rat1)::[])
                  | _,_ -> failwith "unreachable"
              end
          (* dunno if these are helpful *)
          (*| OP_MULT,OP_PLUS ->
              begin
                failwith "not yet implemented"
              end
          | OP_MULT,OP_SUB ->
              begin
                failwith "not yet implemented"
              end*)
          | _ -> failwith "unreachable"
    and
    worstCase b1 b2 = 
			if not (isConcrete_bytes b1 & isConcrete_bytes b2) then
				(make_Bytes_Op(op_symb, operands)) (* TODO: Check that STP treats Bytes_Ops as having the type of the first operand *)
			else
			let c1 = bytes_to_constant b1 typ1 in (*TODO: look at typ1 to see if it's unsigned *)
			let c2 = bytes_to_constant b2 typ2 in
			begin match (c1,c2) with
			| (CInt64(i1,k1,s1),CInt64(i2,k2,s2)) ->
				impl (make_Bytes_Constant c1, make_Bytes_Constant c2)
			| (CReal(i1,k1,s1),CReal(i2,k2,s2)) -> (*TMP*) bytes1
			| _ -> failwith "Match error"
			end
    and
	(* Maybe an error of ocaml: if ibytes? is replaced by bytes?, the scoping is messed up *)	
	impl (ibytes1,ibytes2) =
	match (ibytes1, ibytes2) with
		| (Bytes_Constant(CInt64(i1, _, _)), Bytes_Constant(CInt64(i2, _, _))) ->
				let kind1 = ikind_of_TInt typ1
				and kind2 = ikind_of_TInt typ2 in
				let isSigned = 	Cil.isSigned kind1 || Cil.isSigned kind2 in
				let n64 = op_const isSigned i1 i2 in
				let (n64,_) = Cil.truncateInteger64 kind1 n64 in
				(* Some operators always result in ints---namely, relational, equality,
					 and logical operators. For the others, the result's type is that of
					 the first operand,
					 either because both operands have the same type (and the
					 result should have that type, too), or because this is a
					 shift operation.
					 (See 6.3.1.8.1 and much of 6.5 in the Standard.) *)
				let resultType = if returnsBoolean op_symb then IInt else kind1 in
				let const = CInt64(n64, resultType, None) in
				(make_Bytes_Constant(const))
        | (Bytes_Constant(CInt64 _), Bytes_Op (op, args)) when ((isReducableArithmetic op_symb op) && (atLeastOneConstant args))
          -> reducedArithmetic ibytes1 (op,args) true
        | (Bytes_Op (op, args) , Bytes_Constant(CInt64 _)) when ((isReducableArithmetic op_symb op) && (atLeastOneConstant args))
          -> reducedArithmetic ibytes2 (op,args) false
		(* Allow a particular piece of pointer arithmetic: ptr % num. *)
		| Bytes_Address(blk, offset), op2
				when op_symb = OP_MOD &&
					isConcrete_bytes offset &&
					isConcrete_bytes op2 ->
				let offsetConstant = bytes_to_constant offset !Cil.upointType in
				let op2Constant = bytes_to_constant op2 !Cil.upointType in
				begin match offsetConstant,op2Constant with
					| CInt64 _,CInt64 _ ->
							let ptrAsNum = plus [(make_Bytes_Constant
																			(bytes_to_constant blk.memory_block_addr !Cil.upointType),
																		!Cil.upointType);
																	 (make_Bytes_Constant offsetConstant, typ2)]
							in
							impl (ptrAsNum, make_Bytes_Constant op2Constant)
					| _ -> failwith "Unimplemented pointer arithmetic operation"
				end

		| (b1,b2)  ->
            worstCase b1 b2
	in
		impl (bytes1,bytes2)

(* Fix sign problem! *)
and plus operands  = binop (fun s x y -> Int64.add x y) OP_PLUS operands
and minus operands = binop (fun s x y -> Int64.sub x y) OP_SUB operands	
and mult operands  = binop (fun s x y -> Int64.mul x y) OP_MULT operands 
;;


(*
let signextend operands = 
	let nativeop n1 n2 =
		n1
	in
		binop nativeop OP_SX operands 
;;*)

(* Fix sign problem! *)
let div operands   = binop (fun s x y -> Int64.div x y) OP_DIV operands ;;
let rem operands   = binop (fun s x y -> Int64.rem x y) OP_MOD operands ;;

let shiftlt operands = 	binop (fun s x y -> Int64.shift_left x (Int64.to_int y)) OP_LSL operands;;
let shiftrt operands = 	binop (fun s x y -> Int64.shift_right x (Int64.to_int y)) OP_LSR operands;;

let signed_compare s x y =
	if s then Int64.compare x y else
	match (x >= 0L, y >= 0L) with
		| (true,true) -> Int64.compare x y
		| (true,false) -> -1
		| (false,true) -> 1
		| (false,false) -> Int64.compare y x
;;

let lt operands =	binop (fun s x y -> if signed_compare s x y < 0 then 1L else 0L) OP_LT operands ;;
let gt operands =	binop (fun s x y -> if signed_compare s x y > 0 then 1L else 0L) OP_GT operands ;;
let le operands =	binop (fun s x y -> if signed_compare s x y <= 0 then 1L else 0L) OP_LE operands ;;
let ge operands =	binop (fun s x y -> if signed_compare s x y >= 0 then 1L else 0L) OP_GE operands ;;

let eq operands =	
	binop (fun s x y -> if x = y then 1L else 0L) OP_EQ operands ;;

let ne operands =	
	binop (fun s x y -> if x <> y then 1L else 0L) OP_NE operands ;;

let band operands = binop (fun s x y -> Int64.logand x y ) OP_BAND operands ;;
let bxor operands = binop (fun s x y -> Int64.logxor x y ) OP_BXOR operands ;;
let bor operands  = binop (fun s x y -> Int64.logor x y )  OP_BOR operands ;;

let logand operands =  (* should return int (32-bit) *)
	binop (fun s x y -> if x = 0L || y = 0L
	then 0L else 1L) OP_LAND operands ;;
let logor operands =  (* should return int (32-bit) *)
	binop (fun s x y -> if x = 0L && y = 0L
	then 0L else 1L) OP_LOR operands ;;

let rec opPI op operands =
	let (bytes1, typ1) = List.nth operands 0 in
	let (bytes2, typ2) = List.nth operands 1 in
	match (bytes1, bytes2) with
		| (Bytes_Address(block, offset), offset2) ->
			begin match typ1 with
				| TPtr(basetyp,_) ->
					let base_size = (Cil.bitsSizeOf basetyp)/8 in
					let (offset3) = mult [(lazy_int_to_bytes base_size,!Cil.upointType);(offset2,typ2)] in
					let (offset4) = op [(offset,!Cil.upointType);(offset3,!Cil.upointType)] in (* TODO: do we need to cast the offsets? *)
					(make_Bytes_Address(block, offset4))
				| _ -> failwith "type of Bytes_Address not TPtr"
			end
		| Bytes_ByteArray(_),_ -> (* Doing pointer arithmetic off of a non-pointer, probably NULL *)
			begin match typ1 with
				| TPtr(basetyp,_) ->
					let base_size = (Cil.bitsSizeOf basetyp)/8 in
					let (offset3) = mult [(bytes2,typ2);(lazy_int_to_bytes base_size,!Cil.upointType)] in
					op [(bytes1,!Cil.upointType);(offset3,!Cil.upointType)] (* TODO: do we need to cast the offsets? *)
				| _ -> failwith "type of Bytes_ByteArray (used as a pointer) not TPtr"
			end
		| Bytes_Conditional c, _ ->
			Bytes_Conditional (conditional__map (fun e -> conditional__bytes (opPI op [(e,typ1);(bytes2,typ2)])) c)
		| _, Bytes_Conditional c ->
			Bytes_Conditional (conditional__map (fun e -> conditional__bytes (opPI op [(bytes1,typ1);(e,typ2)])) c)
		| (Bytes_Read(a, x, l)), _ ->
			Bytes_Conditional (
				conditional__map (fun e -> conditional__bytes (opPI op [(e,typ1);(bytes2,typ2)])) 
					(BytesUtility.expand_read_to_conditional a x l)
			)
		| _, (Bytes_Read(a, x, l)) ->
			Bytes_Conditional (
				conditional__map (fun e -> conditional__bytes (opPI op [(bytes1,typ1);(e,typ2)])) 
					(BytesUtility.expand_read_to_conditional a x l)
			)
		| _ ->
			Output.set_mode Output.MSG_MUSTPRINT;
			Output.print_endline ("bytes1: "^(To_string.bytes bytes1)); 
			Output.print_endline ("bytes2: "^(To_string.bytes bytes2));
			failwith "plusPI (p1,p2) not of type (addr,int)"
;;

let plusPI operands =
	opPI plus operands
;;

let minusPI operands =
	opPI minus operands
;;
let minusPP operands : bytes =
	let (bytes1, typ1) = List.nth operands 0 in
	let (bytes2, typ2) = List.nth operands 1 in
	match (bytes1, bytes2) with
		| (Bytes_Address(block1, offset1), Bytes_Address(block2, offset2)) ->
			if block1 <> block2 then 
				failwith "minusPP: different base addresss"
			else
			begin match typ1 with
				| TPtr(basetyp,_) ->
					let base_size = (Cil.bitsSizeOf basetyp)/8 in
					let (offset3) = minus [(offset1,!Cil.upointType);(offset2,!Cil.upointType)] in (* TODO: do we need to cast the offsets? *)
					let (offset4) = div [(offset3,!Cil.upointType);(lazy_int_to_bytes base_size,!Cil.upointType)] in
						(offset4)
				| _ -> failwith "type of Bytes_Address not TPtr"
			end
		| _ ->
				Output.set_mode Output.MSG_MUSTPRINT;
				Output.print_endline ("make_Bytes1: "^(To_string.bytes bytes1));
				Output.print_endline ("make_Bytes2: "^(To_string.bytes bytes2));
				failwith "minusPP (p1,p2) not of type (addr,addr)"
;;



let of_unop unop =
	match unop with
		|	Neg -> neg
		|	BNot	-> bnot
		|	LNot	-> lnot
;;

let of_binop binop =
	match binop with
		|	PlusA	-> plus
		|	MinusA -> minus
		|	Mult	-> mult
		|	Div -> div
		|	Mod -> rem
		|	Shiftlt -> shiftlt
		|	Shiftrt -> shiftrt
		|	Lt -> lt
		|	Gt -> gt
		|	Le -> le
		|	Ge -> ge
		|	Eq -> eq
		|	Ne -> ne
		|	BAnd -> band
		|	BXor -> bxor
		|	BOr -> bor
		|	LAnd -> logand
		|	LOr -> logor
		| PlusPI -> plusPI
		| IndexPI -> plusPI
		| MinusPI -> minusPI
		| MinusPP -> minusPP
;;

let rec eval pc bytes =
	(*
	if not Executeargs.args.Executeargs.arg_print_queries then () else
	Output.print_endline ("Is the following not equal to zero? \n"^(To_string.bytes bytes));*)
	let nontrivial () = 
			Output.set_mode Output.MSG_REG;
			Output.print_endline "Ask STP...";
			Stats.time "STP" (Stp.consult_stp pc) bytes
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
	match bytes with
		(* The following cases are simple enough to not consult STP *)
		| Bytes_Constant (CInt64(n,_,_)) -> if n = 0L then False else True			
		| Bytes_ByteArray (bytearray) ->
				begin try
					let b = bytes_to_bool bytes in  (* TODO:need to use int64 *)
						if b = false then False else True
				with Failure(_) -> nontrivial()
				end
		| Bytes_Address (_,_) -> True
		(* nullity check *)
		| Bytes_Op(OP_LNOT,(b1,_)::[]) -> ternary_not (eval pc b1)
		
		(* Comparison of (ptr+i) and (ptr+j) *)
		| Bytes_Op(op,(Bytes_Address(block1,offset1),_)::(Bytes_Address(block2,offset2),_)::[]) 
			when is_comparison op ->
				if block1!=block2 then (if op==OP_EQ then False else if op==OP_NE then True else nontrivial())
				else  eval pc (run (operation_of op) [(offset1,Cil.intType);(offset2,Cil.intType)])
		
		(* Comparison of (ptr+i) and c (usually zero) *)
		| Bytes_Op(op,(Bytes_Address(block,offset1),_)::(bytes2,_)::[]) 
			when is_comparison op  &&  isConcrete_bytes bytes2 ->
				if op==OP_EQ then False else if op==OP_NE then True else nontrivial()
		(* Function pointer is always true *)
		| Bytes_FunPtr(_,_) -> True
		(* Consult STP *)
		| _ -> 
			nontrivial()
;;

