(**
	This module should define all functions of type operator_action.
	E.g. 
	let plus (operands: (bytes*typ) list) : bytes =
		... 
 *)
open Cil
open Types





let run op operands = op operands;;


let unop op_conc (*bytearray->bytearray*) op_symb operands : bytes  = 
	let (bytes1, typ1) = List.nth operands 0 in
	let rec impl bytes typ =
		match bytes with
			| Bytes_Constant(const) -> impl (Convert.constant_to_bytes const) typ
			| Bytes_ByteArray(bytearray) -> 
				if Convert.isConcrete_bytearray bytearray 
				then
					Bytes_ByteArray(op_conc bytearray typ)
				else 
					Bytes_Op(op_symb,operands)
			| _ -> (Bytes_Op(op_symb, operands))
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
					Byte_Concrete(c')
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
					Byte_Concrete(c')
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
			ImmutableArray.of_list [Byte_Concrete('\000');Byte_Concrete('\000');Byte_Concrete('\000');Byte_Concrete('\000')]
		else (* 1 *)
			ImmutableArray.of_list [Byte_Concrete('\001');Byte_Concrete('\000');Byte_Concrete('\000');Byte_Concrete('\000')]
	in
	unop op_conc OP_LNOT operands ;;


(* TODO: each op must also have typ of par as arg.

 *)
(* binop. suitable for binop with operands of equal type *)
let binop op_const op_symb operands : bytes (* * typ *)=
	let (bytes1, typ1) = List.nth operands 0 in
	let (bytes2, typ2) = List.nth operands 1 in

	(* Maybe an error of ocaml: if ibytes? is replaced by bytes?, the scoping is messed up *)	
	let rec impl (ibytes1,ibytes2) =
	match (ibytes1, ibytes2) with
		| (Bytes_Constant(CInt64(i1, k1, _)), Bytes_Constant(CInt64(i2, k2, _))) ->
				let isSigned = 	if Cil.isSigned k1 <> Cil.isSigned k2 then true else Cil.isSigned k1 in
				let n64 = op_const isSigned i1 i2 in
				let (n64,_) = Cil.truncateInteger64 k1 n64 in
				let const = CInt64(n64, k1, None) in (* ASSUMED result always has type equal to that of first operand *)
				(Bytes_Constant(const))
		
		| (b1,b2)  ->
			if not (Convert.isConcrete_bytes b1 & Convert.isConcrete_bytes b2) then
				(Bytes_Op(op_symb, operands))
			else
			let c1 = Convert.bytes_to_constant b1 typ1 in (*TODO: look at typ1 to see if it's unsigned *)
			let c2 = Convert.bytes_to_constant b2 typ2 in
			begin match (c1,c2) with
			| (CInt64(i1,k1,s1),CInt64(i2,k2,s2)) ->
				impl ((Bytes_Constant(CInt64(i1,k1,s1))),(Bytes_Constant(CInt64(i2,k2,s2))))
			| (CReal(i1,k1,s1),CReal(i2,k2,s2)) -> (*TMP*) bytes1
			| _ -> failwith "Match error"
			end
	in
		impl (bytes1,bytes2)
;;
(*
let signextend operands = 
	let nativeop n1 n2 =
		n1
	in
		binop nativeop OP_SX operands 
;;*)

(* Fix sign problem! *)
let plus operands  = binop (fun s x y -> Int64.add x y) OP_PLUS operands ;;
let minus operands = binop (fun s x y -> Int64.sub x y) OP_SUB operands ;;	
let mult operands  = binop (fun s x y -> Int64.mul x y) OP_MULT operands ;;
let div operands   = binop (fun s x y -> Int64.div x y) OP_DIV operands ;;
let rem operands   = binop (fun s x y -> Int64.rem x y) OP_MOD operands ;;

let shiftlt operands = 	binop (fun s x y -> Int64.shift_left x (Int64.to_int y)) OP_LSL operands;;
let shiftrt operands = 	binop (fun s x y -> Int64.shift_right x (Int64.to_int y)) OP_LSR operands;;

let signed_compare s x y =
	if s then Int64.compare x y else
	match (Int64.compare x Int64.zero >=0 ,Int64.compare y Int64.zero >=0) with
		| (true,true) -> Int64.compare x y
		| (true,false) -> -1
		| (false,true) -> 1
		| (false,false) -> Int64.compare y x
;;

let lt operands =	binop (fun s x y -> if signed_compare s x y < 0 then Int64.one else Int64.zero) OP_LT operands ;;
let gt operands =	binop (fun s x y -> if signed_compare s x y > 0 then Int64.one else Int64.zero) OP_GT operands ;;
let le operands =	binop (fun s x y -> if signed_compare s x y <= 0 then Int64.one else Int64.zero) OP_LE operands ;;
let ge operands =	binop (fun s x y -> if signed_compare s x y >= 0 then Int64.one else Int64.zero) OP_GE operands ;;

let eq operands =	
	binop (fun s x y -> if Int64.compare x y = 0 then Int64.one else Int64.zero) OP_EQ operands ;;

let ne operands =	
	binop (fun s x y -> if Int64.compare x y <> 0 then Int64.one else Int64.zero) OP_NE operands ;;

let band operands = binop (fun s x y -> Int64.logand x y ) OP_BAND operands ;;
let bxor operands = binop (fun s x y -> Int64.logxor x y ) OP_BXOR operands ;;
let bor operands  = binop (fun s x y -> Int64.logor x y )  OP_BOR operands ;;

let logand operands =  (* should return int (32-bit) *)
	binop (fun s x y -> if Int64.compare x Int64.zero = 0 || Int64.compare y Int64.zero = 0
	then Int64.zero else Int64.one) OP_LAND operands ;;
let logor operands =  (* should return int (32-bit) *)
	binop (fun s x y -> if Int64.compare x Int64.zero = 0 && Int64.compare y Int64.zero = 0
	then Int64.zero else Int64.one) OP_LOR operands ;;

let opPI op operands =
	let (bytes1, typ1) = List.nth operands 0 in
	let (bytes2, typ2) = List.nth operands 1 in
	match (bytes1, bytes2) with
		| (Bytes_Address(blockopt,offset), offset2) ->
			begin match typ1 with
				| TPtr(basetyp,_) ->
					let base_size = (Cil.bitsSizeOf basetyp)/8 in
					let (offset3) = mult [(offset2,typ2);(Convert.lazy_int_to_bytes base_size,Cil.intType)] in
					let (offset4) = op [(offset,Cil.intType);(offset3,Cil.intType)] in (* TODO: make typing of offset more accurate? *)
					(Bytes_Address(blockopt,offset4))
				| _ -> failwith "type of Bytes_Address not TPtr"
			end
		| _ ->
			Output.print_endline ("Bytes1: "^(To_string.bytes bytes1)); 
			Output.print_endline ("Bytes2: "^(To_string.bytes bytes2));
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
		| (Bytes_Address(blockopt1,offset1),Bytes_Address(blockopt2,offset2)) ->
			if blockopt1 <> blockopt2 then 
				failwith "minusPP: different base addresss"
			else
			begin match typ1 with
				| TPtr(basetyp,_) ->
					let base_size = (Cil.bitsSizeOf basetyp)/8 in
					let (offset3) = minus [(offset1,Cil.intType);(offset2,Cil.intType)] in (* TODO: make typing of offset more accurate? *)
					let (offset4) = div [(offset3,Cil.intType);(Convert.lazy_int_to_bytes base_size,Cil.intType)] in
						(offset4)
				| _ -> failwith "type of Bytes_Address not TPtr"
			end
		| _ -> failwith "plusPI (p1,p2) not of type (addr,int)"
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

