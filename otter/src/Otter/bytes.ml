open Cil
open Ternary

type operator = 
	(* binop *)
	| OP_PLUS
	| OP_SUB
	| OP_MULT
	| OP_DIV
	| OP_MOD
	| OP_LSL
	| OP_LSR
	| OP_LT
	| OP_GT
	| OP_LE
	| OP_GE
	| OP_EQ
	| OP_NE
	| OP_BAND
	| OP_BXOR
	| OP_BOR
	| OP_LAND
	| OP_LOR
	| OP_SX
	(* unop *)
	| OP_UMINUS
	| OP_BNOT
	| OP_LNOT
;;

(** Does the operator return a boolean value (i.e., 0 or 1)? *)
let returnsBoolean = function
	| OP_LT | OP_GE
	| OP_GT | OP_LE
	| OP_EQ | OP_NE
	| OP_LNOT | OP_LAND | OP_LOR -> true
	| _ -> false


type symbol =	
	{ 
		symbol_id: int; 	
		(*symbol_writable: bool;*)
	}

type byte = (* corresponds to BV *)
	| Byte_Concrete of char
	| Byte_Symbolic of symbol
	| Byte_Bytes of bytes * int (* condense a bytes into a byte, that can be put into an array *)  

and guard =
	| Guard_True
	| Guard_Not of guard
	| Guard_And of guard * guard
	| Guard_Or of guard * guard
	| Guard_Symbolic of symbol
	| Guard_Bytes of bytes

and 'a conditional =
	| IfThenElse of guard * 'a conditional * 'a conditional  (* if guard then a else b *)
	| Unconditional of 'a
   | ConditionalException of exn

and bytes =
	| Bytes_Constant of Cil.constant                (* length=Cil.sizeOf (Cil.typeOf (Const(constant))) *)
	| Bytes_ByteArray of byte ImmutableArray.t      (* content *)
	| Bytes_Address of memory_block * bytes  (* block, offset *)
	| Bytes_Op of operator * (bytes * Cil.typ) list
	| Bytes_Read of bytes * bytes * int             (* less preferrable type *)
	| Bytes_Write of bytes * bytes * int * bytes    (* least preferrable type*)
	| Bytes_FunPtr of Cil.fundec * bytes            (* bytes is the "imaginary address" of the funptr *)
	| Bytes_Unbounded of string * int * bytes       (* name, id, size *)
	| Bytes_Conditional of bytes conditional

and memory_block_type =
	| Block_type_StringLiteral
	| Block_type_Global
	| Block_type_Local
	| Block_type_Heap
	| Block_type_Aliased (* for blocks allocated prior to symbolic execution, below the call stack *)

and memory_block =
	{
		memory_block_name : string;
		memory_block_id : int;
		memory_block_size : int;
		memory_block_addr : bytes;
		memory_block_type : memory_block_type; 
	}

and lval_block = (memory_block * bytes) conditional
;;

let hash_consing_bytes_enabled = ref false;;
let hash_consing_bytes_hits = ref 0;;
let hash_consing_bytes_misses = ref 0;;
let hash_consing_bytes_init_size = 1000000;;
let hash_consing_bytes_tbl : (bytes,bytes) Hashtbl.t = Hashtbl.create hash_consing_bytes_init_size;;
let hash_consing_bytes_create bs = 
  if not (!hash_consing_bytes_enabled) then bs else
  try let rv = Hashtbl.find hash_consing_bytes_tbl bs in Utility.increment hash_consing_bytes_hits; rv
  with Not_found -> Hashtbl.add hash_consing_bytes_tbl bs bs; Utility.increment hash_consing_bytes_misses; bs;;


(*
 *  Since bytes objects are immutable, and bytes is private type, all *bs* are
 *  created by calling make_Bytes_* and do not require hash consing check.
 *)
let rec 
make_Byte_Concrete (c) =
	Byte_Concrete (c)
and
make_Byte_Symbolic (s) =
	Byte_Symbolic (s)
and
make_Byte_Bytes ( bs, n ) =
	Byte_Bytes ( bs, n )
and
make_Bytes_Constant ( const ) =
	hash_consing_bytes_create (Bytes_Constant ( const ))
and
make_Bytes_ByteArray ( bytearray ) =
	hash_consing_bytes_create (Bytes_ByteArray ( bytearray ))
and
make_Bytes_Address ( block , bs ) =
	hash_consing_bytes_create (Bytes_Address ( block , bs ))
and
make_Bytes_Op ( op , lst) =
	hash_consing_bytes_create (Bytes_Op ( op , lst))
and
make_Bytes_Read ( src , off , len ) =
	hash_consing_bytes_create (Bytes_Read ( src , off , len ))
and
make_Bytes_Write ( des , off , n , src ) =
	hash_consing_bytes_create (Bytes_Write ( des , off , n , src ))
and
make_Bytes_FunPtr ( f , bs ) =
	hash_consing_bytes_create (Bytes_FunPtr ( f , bs ))
and
make_Bytes_Conditional = function
	| Unconditional b -> b
	| c -> hash_consing_bytes_create (Bytes_Conditional ( c ))
;;



let ikind_to_len_isSigned ikind =
	(bitsSizeOf (TInt (ikind, [])) / 8, isSigned ikind)
;;

(**
 *	to bytes
 *)

(** Convert constant to make_Bytes_Constant *)
let lazy_constant_to_bytes constant : bytes =
	make_Bytes_Constant(constant)
	;;

(** Convert an (int64 of ikind) to make_Bytes_Constant(CInt64(int64,ikind,None)) *)
let lazy_int64_to_bytes n ikind : bytes =
	lazy_constant_to_bytes (CInt64(n,ikind,None))
	;;

(** Convert an ocaml (signed) int to make_Bytes_Constant(CInt64(int64,IInt,None)) *)
let lazy_int_to_bytes n : bytes =
	lazy_int64_to_bytes (Int64.of_int n) IInt
	;;

(* Make this lazy so that upointType is set correctly (by initCIL) before it is evaluated *)
let kindOfUpointType = lazy (match !upointType with
		TInt (kind, _) -> kind
	| _ -> failwith "upointType is not set properly")
;;

(* Convert an int n to [Bytes_Constant (CInt64 (Int64.of_int n, kindOfUpointType, None))] *)
let int_to_offset_bytes (n : int) : bytes =
	make_Bytes_Constant (CInt64 (Int64.of_int n, Lazy.force kindOfUpointType, None))
;;

(** Convert in64 of ikind to make_Bytes_ByteArray. Truncate if needed.  *)
let int64_to_bytes n64 ikind : bytes =
	let (len,isSigned) = ikind_to_len_isSigned ikind in
	let rec helper n acc count =
		if count = len then acc
		else helper (Int64.shift_right n 8)
				(make_Byte_Concrete (Char.chr ((Int64.to_int n) land 255)) :: acc)
				(succ count)
	in
	make_Bytes_ByteArray(ImmutableArray.of_list (List.rev (helper n64 [] 0))) (* Reverse because we are little-endian *)
	;;

let string_map f s =
	let rec helper i acc =
		if i < 0 then
			acc
		else
			helper (i-1) (f s.[i] :: acc)
	in helper (String.length s - 1) []

(** Convert CString to make_Bytes_ByteArray.  *)
let string_to_bytes (s : string) : bytes =
	make_Bytes_ByteArray (ImmutableArray.of_list (string_map (fun ch -> make_Byte_Concrete ch) (s^"\000")))
;;

(** Coonvert real numbers to make_Bytes *)
(* TODO: actually represent the numbers *)
let float_to_bytes f fkind =
	let length = bitsSizeOf (TFloat (fkind, [])) / 8 in
	make_Bytes_ByteArray (ImmutableArray.make length (make_Byte_Concrete '\000'))

(** Convert constant to make_Bytes_ByteArray *)
let rec constant_to_bytes constant : bytes =
	match constant with
		| CInt64(n64,ikind,_) -> int64_to_bytes n64 ikind
		| CStr(str) ->
				let rec impl str =
					if String.length str = 0 then ['\000'] else
						(str.[0]) :: (impl (String.sub str 1 ((String.length str) - 1)))
				in
				let chars = impl str in
				let bytes = make_Bytes_ByteArray (ImmutableArray.of_list (List.map (fun x -> make_Byte_Concrete(x)) (chars))) in
					bytes
		| CChr(char) ->
				constant_to_bytes (Cil.charConstToInt char)
		| CReal (f,fkind,_) -> float_to_bytes f fkind
		| _ -> failwith "constant_to_bytes: unsupported constant type"
	;;

let constant_to_bytearray constant : byte ImmutableArray.t =
	match constant_to_bytes constant with
		| Bytes_ByteArray(ba) -> ba
		| _ -> failwith "constant_to_bytearray: error"
;;


(**
 *	from bytes
 *)

(** Convert a bytes of (len,isSigned) to int64 of (len,isSigned).
		If (len,isSigned) == IULongLong then interpret int64 as unsigned. (Same treatment throughout the program).
		Exception if bytes is not concrete int *)
let rec bytes_to_int64 bytes isSigned : int64 =
	match bytes with
		(* TODO: special case for const==Int64 *)
		| Bytes_Constant (const) -> bytes_to_int64 (constant_to_bytes const) isSigned
		| Bytes_ByteArray (bytearray) -> (* This assumes little-endian *)
				let rec bytearray_to_int64_helper index acc =
					if index < 0 then acc
					else
						bytearray_to_int64_helper
							(index - 1)
							(match (ImmutableArray.get bytearray index) with
							|	Byte_Concrete(c) -> Int64.logor (Int64.shift_left acc 8) (Int64.of_int (Char.code c))
							|	_ -> failwith "bytes_to_int64: bytearray not concrete")
				in
				bytearray_to_int64_helper ((ImmutableArray.length bytearray) - 1) 0L
		| _ -> failwith "bytes_to_int64: not concrete int"
	;;

(** Convert a bytes to (signed) int64. Exception if bytes is not concrete int *)
let bytes_to_int64_auto bytes : int64 =
	bytes_to_int64 bytes true
	;;

(** Convert a bytes to (signed) ocaml int.
	Warning if bytes represents number out of int's range.
	Exception if bytes is not concrete int *)
let bytes_to_int_auto bytes : int =
	let n64 = bytes_to_int64_auto bytes in
	let res = Int64.to_int n64 in
	if n64 <> Int64.of_int res then
		Errormsg.warn "Int64 %Ld is being truncated to an int" n64;
	res
	;;

(** Convert a bytes to boolean. Exception if bytes is not concrete *int* *)
let bytes_to_bool bytes : bool =
	let n64 = bytes_to_int64_auto bytes in
		n64 <> 0L
	;;

(** Convert a bytes of typ to Cil constant. Exception if bytes is not concrete *)
let rec bytes_to_constant bytes typ : Cil.constant =
	match typ with
		| TInt(ikind,_)->
			let (len,isSigned) = ikind_to_len_isSigned ikind in
			let n64 = bytes_to_int64 bytes isSigned in
			let exp =	Cil.kinteger64 ikind n64 in
				(match exp with Const(const) -> const | _ -> failwith "unreachable")
		| TNamed(tinfo,_) -> bytes_to_constant bytes tinfo.ttype
		| TFloat(fkind,_) -> (*TMP*) CReal(0.1,fkind,None)
		| TEnum (enuminf,_) ->
				(* An enum has type int. [Standard 6.7.2.2.2, but I'm confused by 6.7.2.2.4] *)
				bytes_to_constant bytes Cil.intType
		| t ->
			begin match bytes with
				| Bytes_Constant(c) -> c
				| _ ->
				failwith ("bytes_to_constant: "^(Pretty.sprint 50 (Cil.d_type () t)))
			end
	;;

(** Convert a possibly-address bytes to make_Bytes_Address *)
let rec bytes_to_address bytes : memory_block * bytes =
	let fail () = failwith "bytes_to_address: not an address" in
	match bytes with
		| Bytes_Address(block,offset) -> (block,offset)
		| Bytes_ByteArray(ba) ->
			if ImmutableArray.length ba <> (bitsSizeOf voidPtrType / 8) then fail ()
			else let g = ImmutableArray.get ba in
			begin match g 0,g 1,g 2,g 3 with
				| Byte_Bytes(b0,0),Byte_Bytes(b1,1),Byte_Bytes(b2,2),Byte_Bytes(b3,3)
						when b0==b1 && b1==b2 && b2==b3
						-> bytes_to_address b0
				| _ -> fail ()
			end
		| _ -> fail ()
;;


(** True if bytearray is concrete *)
(* Shouldn't a make_Byte_Bytes with concrete values be considered concrete, too? *)
let isConcrete_bytearray (bytearray : byte ImmutableArray.t) =
	ImmutableArray.for_all (function Byte_Concrete _ -> true | _ -> false) bytearray
	;;

(** True if bytes is concrete *)
let rec isConcrete_bytes (bytes : bytes) =
	match bytes with
		| Bytes_Constant (_) -> true
		| Bytes_ByteArray (bytearray) -> isConcrete_bytearray bytearray
		| _ -> false
	;;


(**
 *  equality for byte/guards/conditionals/bytes
 *)

let rec byte__equal byte1 byte2 = if byte1 == byte2 then true else match byte1, byte2 with
	| Byte_Concrete x, Byte_Concrete y            -> x = y
	| Byte_Symbolic x, Byte_Symbolic y            -> x = y
	| Byte_Bytes (b1, off1), Byte_Bytes(b2, off2) -> off1 = off2 && bytes__equal b1 b2
	| _, _                                        -> false

and guard__equal guard1 guard2 = if guard1 == guard2 then true else match guard1, guard2 with
	| Guard_Not g1, Guard_Not g2               -> guard__equal g1 g2
	| Guard_And (g1, g2), Guard_And (g1', g2') -> guard__equal g1 g1' && guard__equal g2 g2'
	| Guard_Symbolic s1, Guard_Symbolic s2     -> s1 = s2
	| Guard_Bytes b1, Guard_Bytes b2           -> bytes__equal b1 b2
	| _, _                                     -> false

and conditional__equal eq c1 c2 = if c1 == c2 then true else match c1, c2 with
	| Unconditional x1, Unconditional x2 ->
		eq x1 x2
	| IfThenElse (g1, x1, y1), IfThenElse (g2, x2, y2) ->
		guard__equal g1 g2 && conditional__equal eq x1 x2 && conditional__equal eq y1 y2
	| _, _ ->
		false

and bytes__equal bytes1 bytes2 = if bytes1 == bytes2 then true else match bytes1, bytes2 with
	| Bytes_Constant c, b
	| b, Bytes_Constant c ->
		bytes__equal b (constant_to_bytes c)
	| Bytes_ByteArray a1, Bytes_ByteArray a2 ->
		ImmutableArray.length a1 = ImmutableArray.length a2 && ImmutableArray.for_all2 byte__equal a1 a2
	| Bytes_Address(b1, off1),Bytes_Address(b2, off2) ->
		b1 = b2 && bytes__equal off1 off2
	| Bytes_Op (op1, operands1), Bytes_Op (op2, operands2) ->
		op1 = op2 && List.for_all2 (fun (b1, _) (b2, _) -> bytes__equal b1 b2) operands1 operands2
	| Bytes_Read (b1, off1, s1), Bytes_Read (b2, off2, s2) ->
		s1 = s2 && bytes__equal b1 b2 && bytes__equal off1 off2
	| Bytes_Write (old1, off1, s1, new1), Bytes_Write (old2, off2, s2, new2) ->
		s1 = s2 && bytes__equal old1 old2 && bytes__equal off1 off2 && bytes__equal new1 new2
	| Bytes_FunPtr (f1, addr1), Bytes_FunPtr (f2, addr2) ->
		f1 = f2
	| Bytes_Conditional c1, Bytes_Conditional c2 ->
		(* using conditional__equal will make it not polymorphic *)
		let rec bytes_conditional_equal c1 c2 = if c1 == c2 then true else match c1, c2 with
			| Unconditional x1, Unconditional x2 ->
				bytes__equal x1 x2
			| IfThenElse (g1, x1, y1), IfThenElse (g2, x2, y2) ->
				guard__equal g1 g2 && bytes_conditional_equal x1 x2 && bytes_conditional_equal y1 y2
			| _, _ ->
				false
		in
		bytes_conditional_equal c1 c2
	| _, _ ->
		false

;;

(* boolean *)
let tru = lazy_int_to_bytes 1;;
let fls = lazy_int_to_bytes 0;;

let bytes_or b1 b2 = 
  if b1=fls then b2 else if b2=fls then b1 else
  make_Bytes_Op (OP_LOR, [(b1,Cil.intType);(b2,Cil.intType)]);;
let bytes_and b1 b2 = 
  if b1=tru then b2 else if b2=tru then b1 else
  make_Bytes_Op (OP_LAND, [(b1,Cil.intType);(b2,Cil.intType)]);;
let bytes_not b = 
  if b=tru then fls else if b=fls then tru else 
  make_Bytes_Op (OP_LNOT, [b,Cil.intType])

(* A single global byte representing uninitialized memory *)
let byte__undef = Byte_Symbolic({symbol_id = 0}) ;;

let max_bytes_size = 0xffff


let rec bytes__length bytes =
	match bytes with
		| Bytes_Constant (constant) -> (Cil.bitsSizeOf (Cil.typeOf (Const(constant))))/8
		| Bytes_ByteArray (bytearray) -> ImmutableArray.length bytearray
		| Bytes_Address (_,_)-> bitsSizeOf voidPtrType / 8
		| Bytes_Op (op,(bytes2,typ)::tail) -> bytes__length bytes2
		| Bytes_Op (op,[]) -> 0 (* reachable from diff_bytes *)
		| Bytes_Write(bytes2,_,_,_) -> bytes__length bytes2
		| Bytes_Read(_,_,len) -> len
		| Bytes_FunPtr(_) -> bitsSizeOf voidPtrType / 8
		| Bytes_Unbounded (_,_,size) ->
			if isConcrete_bytes bytes then bytes_to_int_auto size
			else  max_bytes_size
		| Bytes_Conditional c ->
			(* all bytes in Bytes_Conditional have the same length *)
			let rec find_one = function
				| IfThenElse (_, c1, c2) -> max (find_one c1) (find_one c2)
				| Unconditional b -> bytes__length b
           | ConditionalException _ -> -1
			in
			find_one c
;;

(**
 *  symbol
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
let byte__make c = make_Byte_Concrete c;;
let byte__zero = byte__make ('\000');;
let byte__111 = byte__make ('\255');;
let byte__random () = byte__make (char__random ());;
let byte__symbolic () = make_Byte_Symbolic (symbol__next ());;


(**
 *	bytes
 *)
let bytes__zero = make_Bytes_Constant(Cil.CInt64(0L,IInt,None));;
let bytes__one = make_Bytes_Constant(Cil.CInt64(1L,IInt,None));;
let bytes__of_list (lst: byte list) =	make_Bytes_ByteArray (ImmutableArray.of_list lst) ;;
let bytes__make_default n byte = make_Bytes_ByteArray(ImmutableArray.make n byte);;
let bytes__make n = bytes__make_default n byte__zero;;
let bytes__random n =
	let rec impl i arr =
		if i>=n then arr else
			impl (i+1) (ImmutableArray.set arr i (byte__random ()))
		in
	make_Bytes_ByteArray(impl 0 (ImmutableArray.make n byte__zero))
;;

let bytes__symbolic n =
	let rec impl len = 
		if len <= 0 then [] else (byte__symbolic ())::(impl (len-1))
	in
		bytes__of_list (impl n)
;;

let rec bytes__get_byte bytes i : byte =
	match bytes with
		| Bytes_Constant (constant) ->  bytes__get_byte (constant_to_bytes constant) i
		| Bytes_ByteArray (bytearray) -> ImmutableArray.get bytearray i 
		| _ -> make_Byte_Bytes(bytes,i)
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
		memory_block_addr = bytes__random (bitsSizeOf voidPtrType / 8);
		memory_block_type = t;
	}
;;
let block__make_string_literal name n =
	let block = block__make name n Block_type_StringLiteral in
	{block with
		memory_block_type = Block_type_StringLiteral;
	}
;;


(** Is a bytes 0, 1, or an expression that must be 0 or 1? *)
let isBoolean = function
	| Bytes_Op(op,_) when returnsBoolean op -> true
	| Bytes_Constant (Cil.CInt64 ((0L|1L),_,_)) -> true
	(* Is it worth testing for a Bytes_ByteArray representing 0 or 1? *)
	| _ -> false

(** Returns a bytes equivalent to !!x, but only adds the double negation if
	necessary to ensure a boolean value. *)
let asBoolean bytes =
	if isBoolean bytes
	then bytes (* bytes is already boolean-valued *)
	(* The result of a '!' is an int [Standard 6.5.3.3.5]; hence, the
		[Cil.intType] below. The [Cil.voidType] is there just as a
		placeholder, because LNOT doesn't actually care about its
		argument's type. Actually, this means that we don't really need
		the intType at all; we could be use voidType in both places. *)
	else make_Bytes_Op(OP_LNOT,[(make_Bytes_Op(OP_LNOT,[(bytes,Cil.voidType)]),Cil.intType)])

(** Remove a NOT from a bytes, if doing so leaves it boolean. Otherwise, add a
	NOT. *)
let logicalNot = function
		Bytes_Op(OP_LNOT,[bytes,_]) when isBoolean bytes -> bytes
	| bytes -> make_Bytes_Op(OP_LNOT,[(bytes, Cil.intType)])

(**
 *  guard
 *)
let guard__true = Guard_True

let guard__not = function
  | Guard_Not g -> g
  | g -> Guard_Not g

let guard__false = guard__not guard__true

let guard__and g1 g2 = match g1, g2 with
  | Guard_True, g
  | g, Guard_True -> g
  | _, _          -> Guard_And (g1, g2)

let guard__or g1 g2 = match g1, g2 with
  | Guard_Not Guard_True, g
  | g, Guard_Not Guard_True -> g
  | _, _          -> Guard_Or (g1, g2)

let guard__and_not g1 g2 = guard__and g1 (guard__not g2)

let guard__symbolic () = Guard_Symbolic (symbol__next ())

let guard__symbolics () = Guard_Bytes (bytes__symbolic 4)

let guard__bytes b = Guard_Bytes b

let rec guard__to_bytes g = match g with
  | Guard_Or(g1,g2) -> Bytes_Op (OP_LOR,[guard__to_bytes g1,Cil.intType;guard__to_bytes g2,Cil.intType])
  | Guard_And(g1,g2) -> Bytes_Op (OP_LAND,[guard__to_bytes g1,Cil.intType;guard__to_bytes g2,Cil.intType])
  | Guard_Not(g1) -> Bytes_Op (OP_LNOT,[guard__to_bytes g1,Cil.intType])
  | Guard_Bytes(b) -> b
  | Guard_Symbolic(s) -> 
      (* The below doesn't work! why???
      bytes__make_default 4 (Byte_Symbolic s) 
      bytes__of_list [(Byte_Symbolic s);byte__zero;byte__zero;byte__zero]
       *)
      failwith "guard__to_bytes: Guard_Symbolic doesn't work"

  | Guard_True -> bytes__one
;;


(**
 *  conditional
 *)

(** Map and fold simultaneously over the leaves of conditionals.
    @param test        an optional test function to filter by the guard condition : guard -> guard -> Ternary.t
    @param eq          an optional equality function to prune identical leaves : 'target -> 'target -> bool
    @param pre         an optional precondition
    @param map_fold     the map and fold function : 'acc -> guard -> 'source -> 'acc * 'target conditional
    @param acc          the initial accumulator
    @param source       the original conditional to map from
    @return [(acc, 'target conditional)]
                        the final accumulator and mapped conditional
*)
let conditional__map_fold ?(test=fun _ _ -> Unknown) ?(eq=(==)) ?(pre=Guard_True) map_fold acc source =
	let rec conditional__map_fold acc pre = function
		| IfThenElse (guard, x, y) ->
			begin match test pre guard with
				| True ->
					(* test pre ==> guard *)
					conditional__map_fold acc pre x
				| False ->
					(* test pre ==> !guard *)
					conditional__map_fold acc pre y
				| Unknown ->
					let acc, x = conditional__map_fold acc (guard__and pre guard) x in
					let acc, y = conditional__map_fold acc (guard__and_not pre guard) y in
					(* prune away unnecessary IfThenElse *)
					if conditional__equal eq x y then
						(acc, x)
					else
						(acc, IfThenElse (guard, x, y))
			end
		| Unconditional x ->
          (
          try
			   map_fold acc pre x
          with e -> 
            if (Executeargs.run_args.Executeargs.arg_use_conditional_exceptions) then
              begin
              Output.set_mode Output.MSG_REG;
              Output.print_endline (Printf.sprintf "(Exception \"%s\" caught in conditional__map_fold)" (Utility.errormsg e));
              acc, ConditionalException e
              end
            else
              raise e
          )
     | ConditionalException e -> acc, ConditionalException e
	in
	conditional__map_fold acc pre source


(** Map over the leaves of conditionals.
    @param test        an optional test function to filter by the guard condition : guard -> guard -> Ternary.t
    @param eq          an optional equality function to prune identical leaves : 'target -> 'target -> bool
    @param pre         an optional precondition
    @param map          the map function : 'source -> 'target conditional
    @param source       the original conditional to map from
    @return ['target conditional]
                        the mapped conditional
*)
let conditional__map ?test ?eq ?pre map source =
	snd (conditional__map_fold ?test ?eq ?pre (fun () _ x -> ((), map x)) () source)


(** Fold over the leaves of conditionals.
    @param test    an optional function to test the conditional guard : guard -> guard -> Ternary.t
    @param pre     an optional precondition
    @param fold     the fold function : 'acc -> guard -> 'source -> 'acc
    @param acc      the initial accumulator
    @param source   the conditional to fold over
    @return [acc]   the final accumulator
*)
let conditional__fold ?test ?pre fold acc source =
	fst (conditional__map_fold ?test ?pre (fun acc pre x -> (fold acc pre x, Unconditional x)) acc source)


(** Given a list of length n, return a conditional tree of height log(n) containing all items in the list.
    @param list     a list of items : 'item
    @return ['item conditional]
                    a conditional tree of items
*)
let conditional__from_list list =
	let rec conditional__make_tree outs = function
		| x::y::rest -> conditional__make_tree ((IfThenElse (guard__symbolics (), x, y))::outs) rest
		| [x]      -> conditional__make_tree_next (x::outs)
		| []         -> conditional__make_tree_next outs
	and conditional__make_tree_next = function
		| [ x ] -> x
		| []    -> failwith "No items in list!"
		| outs  -> conditional__make_tree [] outs
	in
	conditional__make_tree [] list

let conditional__bytes = function
	| Bytes_Conditional c -> c
	| b -> Unconditional b


let conditional__lval_block l =
	Unconditional l

let rec conditional__remove_exceptions c =
   match c with
     | IfThenElse (g,x,y) ->
         let px,x = conditional__remove_exceptions x in
         let py,y = conditional__remove_exceptions y in
         let pxy,xy = match x,y with
           | ConditionalException _,ConditionalException _ -> guard__false,x
           | ConditionalException _,_ -> (guard__and (guard__not g) py),y
           | _,ConditionalException _ -> (guard__and g px),x
           | _,_ -> (guard__or (guard__and g px) (guard__and (guard__not g) py)),IfThenElse(g,x,y)
         in pxy,xy
     | Unconditional something -> guard__true,c (* TODO: something may contain exceptions, but its type is 'a *)
     | ConditionalException _ -> guard__false,c

let rec bytes__remove_exceptions bytes =
  match bytes with
    | Bytes_Conditional (c) -> 
        let g,c = conditional__remove_exceptions c in g,Bytes_Conditional(c)
    | Bytes_ByteArray (bytearray) -> 
        let g = ref guard__true in
        let bytearray = ImmutableArray.map (fun byte -> match byte with 
                              | Byte_Bytes (bs,n) -> 
                                  let g',bs = bytes__remove_exceptions bs in 
                                    g := (guard__and g' (!g));
                                    Byte_Bytes (bs,n)
                              | _ -> byte
                           ) bytearray
        in (!g),Bytes_ByteArray(bytearray)
    | Bytes_Address (block,offset) -> 
        let g,offset = bytes__remove_exceptions offset in g, Bytes_Address(block,offset)
    | Bytes_Op (op,operands) -> 
        let g,oplst = List.fold_right 
                    (fun (operand,typ) (g,oplst) -> 
                       let g',opr = bytes__remove_exceptions operand in 
                         (guard__and g g'),(opr,typ)::oplst) 
                    operands (guard__true,[])
        in 
          g,Bytes_Op(op,oplst)
    | Bytes_Read (b1,b2,n) -> 
        let g1,b1 = bytes__remove_exceptions b1 in
        let g2,b2 = bytes__remove_exceptions b2 in
         (guard__and g1 g2),Bytes_Read(b1,b2,n)
    | Bytes_Write (b1,b2,n,b3) -> 
        let g1,b1 = bytes__remove_exceptions b1 in
        let g2,b2 = bytes__remove_exceptions b2 in
        let g3,b3 = bytes__remove_exceptions b3 in
         (guard__and g3 (guard__and g1 g2)),Bytes_Write (b1,b2,n,b3)
    | _ -> guard__true,bytes
;;

