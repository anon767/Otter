open Cil

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
    | Guard_Symbolic of symbol
    | Guard_Bytes of bytes

and bytes =
	| Bytes_Constant of Cil.constant (* length=Cil.sizeOf (Cil.typeOf (Const(constant))) *)
	| Bytes_ByteArray of byte ImmutableArray.t  (* content *) 
	| Bytes_Address of memory_block option * bytes (* offset *)
	| Bytes_IfThenElse of guard * bytes * bytes (* conditional value of the form: if guard then bytes1 else bytes2 *)
	| Bytes_Op of operator * (bytes * Cil.typ) list
	| Bytes_Read of bytes * bytes * int						(* less preferrable type *)
	| Bytes_Write of bytes * bytes * int * bytes	(* least preferrable type*)
	| Bytes_FunPtr of Cil.fundec * bytes (* bytes is the "imaginary address" of the funptr *)
	| Bytes_Unbounded of string * int * bytes (* name, id, size *)

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

and lval_block =
	| Lval_Block of memory_block * bytes
	| Lval_IfThenElse of guard * lval_block * lval_block
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
make_Bytes_Address ( blockopt , bs ) =
	hash_consing_bytes_create (Bytes_Address ( blockopt , bs ))
and
make_Bytes_IfThenElse ( g , bs1 , bs2 ) =
	hash_consing_bytes_create (Bytes_IfThenElse ( g , bs1 , bs2 ))
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


(* A single global byte representing uninitialized memory *)
let byte__undef = Byte_Symbolic({symbol_id = 0}) ;;

let word__size = 4

let max_bytes_size = 0xffff



(**
 *	utilities
 *)

let ikind_to_len_isSigned ikind =
	let len,isSigned = match ikind with
			|	IChar	-> 1,true
			|	ISChar	-> 1,true
			|	IUChar	-> 1,false
			|	IInt	-> 4,true
			|	IUInt	-> 4,false
			|	IShort	-> 2,true
			|	IUShort	-> 2,false
			|	ILong	-> 4,true
			|	IULong	-> 4,false
			|	ILongLong	-> 8,true
			|	IULongLong	-> 8,false
	in (len,isSigned)
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
	let length = match fkind with
		| FFloat			(*	float	*) -> 1
		| FDouble			(*	double	*) -> 2
		| FLongDouble	(*	long double	*) -> 3
	in
	make_Bytes_ByteArray (ImmutableArray.make (length * word__size) (make_Byte_Concrete '\000'))

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
let rec bytes_to_address bytes : memory_block option*bytes =
	let fail () = failwith "bytes_to_address: not an address" in
	match bytes with
		| Bytes_Address(blockopt,offset) -> (blockopt,offset)
		| Bytes_ByteArray(ba) ->
			if ImmutableArray.length ba <> word__size then fail ()
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

and bytes__equal bytes1 bytes2 = if bytes1 == bytes2 then true else match bytes1, bytes2 with
	| Bytes_Constant c, b
	| b, Bytes_Constant c ->
		bytes__equal b (constant_to_bytes c)
	| Bytes_ByteArray a1, Bytes_ByteArray a2 ->
		ImmutableArray.length a1 = ImmutableArray.length a2 && ImmutableArray.for_all2 byte__equal a1 a2
	| Bytes_Address(Some b1, off1),Bytes_Address(Some b2, off2) ->
		b1 = b2 && bytes__equal off1 off2
	| Bytes_Address(None, off1),Bytes_Address(None, off2) ->
		bytes__equal off1 off2
	| Bytes_IfThenElse (gx, tx, fx), Bytes_IfThenElse (gy, ty, fy) ->
		(guard__equal gx gy) || (bytes__equal tx ty) || (bytes__equal fx fy)
	| Bytes_Op (op1, operands1), Bytes_Op (op2, operands2) ->
		op1 = op2 && List.for_all2 (fun (b1, _) (b2, _) -> bytes__equal b1 b2) operands1 operands2
	| Bytes_Read (b1, off1, s1), Bytes_Read (b2, off2, s2) ->
		s1 = s2 && bytes__equal b1 b2 && bytes__equal off1 off2
	| Bytes_Write (old1, off1, s1, new1), Bytes_Write (old2, off2, s2, new2) ->
		s1 = s2 && bytes__equal old1 old2 && bytes__equal off1 off2 && bytes__equal new1 new2
	| Bytes_FunPtr (f1, addr1), Bytes_FunPtr (f2, addr2) ->
		f1 = f2
	| _, _ ->
		false


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
let byte__make c = make_Byte_Concrete c;;
let byte__zero = byte__make ('\000');;
let byte__111 = byte__make ('\255');;
let byte__random () = byte__make (char__random ());;
let byte__symbolic () = make_Byte_Symbolic (symbol__next ());;


(**
 *  guard
 *)
let guard__true = Guard_True

let guard__not = function
	| Guard_Not g -> g
	| g -> Guard_Not g

let guard__and g1 g2 = match g1, g2 with
	| Guard_True, g
    | g, Guard_True -> g
	| _, _          -> Guard_And (g1, g2)

let guard__and_not g1 g2 = guard__and g1 (guard__not g2)

let guard__symbolic () = Guard_Symbolic (symbol__next ())

let guard__bytes b = Guard_Bytes b


(**
 *	bytes
 *)
let bytes__zero = make_Bytes_Constant(Cil.CInt64(0L,IInt,None));;
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

(* given a list of bytes of length n, return a IfThenElse tree of height log(n) containing all the bytes in the list *)
let bytes__ifthenelse_from_list bytes_list =
	let rec bytes__make_tree outs = function
		| x::y::rest -> bytes__make_tree (make_Bytes_IfThenElse (guard__symbolic (), x, y)::outs) rest
		| x::[]      -> bytes__make_tree_next (x::outs)
		| []         -> bytes__make_tree_next outs
	and bytes__make_tree_next = function
		| [ x ] -> x
        | []    -> failwith "No bytes in bytes_list!"
		| outs  -> bytes__make_tree [] outs
	in
	bytes__make_tree [] bytes_list
;;

let rec bytes__ifthenelse_from_lvals = function
	| Lval_Block (block, offset) ->
		make_Bytes_Address (Some block, offset)
	| Lval_IfThenElse (c, lvals1, lvals2) ->
		make_Bytes_IfThenElse (c, bytes__ifthenelse_from_lvals lvals1, bytes__ifthenelse_from_lvals lvals2)
;;

let rec bytes__length bytes =
	match bytes with
		| Bytes_Constant (constant) -> (Cil.bitsSizeOf (Cil.typeOf (Const(constant))))/8
		| Bytes_ByteArray (bytearray) -> ImmutableArray.length bytearray
		| Bytes_Address (_,_)-> word__size
		| Bytes_IfThenElse (_,b,_) -> bytes__length b (* all bytes in IfThenElse have the same length *)
		| Bytes_Op (op,(bytes2,typ)::tail) -> bytes__length bytes2
		| Bytes_Op (op,[]) -> 0 (* reachable from diff_bytes *)
		| Bytes_Write(bytes2,_,_,_) -> bytes__length bytes2
		| Bytes_Read(_,_,len) -> len
		| Bytes_FunPtr(_) -> word__size
        | Bytes_Unbounded (_,_,size) ->
            if isConcrete_bytes bytes then bytes_to_int_auto size
            else  max_bytes_size
;;

let rec bytes__get_byte bytes i : byte =
	match bytes with
		| Bytes_Constant (constant) ->  bytes__get_byte (constant_to_bytes constant) i
		| Bytes_ByteArray (bytearray) -> ImmutableArray.get bytearray i 
		| _ -> make_Byte_Bytes(bytes,i)
;;

let rec bytes__read bytes off len =
	if (bytes__length bytes) = len then bytes else
	let worst_case = make_Bytes_Read (bytes,off,len) in
	let ret_bytes = 
		begin match bytes,off with
			| Bytes_ByteArray(array),Bytes_Constant(CInt64(i64,k,_)) -> 
					let i = Int64.to_int i64 in
					make_Bytes_ByteArray (ImmutableArray.sub array i len)
			| Bytes_Constant(constant),Bytes_Constant(CInt64(i64,k,_)) -> 
                    let converted_bytes = constant_to_bytes constant in
                      begin match converted_bytes with
                        | Bytes_ByteArray(array) ->
					        let i = Int64.to_int i64 in
					        make_Bytes_ByteArray (ImmutableArray.sub array i len)
                        | _ -> worst_case
                      end
			| Bytes_Write (bytes2,off2,len2,newbytes),_ -> 
				if off2 = off && len2 = len then
					newbytes (* being a bit tricky... *)
				else (* CAUTION: assume [off2,len2] and [off,len] don't overlap.  *)
					worst_case
            | Bytes_IfThenElse(c,e1,e2),_ ->
								let e1' = bytes__read e1 off len in
								let e2' = bytes__read e2 off len in
								if bytes__equal e1' e2' then
									e1'
								else
									make_Bytes_IfThenElse(c,e1',e2')
            | _,Bytes_IfThenElse(c,e1,e2) ->
								failwith "bytes__read: if-then-else offset doesn't happen"
(*                make_Bytes_IfThenElse(c,bytes__read (bytes) (e1) len,bytes__read (bytes) (e2) len)*)
			| _ -> worst_case
		end
		in
		(* try to inflate any Bytes_ByteArray of Byte_Bytes *)
		match ret_bytes with
			| Bytes_ByteArray(bytearray) ->
					begin match ImmutableArray.get bytearray 0 with
						| Byte_Bytes(condensed_bytes,0) when
								(* Make sure length agrees, and that bytes 1 through
									 len-1 match up. *)
								bytes__length condensed_bytes = len &&
								(let rec fn n =
									 if n >= len then true
									 else
										 match ImmutableArray.get bytearray n with
											 | Byte_Bytes(b,i) when i = n && b == condensed_bytes -> fn (succ n)
											 | _ -> false
								 in fn 1)
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
					make_Bytes_ByteArray(impl (len-1) oldarray)
					
			| Bytes_ByteArray(oldarray),Bytes_Constant(CInt64(i64,k,_)),Bytes_Constant(const) ->
				do_write bytes off len (constant_to_bytes const)
				
			| Bytes_ByteArray(oldarray),Bytes_Constant(CInt64(i64,k,_)),_(* anything *) ->
				let rec impl arr i =
					if i>=len then arr else
						impl (ImmutableArray.set arr i (make_Byte_Bytes(newbytes,i))) (i+1)
				in
					do_write bytes off len (make_Bytes_ByteArray(impl (ImmutableArray.make len byte__zero) 0))			
			
			| Bytes_ByteArray(oldarray),_,_
				when isConcrete_bytes off ->
					let n_off = bytes_to_constant off Cil.intType in
					do_write bytes (make_Bytes_Constant(n_off)) len newbytes

			(* Without this next case, writing to a constant would introduce
				 a Bytes_Write. Aside from not wanting a Bytes_Write if we can
				 avoid it (for example, writing a concrete byte to the first
				 byte of a concrete int), this could cause problems. The
				 potential problem has to do with writing past the end of an
				 array that is represented as a Bytes_Constant (which could
				 exist if, for example, you have a 4-byte ByteArray and write
				 a Bytes_Constant int to it). *)
			| Bytes_Constant c,_,_ ->
					do_write (constant_to_bytes c) off len newbytes



            | Bytes_IfThenElse(c,e1,e2),_ ,_ ->
								let e1' = do_write (e1) (off) len newbytes in
								let e2' = do_write (e2) (off) len newbytes in
								if bytes__equal e1' e2' then
									e1'
								else
									make_Bytes_IfThenElse(c,e1',e2')
            | _,Bytes_IfThenElse(c,e1,e2) ,_ ->
								failwith "bytes__write: if-then-else offset doesn't happen"
(*                make_Bytes_IfThenElse(c,do_write (bytes) (e1) len newbytes,do_write (bytes) (e2) len newbytes)*)


			| _ -> make_Bytes_Write (bytes,off,len,newbytes)
	in
	if (bytes__length bytes)=len && (isConcrete_bytes off) && (bytes_to_int_auto off = 0) then 
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


(* given a list of lval_block of length n, return a Lval_May tree of height log(n) containing all the lvals in the list *)
let lval_block__from_list lval_list =
    let rec lval_block__make_tree outs = function
        | x::y::rest -> lval_block__make_tree ((Lval_IfThenElse (guard__symbolic (), x, y))::outs) rest
        | x::[]      -> lval_block__make_tree_next (x::outs)
        | []         -> lval_block__make_tree_next outs
    and lval_block__make_tree_next = function
        | [ x ] -> x
        | []    -> failwith "No lvals in lval_list!"
        | outs  -> lval_block__make_tree [] outs
    in
    lval_block__make_tree [] lval_list
;;

