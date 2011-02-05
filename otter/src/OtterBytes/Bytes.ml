open DataStructures
open OcamlUtilities
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


(** Does the operator return a boolean value (i.e., 0 or 1)? *)
let returnsBoolean = function
	| OP_LT | OP_GE
	| OP_GT | OP_LE
	| OP_EQ | OP_NE
	| OP_LNOT | OP_LAND | OP_LOR -> true
	| _ -> false


module T : sig
    type symbol =
        {
            symbol_id: int;
        }

    type byte = private
        | Byte_Concrete of char
        | Byte_Symbolic of symbol
        | Byte_Bytes of bytes * int

    and guard =
        | Guard_True
        | Guard_Not of guard
        | Guard_And of guard * guard
        | Guard_Symbolic of symbol
        | Guard_Bytes of bytes

    and 'a conditional =
        | IfThenElse of guard * 'a conditional * 'a conditional
        | Unconditional of 'a

    and bytes = private
        | Bytes_Constant of Cil.constant
        | Bytes_ByteArray of byte ImmutableArray.t
        | Bytes_Address of memory_block * bytes
        | Bytes_Op of operator * (bytes * Cil.typ) list
        | Bytes_Read of bytes * bytes * int
        | Bytes_Write of bytes * bytes * int * bytes
        | Bytes_FunPtr of Cil.varinfo * bytes
        | Bytes_Conditional of bytes conditional

    and memory_block_type =
        | Block_type_Const
        | Block_type_Global
        | Block_type_Local
        | Block_type_Heap
        | Block_type_Aliased

    and memory_block =
        {
            memory_block_name : string;
            memory_block_id : int;
            memory_block_size : int;
            memory_block_addr : bytes;
            memory_block_type : memory_block_type;
        }

    and lval_block = (memory_block * bytes) conditional

    val hash_consing_bytes_hits : int ref
    val hash_consing_bytes_misses : int ref

    val make_Byte_Concrete : char -> byte
    val make_Byte_Symbolic : symbol -> byte
    val make_Byte_Bytes : bytes * int -> byte

    val make_Bytes_Constant : Cil.constant -> bytes
    val make_Bytes_ByteArray : byte ImmutableArray.t -> bytes
    val make_Bytes_Address : memory_block * bytes -> bytes
    val make_Bytes_Op : operator * (bytes * Cil.typ) list -> bytes
    val make_Bytes_Read : bytes * bytes * int -> bytes
    val make_Bytes_Write : bytes * bytes * int * bytes -> bytes
    val make_Bytes_FunPtr : Cil.varinfo * bytes -> bytes
    val make_Bytes_Conditional : bytes conditional -> bytes
end = struct
    type symbol =
        {
            symbol_id: int;
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

    and 'a conditional =
        | IfThenElse of guard * 'a conditional * 'a conditional  (* if guard then a else b *)
        | Unconditional of 'a

    and bytes =
        | Bytes_Constant of Cil.constant                (* length=Cil.sizeOf (Cil.typeOf (Const(constant))) *)
        | Bytes_ByteArray of byte ImmutableArray.t      (* content *)
        | Bytes_Address of memory_block * bytes  (* block, offset *)
        | Bytes_Op of operator * (bytes * Cil.typ) list
        | Bytes_Read of bytes * bytes * int             (* less preferrable type *)
        | Bytes_Write of bytes * bytes * int * bytes    (* least preferrable type*)
        | Bytes_FunPtr of Cil.varinfo * bytes            (* bytes is the "imaginary address" of the funptr *)
        | Bytes_Conditional of bytes conditional

    and memory_block_type =
        | Block_type_Const
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

    let hash_consing_bytes_hits = ref 0
    let hash_consing_bytes_misses = ref 0
    let hash_consing_bytes_init_size = 1000000
    let hash_consing_bytes_tbl : (bytes, bytes) Hashtbl.t = Hashtbl.create hash_consing_bytes_init_size
    let hash_consing_bytes_create bs = 
        try
            let rv = Hashtbl.find hash_consing_bytes_tbl bs in
            incr hash_consing_bytes_hits;
            rv
        with Not_found ->
            Hashtbl.add hash_consing_bytes_tbl bs bs;
            incr hash_consing_bytes_misses;
            bs

    (*
     *  Since bytes objects are immutable, and bytes is private type, all *bs* are
     *  created by calling make_Bytes_* and do not require hash consing check.
     *)
    let make_Byte_Concrete c = Byte_Concrete c
    let make_Byte_Symbolic s = Byte_Symbolic s
    let make_Byte_Bytes (bs, n) = Byte_Bytes (bs, n)
    let make_Bytes_Constant const = hash_consing_bytes_create (Bytes_Constant const)
    let make_Bytes_ByteArray bytearray = hash_consing_bytes_create (Bytes_ByteArray bytearray)
    let make_Bytes_Address (block, bs) = hash_consing_bytes_create (Bytes_Address (block, bs))
    let make_Bytes_Op (op, lst) = hash_consing_bytes_create (Bytes_Op (op, lst))
    let make_Bytes_Read (src, off, len) = hash_consing_bytes_create (Bytes_Read (src, off, len))
    let make_Bytes_Write (des, off, n, src) = hash_consing_bytes_create (Bytes_Write (des, off, n, src))
    let make_Bytes_FunPtr (f, bs) = hash_consing_bytes_create (Bytes_FunPtr (f, bs))
    let make_Bytes_Conditional = function
        | Unconditional b -> b
        | c -> hash_consing_bytes_create (Bytes_Conditional c)
end

include T


let ikind_to_len_isSigned ikind =
	(bitsSizeOf (TInt (ikind, [])) / 8, isSigned ikind)


(**
 *	to bytes
 *)


(** Convert an ocaml (signed) int to make_Bytes_Constant(CInt64(int64,IInt,None)) *)
let int_to_bytes n : bytes =
	make_Bytes_Constant (CInt64 (Int64.of_int n, IInt, None))
	

(* Make this lazy so that upointType is set correctly (by initCIL) before it is evaluated *)
let kindOfUpointType = lazy (match !upointType with
		TInt (kind, _) -> kind
	| _ -> failwith "upointType is not set properly")


(* Convert an int n to [Bytes_Constant (CInt64 (Int64.of_int n, kindOfUpointType, None))] *)
let int_to_offset_bytes (n : int) : bytes =
	make_Bytes_Constant (CInt64 (Int64.of_int n, Lazy.force kindOfUpointType, None))
	

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


(** Convert real numbers to make_Bytes *)
(* TODO: actually represent the numbers *)
let float_to_bytes f fkind =
	let length = bitsSizeOf (TFloat (fkind, [])) / 8 in
	make_Bytes_ByteArray (ImmutableArray.make length (make_Byte_Concrete '\000'))

(** Convert constant to make_Bytes_ByteArray *)
let rec constant_to_bytes constant : bytes =
	match constant with
		| CInt64 (n64, ikind, _) -> int64_to_bytes n64 ikind
		| CStr str -> string_to_bytes str
		| CChr char -> constant_to_bytes (Cil.charConstToInt char)
		| CReal (f, fkind, _) -> float_to_bytes f fkind
		| _ -> failwith "constant_to_bytes: unsupported constant type"



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
	

(** Convert a bytes to (signed) int64. Exception if bytes is not concrete int *)
let bytes_to_int64_auto bytes : int64 =
	bytes_to_int64 bytes true
	

(** Convert a bytes to (signed) ocaml int.
	Warning if bytes represents number out of int's range.
	Exception if bytes is not concrete int *)
let bytes_to_int_auto bytes : int =
	let n64 = bytes_to_int64_auto bytes in
	let res = Int64.to_int n64 in
	if n64 <> Int64.of_int res then
		Errormsg.warn "Int64 %Ld is being truncated to an int" n64;
	res
	

(** Convert a bytes to boolean. Exception if bytes is not concrete *int* *)
let bytes_to_bool bytes : bool =
	let n64 = bytes_to_int64_auto bytes in
		n64 <> 0L
	

(** Convert a bytes of typ to Cil constant. Exception if bytes is not concrete *)
let rec bytes_to_constant bytes typ : Cil.constant =
	match unrollType typ with
		| TInt(ikind,_)->
			let (len,isSigned) = ikind_to_len_isSigned ikind in
			let n64 = bytes_to_int64 bytes isSigned in
			let exp =	Cil.kinteger64 ikind n64 in
				(match exp with Const(const) -> const | _ -> failwith "unreachable")
		| TFloat(fkind,_) -> (*TMP*) CReal(0.1,fkind,None)
		| TEnum (enuminf,_) ->
				(* An enum has type int. [Standard 6.7.2.2.2, but I'm confused by 6.7.2.2.4] *)
				bytes_to_constant bytes Cil.intType
		| TPtr _ -> bytes_to_constant bytes !Cil.upointType  
		| t ->
			begin match bytes with
				| Bytes_Constant(c) -> c
				| _ ->
				failwith ("bytes_to_constant: "^(Pretty.sprint 50 (Cil.d_type () t)))
			end

(** True if bytearray is concrete *)
(* Shouldn't a make_Byte_Bytes with concrete values be considered concrete, too? *)
let isConcrete_bytearray (bytearray : byte ImmutableArray.t) =
	ImmutableArray.for_all (function Byte_Concrete _ -> true | _ -> false) bytearray
	

(** True if bytes is concrete *)
let rec isConcrete_bytes (bytes : bytes) =
	match bytes with
		| Bytes_Constant (_) -> true
		| Bytes_ByteArray (bytearray) -> isConcrete_bytearray bytearray
		| _ -> false
	


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



(* A single global byte representing uninitialized memory *)
let byte__undef = make_Byte_Symbolic { symbol_id = 0 }


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
		| Bytes_Conditional c ->
			(* all bytes in Bytes_Conditional have the same length *)
			let rec find_one = function
				| IfThenElse (_, c1, c2) -> max (find_one c1) (find_one c2)
				| Unconditional b -> bytes__length b
			in
			find_one c


(**
 *  symbol
 *)
(* negative id is used as special symbolic values.
   0 is used for the symbolic byte representing uninitialized memory *)
let symbol__currentID = Counter.make ~start:1 ()
let symbol__next () = 
	{	
		symbol_id = Counter.next symbol__currentID;
	} 

let char__random () = Char.chr ((Random.int 255)+1)

(**
 *	byte 
 *)
let byte__make c = make_Byte_Concrete c
let byte__zero = byte__make ('\000')
let byte__111 = byte__make ('\255')
let byte__random () = byte__make (char__random ())
let byte__symbolic () = make_Byte_Symbolic (symbol__next ())


(**
 *	bytes
 *)
let bytes__zero = make_Bytes_Constant(Cil.CInt64(0L,IInt,None))
let bytes__one = make_Bytes_Constant(Cil.CInt64(1L,IInt,None))
let bytes__of_list (lst: byte list) =	make_Bytes_ByteArray (ImmutableArray.of_list lst) 
let bytes__make_default n byte = make_Bytes_ByteArray(ImmutableArray.make n byte)
let bytes__make n = bytes__make_default n byte__zero
let bytes__random n =
	let rec impl i arr =
		if i>=n then arr else
			impl (i+1) (ImmutableArray.set arr i (byte__random ()))
		in
	make_Bytes_ByteArray(impl 0 (ImmutableArray.make n byte__zero))


let bytes__symbolic n =
	let rec impl len = 
		if len <= 0 then [] else (byte__symbolic ())::(impl (len-1))
	in
		bytes__of_list (impl n)


let rec bytes__get_byte bytes i : byte =
	match bytes with
		| Bytes_Constant (constant) ->  bytes__get_byte (constant_to_bytes constant) i
		| Bytes_ByteArray (bytearray) -> ImmutableArray.get bytearray i 
		| _ -> make_Byte_Bytes(bytes,i)



(**
 *	memory block
 *)
let block__current_id = Counter.make ~start:1 ()
let block__make name n t =
	{
		memory_block_name = name;
		memory_block_id = Counter.next block__current_id;
		memory_block_size = n;
		memory_block_addr = bytes__random (bitsSizeOf voidPtrType / 8);
		memory_block_type = t;
	}


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
	| Bytes_Op(OP_LNOT,[bytes,_]) when isBoolean bytes -> bytes
	| bytes -> make_Bytes_Op(OP_LNOT,[(bytes, Cil.intType)])

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
	| _, _ -> Guard_And (g1, g2)

let guard__and_not g1 g2 = guard__and g1 (guard__not g2)

let guard__symbolic () = Guard_Symbolic (symbol__next ())

let guard__bytes b = Guard_Bytes b

let guard__to_bytes = function
	| Guard_True -> bytes__one
	| Guard_Not Guard_True -> bytes__zero
	| Guard_Bytes b -> b
	| g -> make_Bytes_Conditional (IfThenElse (g, Unconditional bytes__one, Unconditional bytes__zero))


(**
 *  conditional
 *)

(** Fold and map simultaneously over the leaves of conditionals, optionally removing leaves.
    @param test is an optional test function to filter by the guard condition : [guard -> guard -> Ternary.t]
    @param eq is an optional equality function to prune identical leaves : ['target -> 'target -> bool]
    @param pre is an optional precondition
    @param fold_map_opt is the fold and map function, which may map to [None] to remove leaves
            : ['acc -> guard -> 'source -> 'acc * 'target conditional option]
    @param acc is the initial accumulator
    @param source is the original conditional to map from
    @return [('acc, 'target conditional option)] the final accumulator and mapped conditional, which may be [None]
            if all leaves were removed
*)
let conditional__fold_map_opt ?(test=fun _ _ -> Ternary.Unknown) ?(eq=(==)) ?(pre=Guard_True) fold_map_opt acc source =
    let rec conditional__fold_map_opt acc pre = function
        | IfThenElse (guard, x, y) ->
            begin match test pre guard with
                | Ternary.True ->
                    (* test pre ==> guard *)
                    conditional__fold_map_opt acc pre x
                | Ternary.False ->
                    (* test pre ==> !guard *)
                    conditional__fold_map_opt acc pre y
                | Ternary.Unknown ->
                    let acc, x_opt = conditional__fold_map_opt acc (guard__and pre guard) x in
                    let acc, y_opt = conditional__fold_map_opt acc (guard__and_not pre guard) y in
                    match x_opt, y_opt with
                        | Some x, Some y when conditional__equal eq x y ->
                            (* prune away unnecessary IfThenElse *)
                            (acc, x_opt)
                        | Some x, Some y ->
                            (acc, Some (IfThenElse (guard, x, y)))
                        | x_opt, None ->
                            (acc, x_opt)
                        | None, y_opt ->
                            (acc, y_opt)
            end
        | Unconditional x ->
            fold_map_opt acc pre x
    in
    conditional__fold_map_opt acc pre source


(** Fold and map simultaneously over the leaves of conditionals.
    @param test is an optional test function to filter by the guard condition : [guard -> guard -> Ternary.t]
    @param eq is an optional equality function to prune identical leaves : ['target -> 'target -> bool]
    @param pre is an optional precondition
    @param fold_map is the fold and map function : ['acc -> guard -> 'source -> 'acc * 'target conditional]
    @param acc is the initial accumulator
    @param source is the original conditional to map from
    @return [('acc, 'target conditional)] the final accumulator and mapped conditional
*)
let conditional__fold_map ?test ?eq ?pre fold_map acc source =
    let fold_map_opt acc pre x =
        let acc, x = fold_map acc pre x in
        (acc, Some x)
    in
    let acc, result_opt = conditional__fold_map_opt ?test ?eq ?pre fold_map_opt acc source in
    match result_opt with
        | Some result -> (acc, result)
        | None -> failwith "Impossible!" (* because fold_map_opt never returns [(_, None)] *)


(** Fold over the leaves of conditionals.
    @param test is an optional test function to filter by the guard condition : [guard -> guard -> Ternary.t]
    @param pre is an optional precondition
    @param fold is the fold function : ['acc -> guard -> 'source -> 'acc]
    @param acc is the initial accumulator
    @param source is the conditional to fold over
    @return ['acc] the final accumulator
*)
let conditional__fold ?test ?pre fold acc source =
	fst (conditional__fold_map ?test ?pre (fun acc pre x -> (fold acc pre x, Unconditional x)) acc source)


(** Map over the leaves of conditionals.
    @param test is an optional test function to filter by the guard condition : [guard -> guard -> Ternary.t]
    @param eq is an optional equality function to prune identical leaves : ['target -> 'target -> bool]
    @param pre is an optional precondition
    @param map is the map function : ['source -> 'target conditional]
    @param source is the original conditional to map from
    @return ['target conditional] the mapped conditional
*)
let conditional__map ?test ?eq ?pre map source =
	snd (conditional__fold_map ?test ?eq ?pre (fun () _ x -> ((), map x)) () source)


(** Prune the leaves of conditionals.
    @param test is the function to filter by the guard condition : [guard -> guard -> Ternary.t]
    @param eq is an optional equality function to prune identical leaves : ['target -> 'target -> bool]
    @param pre is an optional precondition
    @param source is the conditional to fold over
    @return ['acc] the pruned conditional
*)
let conditional__prune ~test ?eq ?pre source =
	snd (conditional__fold_map ~test ?eq ?pre (fun () _ x -> ((), Unconditional x)) () source)


(** Given a list of length n, return a conditional tree of height log(n) containing all items in the list.
    @param list is a list of items : ['item]
    @return ['item conditional] a conditional tree of items
*)
let conditional__from_list list =
	ListPlus.foldm (fun x y -> IfThenElse (guard__symbolic (), x, y)) list


let conditional__bytes = function
	| Bytes_Conditional c -> c
	| b -> Unconditional b


let conditional__lval_block l =
	Unconditional l

