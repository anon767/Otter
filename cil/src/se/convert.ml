open Cil
open Types


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
(*
let bytearray_to_strrep ba =
	ImmutableArray.fold_left 
		(fun s b -> match b with
			| Byte_Concrete(c) -> (Printf.sprintf "%02X" (Char.code c))^s
			| _ -> failwith "bytearray_to_strrep: ba not concrete"
		) 
		"" ba
	;;

let rec strrep_to_bytearray s =
	let len = String.length s in
	if len = 0 then ImmutableArray.make 0 (Byte_Concrete '\000')
	else if len = 1 then failwith "strrep_to_bytearray: error"
	else let ba = strrep_to_bytearray (String.sub s 2 (len-2)) in
		ImmutableArray.add ba (Byte_Concrete(Char.chr (int_of_string ("0X"^(String.sub s 0 2)))))
	;;
*)
(**
 *	to bytes
 *)

(** Convert constant to Bytes_Constant *)
let lazy_constant_to_bytes constant : bytes =
	Bytes_Constant(constant)
	;;

(** Convert an (int64 of ikind) to Bytes_Constant(CInt64(int64,ikind,None)) *)
let lazy_int64_to_bytes n ikind : bytes =
	lazy_constant_to_bytes (CInt64(n,ikind,None))
	;;

(** Convert an ocaml (signed) int to Bytes_Constant(CInt64(int64,IInt,None)) *)
let lazy_int_to_bytes n : bytes =
	lazy_int64_to_bytes (Int64.of_int n) IInt 
	;;
	
(* Why were there two copies of this?
(** Convert ocaml string to Bytes_ByteArray (not Bytes_Address) *)
let string_to_bytes string : bytes =
	let string_map f s =
		let rec helper i acc =
			if i < 0 then
				acc
			else
				helper (i-1) (f s.[i] :: acc)
		in helper (String.length s - 1) []
	in
		Bytes_ByteArray (ImmutableArray.of_list (string_map (fun ch -> Byte_Concrete ch) (string^"\000")))
	;;
*)

(** Convert in64 of ikind to Bytes_ByteArray. Truncate if needed.  *)
let int64_to_bytes n64 ikind : bytes =
	let (len,isSigned) = ikind_to_len_isSigned ikind in
	let rec helper n acc count =
		if count = len then acc
		else helper (Int64.shift_right n 8)
				(Byte_Concrete (Char.chr ((Int64.to_int n) land 255)) :: acc)
				(count + 1)
	in
	Bytes_ByteArray(ImmutableArray.of_list (List.rev (helper n64 [] 0)))
(* (* Replacing the previous line with this commented chunk prints out what this does *)
let ans = List.rev (helper n64 [] 0)
in Printf.printf "int64 to bytes: %Lx -> " n64;
Printf.printf "%s" (Utility.print_list (function | Byte_Concrete(c) -> Printf.sprintf "%C" c | _ -> failwith "hm") ans ",");
Printf.printf " (length %d)\n" len; Bytes_ByteArray (ImmutableArray.of_list ans)*)
	;;

let string_map f s =
	let rec helper i acc =
		if i < 0 then
			acc
		else
			helper (i-1) (f s.[i] :: acc)
	in helper (String.length s - 1) []

(** Convert CString to Bytes_ByteArray.  *)
let string_to_bytes (s : string) : bytes =
	Bytes_ByteArray (ImmutableArray.of_list (string_map (fun ch -> Byte_Concrete ch) (s^"\000")))
;;

(** Coonvert real numbers to Bytes *)
(* TODO: actually represent the numbers *)
let float_to_bytes f fkind =
	match fkind with
		|	FFloat			(*	float	*) -> Bytes_ByteArray (ImmutableArray.make word__size (Byte_Concrete '\000'))
		|	FDouble			(*	double	*) -> Bytes_ByteArray (ImmutableArray.make (2 * word__size) (Byte_Concrete '\000'))
		|	FLongDouble	(*	long double	*) -> Bytes_ByteArray (ImmutableArray.make (3 * word__size) (Byte_Concrete '\000'))
;;

(** Convert constant to Bytes_ByteArray *)
let rec constant_to_bytes constant : bytes =
	match constant with
		| CInt64(n64,ikind,_) -> int64_to_bytes n64 ikind
		| CStr(str) ->
				let rec impl str =
					if String.length str = 0 then ['\000'] else
						(str.[0]) :: (impl (String.sub str 1 ((String.length str) - 1)))
				in
				let chars = impl str in
				let bytes = Bytes_ByteArray (ImmutableArray.of_list (List.map (fun x -> Byte_Concrete(x)) (chars))) in
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


(** Convert a bytes to its length. (moved from MemOp) *)
let rec bytes_to_length bytes : int =
	match bytes with
		| Bytes_Constant (constant) -> (Cil.bitsSizeOf (Cil.typeOf (Const(constant))))/8
		| Bytes_ByteArray (bytearray) -> ImmutableArray.length bytearray 
		| Bytes_Address (_,_)-> word__size 
		| Bytes_Op (op,(bytes2,typ)::tail) -> bytes_to_length bytes2
		| Bytes_Op (op,[]) -> failwith "Unreachable"
		| Bytes_Write(bytes2,_,_,_) -> bytes_to_length bytes2
		| Bytes_Read(_,_,len) -> len
		| Bytes_FunPtr(_) -> word__size
;;

(** Convert a bytes of (len,isSigned) to int64 of (len,isSigned). 
		If (len,isSigned) == IULongLong then interpret int64 as unsigned. (Same treatment throughout the program).
		Exception if bytes is not concrete int *)
let rec bytes_to_int64 bytes isSigned : int64 = 
	match bytes with
		(* TODO: special case for const==Int64 *)
		| Bytes_Constant (const) -> bytes_to_int64 (constant_to_bytes const) isSigned
		| Bytes_ByteArray (bytearray) -> 
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
(* (* Replacing the previous line with this commented chunk prints out what this does *)
let ans = bytearray_to_int64_helper ((ImmutableArray.length bytearray) - 1) 0L in
Printf.printf "bytes to int64: ";
To_string.bytes_ff Format.str_formatter bytes; Printf.printf "%s" (Format.flush_str_formatter ());
Printf.printf " -> %Lx\n" ans; ans
*)
(* (* This (erroneously) treats the bytes in reverse order. I'm keeping it because
			it caused a segmentation fault when running vsftpd, and this might warrant
			looking into if the problem crops up again. *)
				let attach_byte n64 byte =
					match byte with
					|	Byte_Concrete(c) -> Int64.logor (Int64.shift_left n64 8) (Int64.of_int (Char.code c))
					|	_ -> failwith "bytes_to_int64: bytearray not concrete"
				in
let ans =
				ImmutableArray.fold_left attach_byte 0L bytearray
in Printf.printf "bytes to int64: ";
To_string.bytes_ff Format.str_formatter bytes; Printf.printf "%s" (Format.flush_str_formatter ());
Printf.printf " -> %Lx\n" ans; ans*)
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
		Int64.compare n64 Int64.zero <> 0	
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
				bytes_to_constant bytes (TInt (IInt,[]))
		| t ->	
			begin match bytes with
				| Bytes_Constant(c) -> c
				| _ -> 
				failwith ("bytes_to_constant: "^(Pretty.sprint 50 (Cil.d_type () t)))
			end
	;;

(** Convert a possibly-address bytes to Bytes_Address *)
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
(* Shouldn't a Byte_Bytes with concrete values be considered concrete, too? *)
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
	












(* Might not need this 
(** Convert constant to (int64,ikind), or exception if constant is not of int type *)
let constant_to_int64 constant : int64 = 
	failwith "TODO"
	;;
*)


(*

(* Little endian *)

type t = char array
;;
(*
let lazy_unsigned_int_to_bytes n = 
	Bytes_Constant(CInt64(Int64.of_int n,IUInt,None))
;;
let lazy_int_to_bytes n = 
	Bytes_Constant(CInt64(Int64.of_int n,IInt,None))
;;

let lazy_constant_to_bytes constant = 
	Bytes_Constant(constant)
;;
*)
(* TODO *)
let int64_to_bytes_impl n64 isSigned len =
	(* use isSigned later *)
	let mask255 = 0b11111111L in
	let rec impl n nbytes =
		if nbytes <= 0 then []
		else
			((Char.chr (Int64.to_int (Int64.logand n mask255))))::
			(impl (Int64.shift_right_logical n 8) (nbytes - 1))
	in Bytes_ByteArray (ImmutableArray.of_list (List.map (fun b -> Byte_Concrete (b)) (impl n64 len)))
;;

(** Convert an int64 of kind ikind to bytes *)
let int64_to_bytes n64 ikind =
	match ikind with
		|	IChar				(*	char 	*) -> int64_to_bytes_impl n64 false 1
		|	ISChar			(*	signed char	*) -> int64_to_bytes_impl n64 true 1
		|	IUChar			(*	unsigned char	*) -> int64_to_bytes_impl n64 false 1
		|	IInt				(*	int	*) -> int64_to_bytes_impl n64 true 4
		|	IUInt				(*	unsigned int	*) -> int64_to_bytes_impl n64 false 4
		|	IShort			(*	short	*) -> int64_to_bytes_impl n64 true 2
		|	IUShort			(*	unsigned short	*) -> int64_to_bytes_impl n64 false 2
		|	ILong				(*	long	*) -> int64_to_bytes_impl n64 true 4
		|	IULong			(*	unsigned long	*) -> int64_to_bytes_impl n64 false 4
		|	ILongLong		(*	long long (or _int64 on Microsoft Visual C)	*) -> int64_to_bytes_impl n64 true 8
		|	IULongLong	(*	unsigned long long (or unsigned _int64 on Microsoft Visual C)	*) -> int64_to_bytes_impl n64 false 8
;;

(** Convert an int of kind ikind to bytes *)
let int_to_bytes n ikind =
	int64_to_bytes (Int64.of_int n) ikind
;;

(** Convert a string to a bytes *)
let string_map f s =
	let rec helper i acc =
		if i < 0 then
			acc
		else
			helper (i-1) (f s.[i] :: acc)
	in helper (String.length s - 1) []
;;

let string_to_bytes (s : string) : bytes =
	Bytes_ByteArray (ImmutableArray.of_list (string_map (fun ch -> Byte_Concrete ch) (s^"\000")))
;;

(* TODO *)
let float_to_bytes f fkind =
	match fkind with
		|	FFloat			(*	float	*) -> int_to_bytes 0 Cil.IInt
		|	FDouble			(*	double	*) -> int_to_bytes 0 Cil.ILongLong
		|	FLongDouble	(*	long double	*) -> int_to_bytes 0 Cil.ILongLong
;;


let rec constant_to_bytes constant = (* bytes will be of type Bytes_ByteArray of concrete byte *)
	match constant with
		| CInt64(n64, ikind, _) -> int64_to_bytes n64 ikind
		| CStr(str) ->
				let rec impl str =
					if String.length str = 0 then ['\000'] else
						(str.[0]) :: (impl (String.sub str 1 ((String.length str) - 1)))
				in
				let chars = impl str in
				let bytes = Bytes_ByteArray (ImmutableArray.of_list (List.map (fun x -> Byte_Concrete(x)) (chars))) in
					bytes
		| CChr(char) ->
				constant_to_bytes (Cil.charConstToInt char)
		| CReal (f,fkind,_) -> float_to_bytes f fkind
		| _ -> failwith "from_constant: not supported"
;;
(*
let constant_to_bytearray const = 
	(* TODO: make better *)
	let bytes = constant_to_bytes const in match bytes with
		| Bytes_ByteArray(a) -> a
		| _ -> failwith "nah"
	;;
*)
let bytearray_is_concrete bytearray =
	ImmutableArray.fold_left 
	(fun a b -> a && match b with Byte_Concrete(_)-> true | _ -> false) 
	true bytearray
	;;

(** Convert byte to ocaml int (0-255) *)
let byte_to_int byte : int =
	match byte with
		| Byte_Concrete (c) -> Char.code c
		| _ -> failwith "bytes not integer"
;;

(** Convert bytearray[0:nbytes-1] to ocaml int *) 
let bytearray_to_int array nbytes : int =
	let rec impl array i =
		if i >= nbytes then 0
		else let n = impl array (i + 1) in
		n * 256 + (byte_to_int (ImmutableArray.get array i))
	in
	impl array 0
;;

(** Convert bytes[0:3] to ocaml int *) 
let bytes_to_int bytes : int =
	match bytes with
		| Bytes_Constant(CInt64(n,_,_)) -> Int64.to_int n
		| Bytes_ByteArray (bytearray) -> bytearray_to_int bytearray (ImmutableArray.length bytearray)
		| _ -> failwith "bytes not integer"
;;

let is_concrete bytes =
	try let _ = bytes_to_int bytes in true with Failure (_) -> false
;;

let bytes_to_string bytes: string =
	failwith "not implemented"
;;



*)
