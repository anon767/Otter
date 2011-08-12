open DataStructures
open OcamlUtilities
open CilUtilities
open Cil

type operator =
    (* binop *)
    | OP_PLUS
    | OP_SUB
    | OP_MULT
    | OP_DIV
    | OP_SDIV
    | OP_MOD
    | OP_SMOD
    | OP_LSL
    | OP_LSR
    | OP_ASR
    | OP_LT
    | OP_GT
    | OP_LE
    | OP_GE
    | OP_SLT
    | OP_SGT
    | OP_SLE
    | OP_SGE
    | OP_EQ
    | OP_NE
    | OP_BAND
    | OP_BXOR
    | OP_BOR
    | OP_LAND
    | OP_LOR
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


module rec T : sig
    type symbol = private
        {
            symbol_id: int;
        }

    type byte = private
        | Byte_Undefined
        | Byte_Concrete of char
        | Byte_Symbolic of symbol
        | Byte_Bytes of bytes * int

    and bytearray = ByteArray.t

    and guard = private
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
        | Bytes_Symbolic of symbol
        | Bytes_ByteArray of bytearray
        | Bytes_Address of memory_block * bytes
        | Bytes_Op of operator * bytes list
        | Bytes_Sign_Extend of bytes * int
        | Bytes_Zero_Extend of bytes * int
        | Bytes_Read of bytes * bytes * int
        | Bytes_Write of bytes * bytes * int * bytes
        | Bytes_FunPtr of Cil.varinfo
        | Bytes_Conditional of bytes conditional

    and memory_block_type =
        | Block_type_Const
        | Block_type_Global
        | Block_type_Local
        | Block_type_Heap
        | Block_type_Aliased

    and memory_block = private
        {
            memory_block_name : string;
            memory_block_id : int;
            memory_block_size : bytes;
            memory_block_addr : int;
            memory_block_type : memory_block_type;
        }

    and lval_block = (memory_block * bytes) conditional

    val make_Byte_Concrete : char -> byte
    val make_Byte_Symbolic : unit -> byte
    val make_Byte_Bytes : bytes * int -> byte

    val byte__undefined : byte
    val byte__zero : byte

    val guard__true : guard
    val guard__not : guard -> guard
    val guard__and : guard -> guard -> guard
    val guard__symbolic : unit -> guard
    val guard__bytes : bytes -> guard

    val make_Bytes_Constant : Cil.constant -> bytes
    val make_Bytes_Symbolic : unit -> bytes
    val make_Bytes_ByteArray : bytearray -> bytes
    val make_Bytes_Address : memory_block * bytes -> bytes
    val make_Bytes_Op : operator * bytes list -> bytes
    val make_Bytes_Sign_Extend : bytes * int -> bytes
    val make_Bytes_Zero_Extend : bytes * int -> bytes
    val make_Bytes_Read : bytes * bytes * int -> bytes
    val make_Bytes_Write : bytes * bytes * int * bytes -> bytes
    val make_Bytes_FunPtr : Cil.varinfo -> bytes
    val make_Bytes_Conditional : bytes conditional -> bytes

    val block__make : string -> bytes -> memory_block_type -> memory_block

    module type HashedType = sig
        type t
        val equal : t -> t -> bool
        val hash : t -> int
    end
    module SymbolType : HashedType with type t = symbol
    module ByteType : HashedType with type t = byte
    module ByteArrayType : HashedType with type t = bytearray
    module GuardType : HashedType with type t = guard
    module ConditionalType : functor (Data : HashedType) -> HashedType with type t = Data.t conditional
    module ConditionalPolyType : sig
        type 'a t = 'a conditional
        val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
        val hash : ('a -> int) -> 'a t -> int
    end
    module BytesType : HashedType with type t = bytes
    module BlockType : HashedType with type t = memory_block
end = struct
    type symbol =
        {
            symbol_id: int;
        }

    type byte = (* corresponds to BV *)
        | Byte_Undefined
        | Byte_Concrete of char
        | Byte_Symbolic of symbol
        | Byte_Bytes of bytes * int (* condense a bytes into a byte, that can be put into an array *)

    and bytearray = ByteArray.t

    and guard =
        | Guard_True
        | Guard_Not of guard
        | Guard_And of guard * guard
        | Guard_Symbolic of symbol
        | Guard_Bytes of bytes

    and 'a conditional =
        | IfThenElse of guard * 'a conditional * 'a conditional  (* if guard then a else b *)
        | Unconditional of 'a

    (* TODO: merge Bytes_Symbolic and Bytes_ByteArray by making Bytes_ByteArray a sparse array that defaults to pure symbolic *)
    and bytes =
        | Bytes_Constant of Cil.constant                (* length=Cil.sizeOf (Cil.typeOf (Const(constant))) *)
        | Bytes_Symbolic of symbol                      (* arbitrary-length symbolic bytes *)
        | Bytes_ByteArray of bytearray                  (* content *)
        | Bytes_Address of memory_block * bytes         (* block, offset *)
        | Bytes_Op of operator * bytes list
        | Bytes_Sign_Extend of bytes * int
        | Bytes_Zero_Extend of bytes * int
        | Bytes_Read of bytes * bytes * int             (* less preferrable type *)
        | Bytes_Write of bytes * bytes * int * bytes    (* least preferrable type*)
        | Bytes_FunPtr of Cil.varinfo
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
            memory_block_size : bytes;
            memory_block_addr : int;
            memory_block_type : memory_block_type;
        }

    and lval_block = (memory_block * bytes) conditional


    (**/**)
    module Internal = struct
        (* structural equality helpers for symbols/byte/guards/conditionals/bytes:
         *  - the constant in Bytes_Constant is preserved;
         *  - the types of Bytes_Op operands are preserved.
         *)
        let rec symbol_equal symbol1 symbol2 = symbol1 = symbol2

        and byte_equal byte1 byte2 = byte1 == byte2 || match byte1, byte2 with
            | Byte_Concrete x, Byte_Concrete y -> x = y
            | Byte_Symbolic x, Byte_Symbolic y -> symbol_equal x y
            | Byte_Bytes (b1, off1), Byte_Bytes (b2, off2) -> off1 = off2 && bytes_equal b1 b2
            | _, _ -> false

        and bytearray_equal bytearray1 bytearray2 = ByteArray.equal bytearray1 bytearray2

        and guard_equal guard1 guard2 = guard1 == guard2 || match guard1, guard2 with
            | Guard_Not g1, Guard_Not g2 -> guard_equal g1 g2
            | Guard_And (g1, g2), Guard_And (g1', g2') -> guard_equal g1 g1' && guard_equal g2 g2'
            | Guard_Symbolic s1, Guard_Symbolic s2 -> symbol_equal s1 s2
            | Guard_Bytes b1, Guard_Bytes b2 -> bytes_equal b1 b2
            | _, _ -> false

        and conditional_equal : 'a . ('a -> 'a -> bool) -> 'a conditional -> 'a conditional -> bool =
            fun eq c1 c2 -> c1 == c2 || match c1, c2 with
                | Unconditional x1, Unconditional x2 ->
                    eq x1 x2
                | IfThenElse (g1, x1, y1), IfThenElse (g2, x2, y2) ->
                    guard_equal g1 g2 && conditional_equal eq x1 x2 && conditional_equal eq y1 y2
                | _, _ -> false

        and bytes_equal bytes1 bytes2 = bytes1 == bytes2 || match bytes1, bytes2 with
            | Bytes_Constant c1, Bytes_Constant c2 ->
                c1 = c2
            | Bytes_Symbolic s1, Bytes_Symbolic s2 ->
                symbol_equal s1 s2
            | Bytes_ByteArray a1, Bytes_ByteArray a2 ->
                bytearray_equal a1 a2
            | Bytes_Address(b1, off1),Bytes_Address(b2, off2) ->
                block_equal b1 b2 && bytes_equal off1 off2
            | Bytes_Op (op1, operands1), Bytes_Op (op2, operands2) ->
                op1 = op2 && List.for_all2 bytes_equal operands1 operands2
            | Bytes_Sign_Extend (value1, width1), Bytes_Sign_Extend (value2, width2) ->
                width1 = width2 && bytes_equal value1 value2
            | Bytes_Zero_Extend (b1, s1), Bytes_Zero_Extend (b2, s2) ->
                s1 = s2 && bytes_equal b1 b2
            | Bytes_Read (b1, off1, s1), Bytes_Read (b2, off2, s2) ->
                s1 = s2 && bytes_equal b1 b2 && bytes_equal off1 off2
            | Bytes_Write (old1, off1, s1, new1), Bytes_Write (old2, off2, s2, new2) ->
                s1 = s2 && bytes_equal old1 old2 && bytes_equal off1 off2 && bytes_equal new1 new2
            | Bytes_FunPtr f1, Bytes_FunPtr f2 ->
                CilData.CilVar.equal f1 f2
            | Bytes_Conditional c1, Bytes_Conditional c2 ->
                conditional_equal bytes_equal c1 c2
            | _, _ ->
                false

        and block_equal block1 block2 = block1 == block2 ||
            block1.memory_block_id = block2.memory_block_id
            && block1.memory_block_type = block2.memory_block_type
            && block1.memory_block_addr = block2.memory_block_addr
            && block1.memory_block_name = block2.memory_block_name
            && bytes_equal block1.memory_block_size block2.memory_block_size


        (* structural hash helpers for symbols/byte/guards/conditionals/bytes:
         *  - the constant in Bytes_Constant is considered;
         *  - the types of Bytes_Op operands are considered.
         *)
        exception Limit
        let limit = 20
        let hash = ref 0
        let count = ref 0
        let add_hash x =
            if !count >= limit then raise Limit;
            hash := Hashtbl.hash (x, !hash);
            incr count

        let do_hash ha x =
            let hash', count' = !hash, !count in
            hash := 0; count := 0;
            (try ha x with Limit -> ());
            let hash'' = !hash in
            hash := hash'; count := count';
            hash''

        let rec symbol_hash = add_hash
        
        and byte_hash = function
            | Byte_Undefined | Byte_Concrete _ | Byte_Symbolic _ as b -> add_hash b
            | Byte_Bytes (bytes, offset) -> add_hash (`Bytes, offset); bytes_hash bytes

        and bytearray_hash bytearray = add_hash (ByteArray.hash bytearray)

        and guard_hash = function
            | Guard_True -> add_hash `True
            | Guard_Not g -> add_hash `Not; guard_hash g
            | Guard_And (g1, g2) -> add_hash `And; guard_hash g1; guard_hash g2
            | Guard_Symbolic s -> add_hash `Symbolic; symbol_hash s
            | Guard_Bytes b -> add_hash `Bytes; bytes_hash b

        and conditional_hash : 'a . ('a -> unit) -> 'a conditional -> unit =
            fun ha c -> match c with
                | Unconditional x -> add_hash `Unconditional; ha x
                | IfThenElse (g, x, y) -> add_hash `IfThenElse; guard_hash g; conditional_hash ha x; conditional_hash ha y

        and bytes_hash = function
            | Bytes_Constant c -> add_hash (`Constant, c)
            | Bytes_Symbolic s -> add_hash `Symbolic; symbol_hash s
            | Bytes_ByteArray a -> add_hash `ByteArray; bytearray_hash a
            | Bytes_Address (a, o) -> add_hash `Address; block_hash a; bytes_hash o
            | Bytes_Op (op, operands) -> add_hash (`Op, op); List.iter bytes_hash operands
            | Bytes_Sign_Extend (value, width) -> add_hash (`Sign_Extend, width); bytes_hash value
            | Bytes_Zero_Extend (value, width) -> add_hash (`Zero_Extend, width); bytes_hash value
            | Bytes_Read (bytes, offset, size) -> add_hash (`Read, size); bytes_hash bytes; bytes_hash offset
            | Bytes_Write (bytes, offset, size, value) -> add_hash (`Write, size); bytes_hash bytes; bytes_hash offset; bytes_hash value
            | Bytes_FunPtr f -> add_hash (`FunPtr, CilData.CilVar.hash f)
            | Bytes_Conditional c -> add_hash `Conditional; conditional_hash bytes_hash c

        and block_hash block =
            add_hash (block.memory_block_name, block.memory_block_addr, block.memory_block_type, block.memory_block_id);
            bytes_hash block.memory_block_size
    end
    (**/**)


    (**
     *  symbol
     *)
    let symbol__next =
        let symbol_counter = Counter.make () in
        fun () -> { symbol_id = Counter.next symbol_counter }


    (**
     *  byte
     *)
    let hash_consing_byte_create =
        let module Memo = Memo.Make (struct
            type t = byte
            let equal = Internal.byte_equal
            let hash = Internal.do_hash Internal.byte_hash
        end) in
        Memo.make_hashcons "Bytes.make_byte"

    let make_Byte_Concrete c = hash_consing_byte_create (Byte_Concrete c)

    let make_Byte_Symbolic () = Byte_Symbolic (symbol__next ())

    let make_Byte_Bytes (bs, n) = hash_consing_byte_create (Byte_Bytes (bs, n))

    let byte__undefined = Byte_Undefined

    let byte__zero = make_Byte_Concrete '\000'


    (**
     *  guard
     *)
    let hash_consing_guard_create =
        let module Memo = Memo.Make (struct
            type t = guard
            let equal = Internal.guard_equal
            let hash = Internal.do_hash Internal.guard_hash
        end) in
        Memo.make_hashcons "Bytes.make_guard"

    let guard__true = Guard_True

    let guard__not = function
        | Guard_Not g -> g
        | g -> hash_consing_guard_create (Guard_Not g)

    let guard__and g1 g2 = match g1, g2 with
        | Guard_True, g
        | g, Guard_True -> g
        | Guard_Not Guard_True, _
        | _, Guard_Not Guard_True -> guard__not guard__true
        | _, _ -> hash_consing_guard_create (Guard_And (g1, g2))

    let guard__symbolic () = Guard_Symbolic (symbol__next ())

    let guard__bytes b = hash_consing_guard_create (Guard_Bytes b)


    (**
     *  bytes
     *)
    let hash_consing_bytes_create =
        let module Memo = Memo.Make (struct
            type t = bytes
            let equal = Internal.bytes_equal
            let hash = Internal.do_hash Internal.bytes_hash
        end) in
        Memo.make_hashcons "Bytes.make_bytes"

    let make_Bytes_Constant const =
        Profiler.global#call "Bytes.make_Bytes_Constant" begin fun () ->
            hash_consing_bytes_create (Bytes_Constant const)
        end

    let make_Bytes_Symbolic () =
        Bytes_Symbolic (symbol__next ())

    let make_Bytes_ByteArray bytearray =
        Profiler.global#call "Bytes.make_Bytes_ByteArray" begin fun () ->
            hash_consing_bytes_create (Bytes_ByteArray bytearray)
        end

    let make_Bytes_Address (block, bs) =
        Profiler.global#call "Bytes.make_Bytes_Address" begin fun () ->
            hash_consing_bytes_create (Bytes_Address (block, bs))
        end

    let make_Bytes_Op (op, lst) =
        Profiler.global#call "Bytes.make_Bytes_Op" begin fun () ->
            hash_consing_bytes_create (Bytes_Op (op, lst))
        end

    let make_Bytes_Sign_Extend (value, width) =
        Profiler.global#call "Bytes.make_Bytes_Sign_Extend" begin fun () ->
            hash_consing_bytes_create (Bytes_Sign_Extend (value, width))
        end

    let make_Bytes_Zero_Extend (value, width) =
        Profiler.global#call "Bytes.make_Bytes_Zero_Extend" begin fun () ->
            hash_consing_bytes_create (Bytes_Zero_Extend (value, width))
        end

    let make_Bytes_Read (src, off, len) =
        Profiler.global#call "Bytes.make_Bytes_Read" begin fun () ->
            hash_consing_bytes_create (Bytes_Read (src, off, len))
        end

    let make_Bytes_Write (des, off, n, src) =
        Profiler.global#call "Bytes.make_Bytes_Write" begin fun () ->
            hash_consing_bytes_create (Bytes_Write (des, off, n, src))
        end

    let make_Bytes_FunPtr f =
        if not (Cil.isFunctionType f.Cil.vtype) then
            FormatPlus.invalid_arg "not a function: %a" CilPrinter.varinfo f;
        hash_consing_bytes_create (Bytes_FunPtr f)

    let make_Bytes_Conditional = function
        | Unconditional b -> b
        | c -> hash_consing_bytes_create (Bytes_Conditional c)


    (**
     *  memory block
     *)
    let block__make =
        let block_counter = Counter.make ~start:1 () in
        let segment_size = 0x10000 in (* TODO: this is unsound *)
        let block_addr_counter = Counter.make ~start:segment_size () in
        fun memory_block_name memory_block_size memory_block_type ->
            {
                memory_block_name;
                memory_block_size;
                memory_block_type;
                memory_block_id = Counter.next block_counter;
                memory_block_addr = Counter.next ~v:segment_size block_addr_counter;
            }


    (**
     *  Modularized types
     *)

    module type HashedType = sig
        type t
        val equal : t -> t -> bool
        val hash : t -> int
    end

    module SymbolType = struct
        type t = symbol
        let equal = Pervasives.(==) (* not hash-cons'ed, but is private *)
        let hash = Internal.do_hash Internal.symbol_hash
    end

    module ByteType = struct
        type t = byte
        let equal = Pervasives.(==)
        let hash = Internal.do_hash Internal.byte_hash
    end

    module ByteArrayType = ByteArray

    module GuardType = struct
        type t = guard
        let equal = Pervasives.(==)
        let hash = Internal.do_hash Internal.guard_hash
    end

    module ConditionalType (Data : HashedType) = struct
        type t = Data.t conditional
        let equal = Internal.conditional_equal Data.equal (* not hash-cons'ed *)
        let hash = Internal.do_hash (Internal.conditional_hash (fun x -> Internal.add_hash (Data.hash x)))
    end

    module ConditionalPolyType = struct
        type 'a t = 'a conditional
        let equal eq = Internal.conditional_equal eq
        let hash ha = Internal.do_hash (Internal.conditional_hash (fun x -> Internal.add_hash (ha x)))
    end

    module BytesType = struct
        type t = bytes
        let equal = Pervasives.(==)
        let hash = Internal.do_hash Internal.bytes_hash
    end

    module BlockType = struct
        type t = memory_block
        let equal = Pervasives.(==) (* not hash-cons'ed, but is private *)
        let hash = Internal.do_hash Internal.block_hash
    end
end
and ByteArray : ImmutableArray.S with type elt = ByteArrayElement.t =
    ImmutableArray.Make (ByteArrayElement)
    (* can't use T.ByteType.t directly due to "module rec" compiler restriction: recursive dependency cycles must go
     * through a "safe" module that contains only function values *)
and ByteArrayElement : ImmutableArray.ElementType with type t = T.ByteType.t = struct
    include T.ByteType
    let default = lazy T.byte__zero
end

include T


let ikind_to_len_isSigned ikind =
    (bitsSizeOf (TInt (ikind, [])) / 8, isSigned ikind)


(**
 *  to bytes
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
    make_Bytes_ByteArray(ByteArray.of_list (List.rev (helper n64 [] 0))) (* Reverse because we are little-endian *)


let string_map f s =
    let rec helper i acc =
        if i < 0 then
            acc
        else
            helper (i-1) (f s.[i] :: acc)
    in helper (String.length s - 1) []

(** Convert CString to make_Bytes_ByteArray.  *)
let string_to_bytes (s : string) : bytes =
    make_Bytes_ByteArray (ByteArray.of_list (string_map (fun ch -> make_Byte_Concrete ch) (s^"\000")))


(** Convert real numbers to make_Bytes *)
(* TODO: actually represent the numbers *)
let float_to_bytes f fkind =
    let length = bitsSizeOf (TFloat (fkind, [])) / 8 in
    make_Bytes_ByteArray (ByteArray.make length (make_Byte_Concrete '\000'))

(** Convert constant to make_Bytes_ByteArray *)
let rec constant_to_bytes constant : bytes =
    match constant with
        | CInt64 (n64, ikind, _) -> int64_to_bytes n64 ikind
        | CStr str -> string_to_bytes str
        | CChr char -> constant_to_bytes (Cil.charConstToInt char)
        | CReal (f, fkind, _) -> float_to_bytes f fkind
        | _ -> failwith "constant_to_bytes: unsupported constant type"



(**
 *  from bytes
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
                            (match (ByteArray.get bytearray index) with
                            |    Byte_Concrete(c) -> Int64.logor (Int64.shift_left acc 8) (Int64.of_int (Char.code c))
                            |    _ -> failwith "bytes_to_int64: bytearray not concrete")
                in
                bytearray_to_int64_helper ((ByteArray.length bytearray) - 1) 0L
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
            let exp =    Cil.kinteger64 ikind n64 in
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
let isConcrete_bytearray (bytearray : bytearray) =
    ByteArray.for_all (function Byte_Concrete _ -> true | _ -> false) bytearray


(** True if bytes is concrete *)
let rec isConcrete_bytes (bytes : bytes) =
    match bytes with
        | Bytes_Constant (_) -> true
        | Bytes_ByteArray (bytearray) -> isConcrete_bytearray bytearray
        | _ -> false



(**
 *  value reduction for byte/guards/conditionals/bytes
 *)

(**/**)
module InternalReduce = struct
    let byte_wrap =
        let module Memo = Memo.Make (ByteType) in
        Memo.make "Bytes.byte__reduce"

    let guard_wrap =
        let module Memo = Memo.Make (GuardType) in
        Memo.make "Bytes.guard__reduce"

    let bytes_wrap =
        let module Memo = Memo.Make (BytesType) in
        Memo.make "Bytes.bytes__reduce"
end
(**/**)

let rec byte__reduce byte =
    InternalReduce.byte_wrap begin function
        | Byte_Bytes (bytes, offset) -> make_Byte_Bytes (bytes__reduce bytes, offset)
        | Byte_Undefined | Byte_Concrete _ | Byte_Symbolic _ as byte -> byte
    end byte

and guard__reduce guard =
    InternalReduce.guard_wrap begin function
        | Guard_Not guard -> guard__not (guard__reduce guard)
        | Guard_And (guard, guard') -> guard__and (guard__reduce guard) (guard__reduce guard')
        | Guard_Bytes bytes -> guard__bytes (bytes__reduce bytes)
        | Guard_True | Guard_Symbolic _ as guard -> guard
    end guard

and conditional__reduce : 'a . ('a -> 'a) -> 'a conditional -> 'a conditional = fun reduce conditional ->
    let rec conditional__reduce = function
        | Unconditional a -> Unconditional (reduce a)
        | IfThenElse (guard, conditional, conditional') -> IfThenElse (guard__reduce guard, conditional__reduce conditional, conditional__reduce conditional')
    in
    conditional__reduce conditional

and bytes__reduce bytes =
    InternalReduce.bytes_wrap begin function
        | Bytes_Constant c -> constant_to_bytes c
        | Bytes_ByteArray array -> make_Bytes_ByteArray (ByteArray.map (byte__reduce) array)
        | Bytes_Address (block, offset) -> make_Bytes_Address (block, bytes__reduce offset)
        | Bytes_Op (op, operands) -> make_Bytes_Op (op, List.map bytes__reduce operands)
        | Bytes_Sign_Extend (value, width) -> make_Bytes_Sign_Extend (bytes__reduce value, width)
        | Bytes_Zero_Extend (value, width) -> make_Bytes_Zero_Extend (bytes__reduce value, width)
        | Bytes_Read (bytes, offset, size) -> make_Bytes_Read (bytes__reduce bytes, bytes__reduce offset, size)
        | Bytes_Write (bytes, offset, size, bytes') -> make_Bytes_Write (bytes__reduce bytes, bytes__reduce offset, size, bytes__reduce bytes')
        | Bytes_Conditional conditional -> make_Bytes_Conditional (conditional__reduce bytes__reduce conditional)
        | Bytes_Symbolic _ | Bytes_FunPtr _ as bytes -> bytes
    end bytes



(**
 *  value equality for byte/guards/conditionals/bytes
 *)

let byte__equal byte byte' = ByteType.equal byte byte' || ByteType.equal (byte__reduce byte) (byte__reduce byte')
let guard__equal guard guard' = GuardType.equal guard guard' || GuardType.equal (guard__reduce guard) (guard__reduce guard')
let conditional__equal eq conditional conditional' = ConditionalPolyType.equal eq conditional conditional' (* rely on eq to perform reduction *)
let bytes__equal bytes bytes' = BytesType.equal bytes bytes' || BytesType.equal (bytes__reduce bytes) (bytes__reduce bytes')
let block__equal = BlockType.equal


let rec bytes__length bytes =
    match bytes with
        | Bytes_Constant (constant) -> (Cil.bitsSizeOf (Cil.typeOf (Const(constant))))/8
        | Bytes_Symbolic _ -> 0 (* TODO: bytes__length should be deprecated *)
        | Bytes_ByteArray (bytearray) -> ByteArray.length bytearray
        | Bytes_Address (_,_)-> bitsSizeOf voidPtrType / 8
        | Bytes_Op ((OP_LT | OP_GT | OP_LE | OP_GE | OP_EQ | OP_NE | OP_LAND | OP_LOR | OP_LNOT), _) ->
            (* result has type int per C99 6.5.8 Relational operators, 6.5.9 Equality operators, 6.5.13 Logical AND
             * operator, 6.5.14 Logical OR operator, 6.5.3.3 Unary arithmetic operators *)
            bitsSizeOf intType / 8
        | Bytes_Op (op,bytes2::tail) -> bytes__length bytes2
        | Bytes_Op (op,[]) -> 0 (* reachable from diff_bytes *)
        | Bytes_Sign_Extend (_, width) | Bytes_Zero_Extend (_, width) | Bytes_Read (_ ,_ ,width) -> width
        | Bytes_Write(bytes2,_,_,_) -> bytes__length bytes2
        | Bytes_FunPtr(_) -> bitsSizeOf voidPtrType / 8
        | Bytes_Conditional c ->
            (* all bytes in Bytes_Conditional have the same length *)
            let rec find_one = function
                | IfThenElse (_, c1, c2) -> max (find_one c1) (find_one c2)
                | Unconditional b -> bytes__length b
            in
            find_one c


(**
 *    byte
 *)
let byte__make c = make_Byte_Concrete c
let byte__111 = byte__make ('\255')


(**
 *  bytes
 *)
let bytes__zero = make_Bytes_Constant(Cil.CInt64(0L,IInt,None))
let bytes__one = make_Bytes_Constant(Cil.CInt64(1L,IInt,None))
let bytes__of_list (lst: byte list) =    make_Bytes_ByteArray (ByteArray.of_list lst)
let bytes__make_default n byte = make_Bytes_ByteArray(ByteArray.make n byte)
let bytes__make n = bytes__make_default n byte__zero


let bytes__symbolic n =
    let rec impl len =
        if len <= 0 then [] else (make_Byte_Symbolic ())::(impl (len-1))
    in
        bytes__of_list (impl n)


let rec bytes__get_byte bytes i : byte =
    match bytes with
        | Bytes_Constant (constant) ->  bytes__get_byte (constant_to_bytes constant) i
        | Bytes_ByteArray (bytearray) -> ByteArray.get bytearray i
        | _ -> make_Byte_Bytes(bytes,i)



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
    else make_Bytes_Op (OP_LNOT, [ make_Bytes_Op (OP_LNOT, [ bytes ]) ])

(** Remove a NOT from a bytes, if doing so leaves it boolean. Otherwise, add a
    NOT. *)
let logicalNot = function
    | Bytes_Op (OP_LNOT, [ bytes ]) when isBoolean bytes -> bytes
    | bytes -> make_Bytes_Op (OP_LNOT, [ bytes ])


(**
 *  guard
 *)
let guard__and_not g1 g2 = guard__and g1 (guard__not g2)

let guard__to_bytes = function
    | Guard_True -> bytes__one
    | Guard_Not Guard_True -> bytes__zero
    | Guard_Bytes b -> b
    | g -> make_Bytes_Conditional (IfThenElse (g, Unconditional bytes__one, Unconditional bytes__zero))


(**
 *  conditional
 *)

(** Fold and map simultaneously over the leaves of conditionals, optionally removing leaves.
    @param test is an optional test function to filter by the guard condition; the accumulator is passed to the test
            function as well : ['acc -> guard -> guard -> 'acc * Ternary.t]
    @param eq is an optional equality function to prune identical leaves : ['target -> 'target -> bool]
    @param pre is an optional precondition
    @param fold_map_opt is the fold and map function, which may map to [None] to remove leaves
            : ['acc -> guard -> 'source -> 'acc * 'target conditional option]
    @param acc is the initial accumulator
    @param source is the original conditional to map from
    @return [('acc, 'target conditional option)] the final accumulator and mapped conditional, which may be [None]
            if all leaves were removed
*)
let conditional__fold_map_opt ?(test=fun acc _ _ -> (acc, Ternary.Unknown)) ?(eq=(==)) ?(pre=guard__true) fold_map_opt acc source =
    let rec conditional__fold_map_opt acc pre = function
        | IfThenElse (guard, x, y) ->
            let acc, truth = test acc pre guard in
            begin match truth with
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
    @param test is an optional test function to filter by the guard condition; the accumulator is passed to the test
            function as well : ['acc -> guard -> guard -> 'acc * Ternary.t]
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
    @param test is an optional test function to filter by the guard condition; the accumulator is passed to the test
            function as well : ['acc -> guard -> guard -> 'acc * Ternary.t]
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
    let test = match test with Some f -> Some (fun () pre guard -> ((), f pre guard)) | None -> None in
    snd (conditional__fold_map ?test ?eq ?pre (fun () _ x -> ((), map x)) () source)


(** Prune the leaves of conditionals.
    @param test is an optional test function to filter by the guard condition; an accumulator is passed to the test
            function as well : ['acc -> guard -> guard -> 'acc * Ternary.t]
    @param eq is an optional equality function to prune identical leaves : ['target -> 'target -> bool]
    @param pre is an optional precondition
    @param acc is the initial accumulator for the test function
    @param source is the conditional to fold over
    @return [('acc, 'target conditional)] the final accumulator and pruned conditional
*)
let conditional__prune ~test ?eq ?pre acc source =
    conditional__fold_map ~test ?eq ?pre (fun acc _ x -> (acc, Unconditional x)) acc source


(** Given a list of length n, return a conditional tree of height log(n) containing all items in the list.
    @param list is a list of items : ['item]
    @return ['item conditional] a conditional tree of items
*)
let conditional__from_list list =
    ListPlus.foldm (fun x y -> IfThenElse (guard__symbolic (), x, y)) list


(** [conditional__to_list conditional] converts [conditional] into a list of
    {!type:'a}s, each one paired with the {!type:guard} under which
    [conditional] takes that value.
*)
(* We could collapse multiple occurrences of the same value together. Should we? *)
let conditional__to_list conditional =
    conditional__fold (fun result guard bytes -> (guard, bytes) :: result) [] conditional


let conditional__bytes = function
    | Bytes_Conditional c -> c
    | b -> Unconditional b


let conditional__lval_block l =
    Unconditional l

