open DataStructures
open OcamlUtilities
open Cil
open Bytes

module SymbolSet = Set.Make
    (struct
         type t = symbol
         let compare (x : t) y = Pervasives.compare x.symbol_id y.symbol_id
     end)

let print_stp_queries = ref false
let arg_max_stp_time = ref (-0.1) (* negative means unlimited *)
let stp_queries = ref []

(**/**) (* helpers for all_symbols *)
module InternalAllSymbols = struct
    module ByteArrayMemo = Memo.Make (ByteArrayType)
    let wrap_all_symbols_in_bytearray = ByteArrayMemo.make "BytesSTP.all_symbols_in_bytearray"

    module GuardMemo = Memo.Make (GuardType)
    let wrap_all_symbols_in_guard = GuardMemo.make "BytesSTP.all_symbols_in_guard"

    module ConditionalBytesMemo = Memo.Make (ConditionalType (BytesType))
    let wrap_all_symbols_in_conditional_bytes = ConditionalBytesMemo.make "BytesSTP.all_symbols_in_conditional_bytes"

    module BytesMemo = Memo.Make (BytesType)
    let wrap_all_symbols_in_bytes = BytesMemo.make "BytesSTP.all_symbols_in_bytes"

    module BytesListMemo = Memo.Make (ListPlus.MakeHashedList (BytesType))
    let wrap_all_symbols_in_list = BytesListMemo.make "BytesSTP.all_symbols_in_list"

    let rec all_symbols_in_bytearray bytearray =
        wrap_all_symbols_in_bytearray begin
            ImmutableArray.fold_left
                begin fun symbols byte -> match byte with
                     | Byte_Concrete _ -> symbols
                     | Byte_Symbolic s -> SymbolSet.add s symbols
                     | Byte_Bytes (bytes, _) -> SymbolSet.union symbols (all_symbols_in_bytes bytes)
                end
                SymbolSet.empty
        end bytearray

    and all_symbols_in_guard guard =
        wrap_all_symbols_in_guard begin function
            | Guard_True ->
                SymbolSet.empty
            | Guard_Not g ->
                all_symbols_in_guard g
            | Guard_And (g1, g2) ->
                SymbolSet.union (all_symbols_in_guard g1) (all_symbols_in_guard g2)
            | Guard_Symbolic s ->
                SymbolSet.singleton s
            | Guard_Bytes b ->
                all_symbols_in_bytes b
        end guard

    and all_symbols_in_conditional_bytes conditional_bytes =
        wrap_all_symbols_in_conditional_bytes begin function
            | IfThenElse (guard, c1, c2) ->
                SymbolSet.union (all_symbols_in_guard guard)
                    (SymbolSet.union (all_symbols_in_conditional_bytes c1) (all_symbols_in_conditional_bytes c2))
            | Unconditional b ->
                all_symbols_in_bytes b
        end conditional_bytes

    and all_symbols_in_bytes bytes =
        wrap_all_symbols_in_bytes begin function
            | Bytes_Constant _ ->
                SymbolSet.empty
            | Bytes_ByteArray bytearray ->
                all_symbols_in_bytearray bytearray
            | Bytes_Address (_, bytes) ->
                all_symbols_in_bytes bytes
            | Bytes_Op (_, bytes_typ_list) ->
                List.fold_left
                    (fun symbols (b, _) -> SymbolSet.union symbols (all_symbols_in_bytes b))
                    SymbolSet.empty
                    bytes_typ_list
            | Bytes_Read (bytes1, bytes2, _) ->
                SymbolSet.union (all_symbols_in_bytes bytes1) (all_symbols_in_bytes bytes2)
            | Bytes_Write (bytes1, bytes2, _, bytes3) ->
                SymbolSet.union
                    (all_symbols_in_bytes bytes3)
                    (SymbolSet.union (all_symbols_in_bytes bytes1) (all_symbols_in_bytes bytes2))
            | Bytes_FunPtr _ ->
                SymbolSet.empty
            | Bytes_Conditional c ->
                all_symbols_in_conditional_bytes c
        end bytes
end
(**/**)


(** Return a SymbolSet of all symbols in the given Bytes *)
let all_symbols_in_bytes bytes =
    Profiler.global#call "BytesSTP.all_symbols_in_bytes" begin fun () ->
        InternalAllSymbols.all_symbols_in_bytes bytes
    end


(** Return a SymbolSet of all symbols in the given list of Bytes *)
let all_symbols_in_list bytes_list =
    Profiler.global#call "BytesSTP.all_symbols_in_list" begin fun () ->
        let rec all_symbols_in_list = function
            | x::[] -> all_symbols_in_bytes x
            | x::xs as all -> InternalAllSymbols.wrap_all_symbols_in_list (fun _ -> SymbolSet.union (all_symbols_in_bytes x) (all_symbols_in_list xs)) all
            | [] -> SymbolSet.empty
        in
        all_symbols_in_list bytes_list
    end


(* This implements 'constraint independence': it picks out only those
 * clauses from the pc which can have an influence on the given
 * symbols. It grabs the clauses that mention any of the symbols, and
 * then iterates with the symbols present in these clauses, and so on,
 * until nothing else mentions any of the symbols we care about. (This
 * will happen eventually, because eventually pc will become empty.) *)
let getRelevantAssumptions pc query =
    Profiler.global#call "BytesSTP.getRelevantAssumptions" begin fun () ->
        let rec getRelevantAssumptions_aux acc symbols pc =
            (* See what clauses mention any of the symbols *)
            let subPC, relevant = List.partition (fun b -> SymbolSet.is_empty (SymbolSet.inter symbols (all_symbols_in_bytes b))) pc in
            match subPC, relevant with
                | _, [] ->
                     (* Nothing else is relevant *)
                    acc
                | subPC, relevant ->
                    (* Does using rev_append, rather than (@), mess with the order in a way
                     * that's bad for the cache? I think not, but I'm not sure. *)
                    getRelevantAssumptions_aux
                        (List.rev_append relevant acc)
                        (all_symbols_in_list relevant)
                        subPC
        in
        getRelevantAssumptions_aux [] (all_symbols_in_bytes query) pc
    end


(* TODO: Yit:

   I'd suggest splitting Bytes_Op into Bytes_UnOp and Bytes_BinOp so that the number of arguments can be statically
   typed, rather than asserting at runtime. All operators can be classified as unary or binary anyway, so there's no
   loss of expressiveness. It would make pattern-matching a lot less annoying too.

   It may not be such a bad idea to put all boolean operators into the guard type and not in the bytes type. It would
   modularize boolean operations and optimizations, putting it all in one place rather than all over as it is now.

   Maybe the type of the path condition should be changed from [ bytes ] to guard, which may present more opportunity
   to filter relevant clauses (by traversing the operands of Guard_And).

   Another thing to look into: is it possible to eliminate the byte type using an interval-tree, or some variant of it
   (http://en.wikipedia.org/wiki/Interval_tree)? My understanding is that, basically, the bytes type is an overlay on
   top of an STP bitvector that is built one byte at a time (e.g., WRITE (b0, 0, (WRITE b1, 1, ...))), leading to long
   STP expressions, and requiring the odd Byte_Bytes variant. I think an interval-tree can be used essentially to
   implement Bytes_Read/Bytes_Write directly without the byte type, which maps directly to STP's read/write to operate
   on a whole bitvector at once.
*)


(**/**) (* various helpers for STP conversion *)
(* memoization wrappers, which is possible because STP expressions are supposed to be immutable *)
module InternalToSTP = struct
    module WithSTP (T : Hashtbl.HashedType) = struct
        type t = OcamlSTP.context * T.t
        let equal (vc, x) (vc', x') = vc = vc' && T.equal x x'
        let hash (vc, x) = Hashtbl.hash (vc, T.hash x)
    end
    module ByteMemo = Memo.Make (WithSTP (ByteType))
    let wrap_byte_to_stp_bv = ByteMemo.make "BytesSTP.byte_to_stp_bv"

    module BytesMemo = Memo.Make (WithSTP (BytesType))
    let wrap_bytes_to_stp_array : (OcamlSTP.context * bytes -> ([`array], [`const|`expr|`var]) OcamlSTP.expr as 'a) -> 'a =
        BytesMemo.make "BytesSTP.bytes_to_stp_array"
    let wrap_bytes_to_stp_bv = BytesMemo.make "BytesSTP.bytes_to_stp_bv"
    let wrap_bytes_to_stp_bool = BytesMemo.make "BytesSTP.bytes_to_stp_bool"

    module GuardMemo = Memo.Make (WithSTP (GuardType))
    let wrap_guard_to_stp_bool = GuardMemo.make "BytesSTP.guard_to_stp_bool"

    module ArrayMetaMemo = Memo.Make (WithSTP (struct
        type t = BytesType.t * int
        let equal (xb, xw) (yb, yw) = xw = yw && BytesType.equal xb yb
        let hash (b, w) = Hashtbl.hash (BytesType.hash b, w)
    end))
    let array_counter = Counter.make () (* to make up a unique name for each array *)
    let array_meta =
        (* strictly speaking, this isn't memoization since it's side-effecting, but using Memo is rather convenient *)
        ArrayMetaMemo.memo "BytesSTP.array_meta" begin fun (vc, (bytes, length)) ->
            let name = "array_symbol_" ^ string_of_int (Counter.next array_counter) in
            let index_width =
                let rec index_width n m =
                    (* use bitmask test to avoid signed arithmetic issues *)
                    if m land length = length then
                        n
                    else
                        index_width (n + 1) (m lsl 1 lor 1)
                in
                index_width 1 1
            in
            (name, index_width)
        end
end

(* lookup or create an STP bool variable with a computed name *)
let symbol_to_stp_bool vc symbol =
    OcamlSTP.bool_var vc ("bool_symbol_" ^ string_of_int symbol.symbol_id)

(* lookup or create an STP bitvector variable with a computed name *)
let symbol_to_stp_bv vc symbol =
    OcamlSTP.bv_var vc ("byte_symbol_" ^ string_of_int symbol.symbol_id) 8

(* lookup or create an STP array variable with a name computed from a bytes and a sufficiently wide index width for the length of the array *)
let lookup_stp_array vc bytes length =
    let name, index_width = InternalToSTP.array_meta (vc, (bytes, length)) in
    OcamlSTP.array_var vc name index_width 8
(**/**)


(* The STP translation functions below are divided into three types:
    - foo_to_stp_bool translates various Bytes types to STP booleans, in particular, it converts only Bytes.bytes
        variants that result in booleans (i.e., logical or comparison operations);
    - foo_to_stp_bv translates various Bytes types to STP bitvectors, in particular, it converts only Bytes.bytes
        variants that results in bitvectors (i.e., arithmetic or bitwise operations);
    - foo_to_stp_array translates Bytes_ByteArray, Bytes_Read and Bytes_Write to equivalent array operations.

    Coercion from bool to/from bitvector is done only when needed. Similarly, big-endian to/from little-endian
    conversion is done only at Bytes_Read into an STP bitvector and Bytes_Write to an STP array.
 *)

(* TODO: OtterBytes should separate the following into different types:
    - unary operations from binary operations (rather than using a list in Bytes_Op);
    - boolean operation from arithmetic operations from comparison operations (rather than overloading Bytes_Op);
    - simple values from compound values (rather than overloading Bytes_Write/Bytes_Read).
*)

let rec byte_to_stp_bv vc byte =
    InternalToSTP.wrap_byte_to_stp_bv begin fun (vc, byte) -> match byte with
        | Byte_Concrete byte ->
            OcamlSTP.bv_of_int vc 8 (int_of_char byte)
        | Byte_Symbolic _ as b when b = byte__undef -> (* Here is where we catch attempts to use the undefined symbolic byte *)
            failwith "Conditional depends on undefined value"
        | Byte_Symbolic s ->
            symbol_to_stp_bv vc s
        | Byte_Bytes (array, index) ->
            let array = bytes_to_stp_array vc array in
            let index_width = OcamlSTP.array_index_width vc array in
            OcamlSTP.array_read vc array (OcamlSTP.bv_of_int vc index_width index)
    end (vc, byte)


and bytes_to_stp_array vc bytes =
    InternalToSTP.wrap_bytes_to_stp_array begin fun (vc, bytes) -> match bytes with
        | Bytes_ByteArray bytearray as bytes ->
            let array = lookup_stp_array vc bytes bytearray.ImmutableArray.length in
            let index_width = OcamlSTP.array_index_width vc array in
            (* TODO: ditch symbolic byte and make bytearray named, symbolic and sparse by default *)
            ImmutableArray.foldi begin fun array index byte ->
                OcamlSTP.array_write vc array (OcamlSTP.bv_of_int vc index_width index) (byte_to_stp_bv vc byte)
            end array bytearray

        | Bytes_Write (array, index, width, (Bytes_ByteArray _ | Bytes_Read _ | Bytes_Write _ as value)) ->
            (* values encode as Bytes_ByteArray or compound values do not need endian conversion *)
            let array = bytes_to_stp_array vc array in
            let index_width = OcamlSTP.array_index_width vc array in
            let index_start = OcamlSTP.bv_extract vc (bytes_to_stp_bv vc index) (index_width - 1) 0 in
            let src_array = bytes_to_stp_array vc value in
            let src_index_width = OcamlSTP.array_index_width vc src_array in
            let rec write array offset =
                if offset < width then
                    let bv8 = OcamlSTP.array_read vc src_array (OcamlSTP.bv_of_int vc src_index_width offset) in
                    let index = OcamlSTP.bv_add vc index_start (OcamlSTP.bv_of_int vc index_width offset) in
                    write (OcamlSTP.array_write vc array index bv8) (offset + 1)
                else
                    array
            in
            write array 0

        | Bytes_Write (array, index, width, (Bytes_Constant _ | Bytes_Address _ | Bytes_FunPtr _ | Bytes_Op _ | Bytes_Conditional _ as value)) ->
            (* simple values need to be converted to little-endian before writing to a compound value *)
            let array = bytes_to_stp_array vc array in
            let index_width = OcamlSTP.array_index_width vc array in
            let index_start = OcamlSTP.bv_extract vc (bytes_to_stp_bv vc index) (index_width - 1) 0 in
            let value = bytes_to_stp_bv vc value in
            let rec write array offset =
                if offset < width then
                    let bv8 = OcamlSTP.bv_extract vc value (offset * 8 + 7) (offset * 8) in (* write big-endian values as little-endian into arrays *)
                    let index = OcamlSTP.bv_add vc index_start (OcamlSTP.bv_of_int vc index_width (width - offset - 1)) in
                    write (OcamlSTP.array_write vc array index bv8) (offset + 1)
                else
                    array
            in
            write array 0

        | Bytes_Read (array, index, width) as bytes ->
            (* Otter's Bytes_Read is actually an array slice, not an array read *)
            let src_array = bytes_to_stp_array vc array in
            let src_index_width = OcamlSTP.array_index_width vc src_array in
            let src_index_start = OcamlSTP.bv_extract vc (bytes_to_stp_bv vc index) (src_index_width - 1) 0 in
            let array = lookup_stp_array vc bytes width in
            let index_width = OcamlSTP.array_index_width vc array in
            let rec write array offset =
                if offset < width then
                    let src_index = OcamlSTP.bv_add vc src_index_start (OcamlSTP.bv_of_int vc src_index_width offset) in
                    let bv8 = OcamlSTP.array_read vc src_array src_index in
                    let index = OcamlSTP.bv_of_int vc index_width offset in
                    write (OcamlSTP.array_write vc array index bv8) (offset + 1)
                else
                    array
            in
            write array 0

        | Bytes_Constant _ | Bytes_Address _ | Bytes_FunPtr _ | Bytes_Op _ | Bytes_Conditional _ as bytes ->
            (* TODO: ditch this case by enforcing that all Bytes_Write write to Bytes_ByteArray *)
            let value = bytes_to_stp_bv vc bytes in (* big-endian *)
            let width = OcamlSTP.bv_width vc value  / 8 in
            let array = lookup_stp_array vc bytes width in
            let index_width = OcamlSTP.array_index_width vc array in
            let rec write array offset =
                if offset < width then
                    let bv8 = OcamlSTP.bv_extract vc value (offset * 8 + 7) (offset * 8) in
                    let index = OcamlSTP.bv_of_int vc index_width offset in
                    write (OcamlSTP.array_write vc array index bv8) (offset + 1)
                else
                    array
            in
            write array 0
    end (vc, bytes)


and bytes_to_stp_bv vc bytes =
    InternalToSTP.wrap_bytes_to_stp_bv begin fun (vc, bytes) -> match bytes with
        | Bytes_Constant (Cil.CInt64 (value, ikind, _)) ->
            OcamlSTP.bv_of_int64 vc (Cil.bytesSizeOfInt ikind * 8) value
        | Bytes_Constant (Cil.CChr char) ->
            OcamlSTP.bv_of_int vc 8 (int_of_char char)
        | Bytes_Constant constant ->
            bytes_to_stp_bv vc (constant_to_bytes constant)

        | Bytes_Address (block, offset) ->
            let width = Cil.bitsSizeOf Cil.voidPtrType in
            let addr = OcamlSTP.bv_of_int vc width block.memory_block_addr in
            let offset = bytes_to_stp_bv vc offset in
            OcamlSTP.bv_add vc addr offset
        | Bytes_FunPtr varinfo ->
            (* TODO: make this symbolic s.t. different functions have different addresses *)
            let width = Cil.bitsSizeOf Cil.voidPtrType in
            OcamlSTP.bv_of_int vc width (Hashtbl.hash varinfo.Cil.vname)

        | Bytes_Op (OP_BNOT, [(bytes, _)]) ->
            OcamlSTP.bv_not vc (bytes_to_stp_bv vc bytes)

        | Bytes_Op (OP_UMINUS, [(bytes, _)]) ->
            OcamlSTP.bv_neg vc (bytes_to_stp_bv vc bytes)

        | Bytes_Op (OP_BAND | OP_BOR | OP_BXOR | OP_PLUS | OP_SUB | OP_MULT as binop, [(left, _); (right, _)]) ->
            let bv_binop = match binop with
                | OP_BAND -> OcamlSTP.bv_and
                | OP_BOR -> OcamlSTP.bv_or
                | OP_BXOR -> OcamlSTP.bv_xor
                | OP_PLUS -> OcamlSTP.bv_add
                | OP_SUB -> OcamlSTP.bv_sub
                | OP_MULT -> OcamlSTP.bv_mul
                | _ -> assert false
            in
            bv_binop vc (bytes_to_stp_bv vc left) (bytes_to_stp_bv vc right)

        | Bytes_Op (OP_LSL | OP_LSR as binop, [(left, typ); (right, _)]) ->
            (* C99 6.5.7.5 leaves right-shift undefined, but according to Wikipedia, most compilers use arithmetic shift for signed types *)
            let signed = match Cil.unrollType typ with
                | Cil.TInt (ikind, _) -> Cil.isSigned ikind
                | Cil.TFloat _ -> true (* TODO: actually do something reasonable *)
                | Cil.TPtr _ -> false
                | _ -> assert false (* there should be no other comparable type *)
            in
            let bv_binop = match binop with
                | OP_LSL -> OcamlSTP.bv_shift_left
                | OP_LSR -> if signed then OcamlSTP.bv_signed_shift_right else OcamlSTP.bv_shift_right
                | _ -> assert false
            in
            (* C only requires integer promotion on the left and right operand separately, but STP needs the operands to have the same width *)
            let value = bytes_to_stp_bv vc left in
            let value_width = OcamlSTP.bv_width vc value in
            let shift =
                let shift = bytes_to_stp_bv vc right in
                if value_width > OcamlSTP.bv_width vc shift then
                    OcamlSTP.bv_zero_extend vc shift value_width
                else
                    shift
            in
            bv_binop vc value shift

        | Bytes_Op (OP_DIV | OP_MOD as binop, [(left, typ); (right, _)]) ->
            (* usual arithmetic conversion is done by CIL, just need the sign *)
            (* TODO: move this out of OtterBytes by having specific signed comparison operators *)
            let signed = match Cil.unrollType typ with
                | Cil.TInt (ikind, _) -> Cil.isSigned ikind
                | Cil.TFloat _ -> true (* TODO: actually do something reasonable *)
                | Cil.TPtr _ -> false
                | _ -> assert false (* there should be no other comparable type *)
            in
            let bv_binop = match binop with
                | OP_DIV -> if signed then OcamlSTP.bv_signed_div else OcamlSTP.bv_div
                | OP_MOD -> if signed then OcamlSTP.bv_signed_rem else OcamlSTP.bv_rem
                | _ -> assert false
            in
            bv_binop vc (bytes_to_stp_bv vc left) (bytes_to_stp_bv vc right)

        | Bytes_Op ((OP_BNOT | OP_UMINUS | OP_BAND | OP_BOR | OP_BXOR | OP_LSL | OP_LSR | OP_PLUS | OP_SUB | OP_MULT | OP_DIV | OP_MOD), _) ->
            (* invalid number of operands *)
            (* TODO: separate boolean/arithmetic and unary/binary into different Bytes_X variant *)
            assert false

        | Bytes_Op (OP_SX, _) ->
            assert false (* TODO: remove OP_SX as it is completely unused *)

        | Bytes_Op _ as bytes ->
            (* CIL should enforce that comparisons are casted to int *)
            let width = Cil.bitsSizeOf Cil.intType in
            OcamlSTP.ite vc (bytes_to_stp_bool vc bytes) (OcamlSTP.bv_of_int vc width 1) (OcamlSTP.bv_of_int vc width 0)

        | Bytes_Conditional conditional ->
            conditional_to_stp vc bytes_to_stp_bv conditional

        | Bytes_ByteArray bytearray ->
            (* TODO: ditch this case by enforcing that values are never encoded as bytearrays *)
            let bv_opt = ImmutableArray.fold_left begin fun bv_opt byte ->
                let bv8 = byte_to_stp_bv vc byte in
                match bv_opt with
                    | Some bv -> Some (OcamlSTP.bv_concat vc bv8 bv) (* read big-endian values as little-endian from array *)
                    | None -> Some bv8
            end None bytearray in
            begin match bv_opt with
                | Some bv -> bv
                | None -> assert false (* zero-length array *)
            end

        | Bytes_Read (array, index, width) ->
            (* a simple value read from a compound value needs to be converted to big-endian *)
            let array = bytes_to_stp_array vc array in
            let index_width = OcamlSTP.array_index_width vc array in
            let index_start = OcamlSTP.bv_extract vc (bytes_to_stp_bv vc index) (index_width - 1) 0 in
            let rec read bv_opt offset =
                if offset < width then
                    let index = OcamlSTP.bv_add vc index_start (OcamlSTP.bv_of_int vc index_width offset) in
                    let bv8 = OcamlSTP.array_read vc array index in
                    let bv = match bv_opt with
                        | Some bv -> OcamlSTP.bv_concat vc bv8 bv (* read big-endian values as little-endian from array *)
                        | None -> bv8
                    in
                    read (Some bv) (offset + 1)
                else
                    bv_opt
            in
            begin match read None 0 with
                | Some bv -> bv
                | None -> assert false (* zero-length array *)
            end

        | Bytes_Write _ ->
            assert false
    end (vc, bytes)


and conditional_to_stp : 'a 'b . OcamlSTP.context -> (OcamlSTP.context -> 'a -> ('b, _) OcamlSTP.expr) -> 'a conditional -> ('b, _) OcamlSTP.expr =
    fun vc to_stp conditional ->
        (* TODO: specializing this to bool/bv could afford some optimization opportunities *)
        let rec conditional_to_stp = function
            | IfThenElse (guard, the, els) -> OcamlSTP.ite vc (guard_to_stp_bool vc guard) (conditional_to_stp the) (conditional_to_stp els)
            | Unconditional x -> to_stp vc x
        in
        conditional_to_stp conditional


and guard_to_stp_bool vc guard =
    InternalToSTP.wrap_guard_to_stp_bool begin fun (vc, guard) -> match guard with
        | Guard_Not guard -> OcamlSTP.bool_not vc (guard_to_stp_bool vc guard)
        | Guard_And (left, right) -> OcamlSTP.bool_and vc (guard_to_stp_bool vc left) (guard_to_stp_bool vc right)
        | Guard_Symbolic symbol -> symbol_to_stp_bool vc symbol
        | Guard_Bytes bytes -> bytes_to_stp_bool vc bytes
        | Guard_True -> assert false (* Guard_True can and should be optimized away *)
    end (vc, guard)


and bytes_to_stp_bool vc bytes =
    InternalToSTP.wrap_bytes_to_stp_bool begin fun (vc, bytes) -> match bytes with
        | Bytes_Constant (Cil.CInt64 (value, _, _)) ->
            if value <> 0L then OcamlSTP.bool_true vc else OcamlSTP.bool_false vc
        | Bytes_Constant (Cil.CChr value) ->
            if value <> '\000' then OcamlSTP.bool_true vc else OcamlSTP.bool_false vc
        | Bytes_Constant (Cil.CReal (value, _, _)) ->
            if value <> 0. then OcamlSTP.bool_true vc else OcamlSTP.bool_false vc
        | Bytes_Constant _ ->
            assert false (* are any other constants compared? *)

        | Bytes_Address _ | Bytes_FunPtr _ ->
            OcamlSTP.bool_true vc (* NULL is represented by a constant integer 0 *)

        | Bytes_Op (OP_LNOT, [(bytes, _)]) ->
            OcamlSTP.bool_not vc (bytes_to_stp_bool vc bytes)

        | Bytes_Op (OP_LAND | OP_LOR as binop, [(left, _); (right, _)]) ->
            let bool_binop = match binop with
                | OP_LAND -> OcamlSTP.bool_and
                | OP_LOR -> OcamlSTP.bool_or
                | _ -> assert false
            in
            bool_binop vc (bytes_to_stp_bool vc left) (bytes_to_stp_bool vc right)

        | Bytes_Op (OP_EQ | OP_NE | OP_LT | OP_GT | OP_LE | OP_GE as cmp, [(left, typ); (right, _)]) ->
            (* usual arithmetic conversion is done by CIL, just need the sign *)
            (* TODO: move this out of OtterBytes by having specific signed comparison operators *)
            let signed = match Cil.unrollType typ with
                | Cil.TInt (ikind, _) -> Cil.isSigned ikind
                | Cil.TFloat _ -> true (* TODO: actually do something reasonable *)
                | Cil.TPtr _ -> false
                | _ -> assert false (* there should be no other comparable type *)
            in
            let bv_cmp = match cmp with
                | OP_EQ -> OcamlSTP.bv_eq
                | OP_NE -> (fun vc left right -> OcamlSTP.bool_not vc (OcamlSTP.bv_eq vc left right))
                | OP_LT -> if signed then OcamlSTP.bv_signed_lt else OcamlSTP.bv_lt
                | OP_GT -> if signed then OcamlSTP.bv_signed_gt else OcamlSTP.bv_gt
                | OP_LE -> if signed then OcamlSTP.bv_signed_le else OcamlSTP.bv_le
                | OP_GE -> if signed then OcamlSTP.bv_signed_ge else OcamlSTP.bv_ge
                | _ -> assert false
            in
            bv_cmp vc (bytes_to_stp_bv vc left) (bytes_to_stp_bv vc right)

        | Bytes_Op ((OP_LNOT | OP_LAND | OP_LOR | OP_EQ | OP_NE | OP_LT | OP_GT | OP_LE | OP_GE), _) ->
            (* invalid number of operands *)
            (* TODO: separate boolean/arithmetic and unary/binary into different Bytes_X variant *)
            assert false

        | Bytes_Conditional conditional ->
            conditional_to_stp vc bytes_to_stp_bool conditional

        | Bytes_ByteArray bytearray when isConcrete_bytearray bytearray ->
            if ImmutableArray.for_all (function Byte_Concrete '\000' -> true | _ -> false) bytearray then
                OcamlSTP.bool_false vc
            else
                OcamlSTP.bool_true vc

        | bytes ->
            let value = bytes_to_stp_bv vc bytes in
            let width = OcamlSTP.bv_width vc value in
            let zero = OcamlSTP.bv_of_int64 vc width 0L in
            OcamlSTP.bool_not vc (OcamlSTP.bv_eq vc value zero) (* bytes != 0 *)
    end (vc, bytes)


(** return (True) False if bytes (not) evaluates to all zeros. Unknown otherwise. *)
let query_stp =
    let module PC = ListPlus.MakeHashedList (BytesType) in

    (* maintain the state of STP, and update only if necessary *)
    let vc = OcamlSTP.make_context () in
    let prev_pc = ref [] in
    let prev_pre = ref guard__true in
    let prev_guard = ref guard__true in
    let prev_guard_stp = ref (OcamlSTP.bool_true vc) in
    let setup pc pre guard =
        if not (guard__equal pre !prev_pre && PC.equal pc !prev_pc) then begin
            OcamlSTP.vc_pop vc;
            OcamlSTP.vc_push vc;
            Output.set_mode Output.MSG_STP;
            Output.printf "%% STP Program: %%@.";

            begin try
                Profiler.global#call "BytesSTP.query_stp/doassert" begin fun () ->
                    List.iter begin fun bytes ->
                        let pc_stp = Profiler.global#call "BytesSTP.query_stp/construct" (fun () -> bytes_to_stp_bool vc bytes) in (* 1 *)
                        Profiler.global#call "BytesSTP.query_stp/add assertion" (fun () -> OcamlSTP.vc_assert vc pc_stp) ; (* 2 *)
                        Output.set_mode Output.MSG_STP;
                        Output.printf "@[ASSERT(%t);@]@." (fun ff -> Format.pp_print_string ff (OcamlSTP.to_string vc pc_stp))
                    end pc
                end;

                if not (guard__equal pre guard__true) then begin
                    let pre_stp = Profiler.global#call "BytesSTP.query_stp/convert pre-condition" (fun () -> guard_to_stp_bool vc pre) in
                    Profiler.global#call "BytesSTP.query_stp/do_assert pre-condition" (fun () -> OcamlSTP.vc_assert vc pre_stp)
                end;
            with exn ->
                OcamlSTP.vc_pop vc;
                prev_pc := [];
                prev_pre := guard__true;
                raise exn
            end;

            (* set prev_* at the end, only if the above succeeds without an exception *)
            prev_pc := pc;
            prev_pre := pre;
        end;
        if not (guard__equal guard !prev_guard) then begin
            prev_guard_stp := Profiler.global#call "BytesSTP.query_stp/convert guard" (fun () -> guard_to_stp_bool vc guard);

            (* set prev_* at the end, only if the above succeeds without an exception *)
            prev_guard := guard;
        end;
        !prev_guard_stp
    in

    let module Memo = Memo.Make (struct
        type t = PC.t * guard * guard * bool (* pc * pre * guard * sign *)
        (* It might be better to have this be set-based equality rather than list-based. *)
        let equal ((pc1, pre1, guard1, sign1) : t) ((pc2, pre2, guard2, sign2) : t) =
            sign1 == sign2
            && GuardType.equal guard1 guard2
            && GuardType.equal pre1 pre2
            && PC.equal pc1 pc2

        let hash (pc, pre, guard, sign) = Hashtbl.hash (PC.hash pc, pre, guard, sign)
    end) in
    let query_stp = Memo.memo "BytesSTP.query_stp" begin fun (pc, pre, guard, sign) ->
        let guard_stp = setup pc pre guard in
        let guard_stp = if sign then guard_stp else OcamlSTP.bool_not vc guard_stp in

        Output.set_mode Output.MSG_STP;
        Output.printf "@[QUERY(%t);@]@." (fun ff -> Format.pp_print_string ff (OcamlSTP.to_string vc guard_stp));

        Profiler.global#call "BytesSTP.query_stp/query" begin fun () ->
            (* Note: the count is used as a proxy of running time in BackOtter *)
            DataStructures.NamedCounter.incr "stpc_query";

            (* synchronize OcamlSTP's random number generator to Otter's for reproducibility *)
            OcamlSTP.set_seed vc (Some (Random.bits ()));

            let stp_query () = OcamlSTP.vc_query vc guard_stp in
            let start = Sys.time () in
            let answer =
                try
                    OcamlSTP.vc_push vc;
                    let answer =
                        if (!arg_max_stp_time) < 0.0 then
                            stp_query ()
                        else
                            UnixPlus.fork_call ~time_limit:(!arg_max_stp_time) stp_query
                    in
                    OcamlSTP.vc_pop vc;
                    answer
                with exn ->
                    OcamlSTP.vc_pop vc;
                    match exn with
                        | Invalid_argument s ->
                            FormatPlus.failwith "Invalid_argument (%s)" s
                        | UnixPlus.ForkCallTimedOut ->
                            FormatPlus.failwith "ForkCallTimedOut caught in OcamlSTP.vc_query"
                        | UnixPlus.ForkCallException _ ->
                            FormatPlus.failwith "ForkCallException caught in OcamlSTP.vc_query"
                        | UnixPlus.ForkCallFailure _ ->
                            FormatPlus.failwith "ForkCallFailure caught in OcamlSTP.vc_query"
                        | UnixPlus.ForkCallExited i ->
                            FormatPlus.failwith "ForkCallExited (%d) caught in OcamlSTP.vc_query" i
                        | UnixPlus.ForkCallKilled i ->
                            FormatPlus.failwith "ForkCallKilled (%d) caught in OcamlSTP.vc_query" i
                        | UnixPlus.ForkCallStopped i ->
                            FormatPlus.failwith "ForkCallStopped (%d) caught in OcamlSTP.vc_query" i
                        | exn ->
                            raise exn
            in
            let elapsed = Sys.time () -. start in
            stp_queries := (pc, pre, guard, answer = OcamlSTP.Valid, elapsed)::(!stp_queries);
            answer
        end
    end in

    fun pc pre guard ->
        Profiler.global#call "BytesSTP.query_stp" begin fun () ->
            let pc = List.map (fun bytes -> Bytes.bytes__reduce bytes) pc in
            let pc = getRelevantAssumptions ((guard__to_bytes pre)::pc) (guard__to_bytes guard) in
            let pre = Bytes.guard__reduce pre in
            let guard = Bytes.guard__reduce guard in
            (* TODO: should consider Invalid or Undecided too? *)
            if query_stp (pc, pre, guard, true) = OcamlSTP.Valid then
                Ternary.True
            else if query_stp (pc, pre, guard, false) = OcamlSTP.Valid then
                Ternary.False
            else
                Ternary.Unknown
        end


let query_bytes pc bytes =
    query_stp pc guard__true (guard__bytes bytes)


(** Given a path condition and a list of symbols, return an
        association list of (symbol, char)s, where each char is the value
        STP gives to that symbol to make the path condition true. *)
let getValues pathCondition symbolList =
        let vc = OcamlSTP.make_context () in
        (* To get values for the symbolic values which make the path
             condition true, we need to query for its *negation* because STP
             gives counterexamples (not satisfying assignments). *)
        let negatedPcExpr =
            let pcExpr =
                List.fold_left
                    (fun expr bytes -> OcamlSTP.bool_and vc expr (bytes_to_stp_bool vc bytes))
                    (OcamlSTP.bool_true vc)
                    pathCondition
            in
            OcamlSTP.bool_not vc pcExpr
        in
        (* Extract the value of a symbol from STP's counterexample *)
        (* TODO (martin): getOneVal s fails when s is the guard in an ITE pointer *)
        let getOneVal s =
            let bv = symbol_to_stp_bv vc s in
            match OcamlSTP.vc_get_counterexample vc bv with
                | Some c -> (s, Char.chr (OcamlSTP.to_int vc c))
                | None -> assert false (* the query should always be invalid *)
        in
        let doit () =
            if OcamlSTP.vc_query vc negatedPcExpr = OcamlSTP.Valid then
                FormatPlus.failwith "The path condition is unsatisfiable!@\n@[  %a@]" (FormatPlus.pp_print_list (fun ff bytes -> BytesPrinter.bytes ff bytes) "@\nAND@\n  ") pathCondition;
            Some (List.map getOneVal (List.rev symbolList))
        in
        if (!arg_max_stp_time) < 0.0 then
            doit ()
        else
            try
                UnixPlus.fork_call ~time_limit:(!arg_max_stp_time) doit
            with UnixPlus.ForkCallTimedOut ->
                None


let getAllValues pathCondition =
    getValues pathCondition (SymbolSet.elements (all_symbols_in_list pathCondition))


(** {1 Command-line options} *)

let options = [
    "--printStpQueries",
        Arg.Set print_stp_queries,
        " Print STP queries (Path condition * pre * query * truth_value)";
    "--max-stp-time",
        Arg.Set_float arg_max_stp_time,
        "<t-secs> Maximum amount of time for a single query (default=unlimited)";
]

