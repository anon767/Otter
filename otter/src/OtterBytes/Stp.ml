open DataStructures
open OcamlUtilities
open Cil
open Bytes

module SymbolSet = Set.Make
    (struct
         type t = symbol
         let compare (x : t) y = Pervasives.compare x.symbol_id y.symbol_id
     end)

let stp_count = ref 0

let print_stp_queries = ref false
let stp_queries = ref []

let global_vc = Stpc.create_validity_checker ()

module BytesMagicMap = Map.Make (struct
    type t = bytes * int
    let compare (bs1, id1) (bs2, id2) =
        if true then compare (Obj.magic bs1 : int) (Obj.magic bs2) else
        if (bs1=bs2) then 0 else compare id1 id2
end)

let bytes_stpbv_id = Counter.make ()
let bytes_stpbv_map : (Stpvc.exp * int) BytesMagicMap.t ref = ref BytesMagicMap.empty
let bytes_stpbv_add bytes bv len =
    bytes_stpbv_map := BytesMagicMap.add (bytes, Counter.next bytes_stpbv_id) (bv, len) (!bytes_stpbv_map)
let bytes_stpbv_get bytes =
    BytesMagicMap.find (bytes, Counter.next bytes_stpbv_id) (!bytes_stpbv_map) (* raise Not_found *)

(** Return a SymbolSet of all symbols in the given Bytes *)
let rec allSymbolsInGuard = function
    | Guard_True ->
        SymbolSet.empty
    | Guard_Not g ->
        allSymbolsInGuard g
    | Guard_And (g1, g2) ->
        SymbolSet.union (allSymbolsInGuard g1) (allSymbolsInGuard g2)
    | Guard_Symbolic s ->
        SymbolSet.singleton s
    | Guard_Bytes b ->
        allSymbols b

and allSymbols = function
    | Bytes_Constant const -> SymbolSet.empty
    | Bytes_ByteArray bytearray ->
        ImmutableArray.fold_left
            begin fun symbSet byte -> match byte with
                 | Byte_Concrete _ -> symbSet
                 | Byte_Symbolic symb -> SymbolSet.add symb symbSet
                 | Byte_Bytes (bytes, _) -> SymbolSet.union symbSet (allSymbols bytes)
            end
            SymbolSet.empty
            bytearray
    | Bytes_Address (memBlock, bytes) ->
        SymbolSet.union (allSymbols bytes) (allSymbols memBlock.memory_block_addr)
    | Bytes_Op (_, bytes_typ_list) ->
        List.fold_left
            (fun symbSet (b, _) -> SymbolSet.union symbSet (allSymbols b))
            SymbolSet.empty
            bytes_typ_list
    | Bytes_Read (bytes1, bytes2, _) ->
        SymbolSet.union (allSymbols bytes1) (allSymbols bytes2)
    | Bytes_Write (bytes1, bytes2, _, bytes3) ->
        SymbolSet.union
            (allSymbols bytes3)
            (SymbolSet.union (allSymbols bytes1) (allSymbols bytes2))
    | Bytes_FunPtr (_, bytes) -> allSymbols bytes
    | Bytes_Unbounded (_, _, _) -> SymbolSet.empty
    | Bytes_Conditional c ->
        let rec allSymbolsInConditional = function
            | IfThenElse (guard, c1, c2) ->
                SymbolSet.union (allSymbolsInGuard guard)
                    (SymbolSet.union (allSymbolsInConditional c1) (allSymbolsInConditional c2))
            | Unconditional b ->
                allSymbols b
        in
        allSymbolsInConditional c

(** Return a SymbolSet of all symbols in the given list of Bytes *)
let allSymbolsInList byteslist =
    List.fold_left
        (fun symbSet b -> SymbolSet.union symbSet (allSymbols b))
        SymbolSet.empty
        byteslist

(* This implements 'constraint independence': it picks out only those
 * clauses from the pc which can have an influence on the given
 * symbols. It grabs the clauses that mention any of the symbols, and
 * then iterates with the symbols present in these clauses, and so on,
 * until nothing else mentions any of the symbols we care about. (This
 * will happen eventually, because eventually pc will become empty.) *)
let getRelevantAssumptions pc query =
    let rec getRelevantAssumptions_aux acc symbols pc =
        (* See what clauses mention any of the symbols *)
        let subPC, relevant = List.partition (fun b -> SymbolSet.is_empty (SymbolSet.inter symbols (allSymbols b))) pc in
        match subPC, relevant with
            | _, [] ->
                 (* Nothing else is relevant *)
                acc
            | subPC, relevant ->
                (* Does using rev_append, rather than (@), mess with the order in a way
                 * that's bad for the cache? I think not, but I'm not sure. *)
                getRelevantAssumptions_aux
                    (List.rev_append relevant acc)
                    (allSymbolsInList relevant)
                    subPC
    in
    getRelevantAssumptions_aux [] (allSymbols query) pc


(* TODO: Yit:

   There seems to be a few optimization opportunities: rather than always converting every C boolean expression
   into a bitvector of zero or one and then comparing b != 0, why not represent it as an STP boolean, and convert it
   only when necessary? That would cut down really verbose STP expressions, and turn something like
   ((b1 != 0 && (IF (b2 != 0 || b3 != 0) != 0 THEN 0 ELSE 1)) != 0) into ((b != 0) && !(b2 != 0 || b3 != 0)), saving
   a whole bunch of extraneous bitvectors. I think this can be done by special-casing the handling of LAND/LOR/LNOT
   with a new Bytes_Guard variant.

   I'd also suggest splitting Bytes_Op into Bytes_UnOp and Bytes_BinOp so that the number of arguments can be statically
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

let new_array vc bytes =
    let name =
        "bytes_" ^ (string_of_int (Hashtbl.hash bytes)) (* may clash *)
    in
    let arr_typ = Stpc.array_t vc (Stpc.bitvector_t vc 32) (Stpc.bitvector_t vc 8) in
    Stpc.e_var vc name arr_typ


let make_var symbol =
    "symbol_" ^ (string_of_int symbol.symbol_id)



let rec to_stp_array vc arr bytes =
    match bytes with
        | Bytes_Constant(constant) ->
            let bytes2 = constant_to_bytes constant in
            to_stp_array vc arr bytes2
        | Bytes_ByteArray(bytearray) ->
            let len = ImmutableArray.length bytearray in
            (*    Output.print_endline ("bytearray " ^ (string_of_int len)); *)
            let bv8 = begin match ImmutableArray.get bytearray (len - 1) with
                | Byte_Concrete(c) -> Stpc.e_bv_of_int vc 8 (Char.code c)
                | Byte_Symbolic(_) as b when b = byte__undef -> (* Here is where we catch attempts to use the undefined symbolic byte *)
                    failwith "Conditional depends on undefined value"
                | Byte_Symbolic(s) ->
                    Stpc.e_var vc (make_var s) (Stpc.bitvector_t vc 8)
                | Byte_Bytes(b, i) ->
                    let bv_condensed, _ = to_stp_bv vc b in
                    let right_i = i * 8 in
                    let left_i = right_i + 7 in
                    Stpc.e_bvextract vc bv_condensed left_i right_i
            end in
            if len = 1 then
                Stpc.e_write vc arr (Stpc.e_bv_of_int vc 32 0) bv8
            else
                let arr2 = to_stp_array vc arr (make_Bytes_ByteArray (ImmutableArray.sub bytearray 0 (len - 1))) in
                Stpc.e_write vc arr2 (Stpc.e_bv_of_int vc 32 (len - 1)) bv8

        | Bytes_Read (content, offset, len) ->
            let array_content = to_stp_array vc (new_array vc content) content in
            let bv_offset, _ = to_stp_bv vc offset in

            let rec read bv_offset len array =
                if len < 1 then
                    failwith "Bytes_Read len < 1"
                else if len = 1 then
                    Stpc.e_write vc array (Stpc.e_bv_of_int vc 32 0) (Stpc.e_read vc array_content bv_offset)
                else
                    let array2 = read bv_offset (len - 1) array in
                    let bv_offset2 = Stpc.e_bv_of_int vc 32 (len - 1) in
                    Stpc.e_write vc array2 bv_offset2 (Stpc.e_read vc array_content (Stpc.e_bvplus vc 32 bv_offset bv_offset2))
            in
            read bv_offset len (new_array vc bytes)

        | Bytes_Write (content, offset, len, newbytes) ->
            let bv_offset, _ = to_stp_bv vc offset in
            let bv_newbytes, _ = to_stp_bv vc newbytes in
            let rec write array_target bv_offset len  =
                let extracted_byte = Stpc.e_bvextract vc bv_newbytes (len * 8 + 7) (len * 8) in
                if len < 1 then
                    failwith "Bytes_Write len < 1"
                else if len = 1 then
                    Stpc.e_write vc array_target bv_offset extracted_byte
                else
                    let array_target2 = write array_target bv_offset (len - 1)  in
                    let bv_offset2 = Stpc.e_bv_of_int vc 32 (len - 1) in
                    Stpc.e_write vc array_target2 (Stpc.e_bvplus vc 32 bv_offset bv_offset2) extracted_byte
            in
            write (to_stp_array vc  (new_array vc content) content) bv_offset len

        | _ ->
            let (bv, len) = to_stp_bv vc bytes in
            let rec write array arr_len index = (* arr[index:(index - 7)] *)
                if index<0 then
                    array
                else
                    let array2 = Stpc.e_write vc array (Stpc.e_bv_of_int vc 32 arr_len) (Stpc.e_bvextract vc bv index (index - 7)) in
                    write array2 (arr_len + 1) (index - 8)
            in
            write (new_array vc bytes) 0 (len - 1)

and

to_stp_guard vc = function
    | Guard_Not g ->
        Stpc.e_not vc (to_stp_guard vc g)
    | Guard_And (g1, g2) ->
        Stpc.e_and vc (to_stp_guard vc g1) (to_stp_guard vc g2)
    | Guard_Symbolic s ->
        Stpc.e_var vc (make_var s) (Stpc.bool_t vc)
    | Guard_Bytes b ->
        let bv, len = to_stp_bv vc b in
        Stpc.e_ctrue vc len bv
    | Guard_True ->
        (* Guard_True can and should be optimized away *)
        failwith "to_stp_guard: cannot convert Guard_True"

and

to_stp_bv vc bytes =
  (*if false then to_stp_bv_impl vc bytes else*)
    try
        bytes_stpbv_get bytes
    with Not_found ->
        let (bv, len) = to_stp_bv_impl vc bytes in
        bytes_stpbv_add bytes bv len;
        (bv, len)

and

(** return (bv, len)
    bv: a 1-d bitvector that describe the bytes
        len: length (bits) of bv

        note: e_bv_of_int (with) (content)  -> bv
 *)
to_stp_bv_impl vc bytes =
    match bytes with
        | Bytes_Constant (constant) ->
            let bytes2 = constant_to_bytes constant in
            to_stp_bv vc bytes2

        | Bytes_ByteArray (bytearray) ->
            let len = ImmutableArray.length bytearray in
            let bv8 = begin match ImmutableArray.get bytearray 0 with
                | Byte_Concrete(c) -> Stpc.e_bv_of_int vc 8 (Char.code c)
                | Byte_Symbolic(_) as b when b = byte__undef -> (* Here is where we catch attempts to use the undefined symbolic byte *)
                    failwith "Conditional depends on undefined value"
                | Byte_Symbolic(s) ->
                    Stpc.e_var vc (make_var s) (Stpc.bitvector_t vc 8)
                | Byte_Bytes(b, i) ->
                    let bv_condensed, _ = to_stp_bv vc b in
                    let right_i = i * 8 in
                    let left_i = right_i + 7 in
                    Stpc.e_bvextract vc bv_condensed left_i right_i
            end in
            if len = 1 then
                (bv8, 8)
            else
                let bv, l = to_stp_bv vc (make_Bytes_ByteArray (ImmutableArray.sub bytearray 1 (len - 1))) in
                (Stpc.e_bvconcat vc bv8 bv, l + 8)

        | Bytes_Address (block, offset) ->
            let bv_offset, _ = to_stp_bv vc offset in
            let bv_blockaddr, len = to_stp_bv vc block.memory_block_addr in
            (Stpc.e_bvplus vc len bv_blockaddr bv_offset, len)

        | Bytes_Conditional c ->
            let rec to_stp_bv_conditional = function
                | Unconditional b ->
                    to_stp_bv vc b
                | IfThenElse (guard, c1, c2) ->
                    let cond = to_stp_guard vc guard in
                    let bv1, len1 = to_stp_bv_conditional c1 in
                    let bv2, len2 = to_stp_bv_conditional c2 in
                    assert (len1 = len2);
                    (* if cond then bv1 else bv2 *)
                    (Stpc.e_ite vc cond bv1 bv2, len1)
            in
            to_stp_bv_conditional c

        | Bytes_Op(op, [(bytes1, typ1); (bytes2, typ2)]) -> (* BINOP *)
            (* typ info maybe added to the stp formula later *)
            let (bv1, len1) = to_stp_bv vc bytes1 in
            let (bv2, len2) = to_stp_bv vc bytes2 in
            let len_of_1_0 = 32 in
            let bv_1 = (Stpc.e_bv_of_int vc len_of_1_0 1) in
            let bv_0 = (Stpc.e_bv_of_int vc len_of_1_0 0) in
            let isSigned = match Cil.unrollType typ1 with
                | TInt (ikind, _) -> Cil.isSigned ikind
                | _ -> false
            in
            let op_func bv1 len1 bv2 len2 = match op with
                | OP_PLUS ->    (Stpc.e_bvplus vc len1 bv1 bv2, len1)
                | OP_SUB -> (Stpc.e_bvminus vc len1 bv1 bv2, len1)
                | OP_MULT ->    (Stpc.e_bvmult vc len1 bv1 bv2, len1)
                | OP_DIV -> (Stpc.e_bvdiv vc len1 bv1 bv2, len1) (* TODO: add sign support *)
                | OP_MOD -> (Stpc.e_bvmod vc len1 bv1 bv2, len1)
                (* for left shift, many need to resize the len *)
(*              | OP_LSL -> (Stpc.e_bvextract vc (Stpc.e_bvshiftleft vc len1 bv1 len2 bv2) (len1 - 1) 0, len1)                *)
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
                    let bv =
                        (Stpc.e_ite vc
                            (Stpc.e_or vc
                                (Stpc.e_eq vc bv1 (Stpc.e_bv_of_int vc len1 0))
                                (Stpc.e_eq vc bv2 (Stpc.e_bv_of_int vc len2 0))
                            )
                            bv_0 bv_1
                        )
                    in
                    (bv, len_of_1_0)
                | OP_LOR ->
                    let bv =
                        (Stpc.e_ite vc
                            (Stpc.e_and vc
                                (Stpc.e_eq vc bv1 (Stpc.e_bv_of_int vc len1 0))
                                (Stpc.e_eq vc bv2 (Stpc.e_bv_of_int vc len2 0))
                            )
                            bv_0 bv_1
                        )
                    in
                    (bv, len_of_1_0)
                | OP_SX -> (* here bv2 must be constant *)
                    failwith "not implemented"
                | _ ->
                    FormatPlus.failwith "%a is not a binary operator" BytesPrinter.operator op
            in
            op_func bv1 len1 bv2 len2

        | Bytes_Op(op, [(bytes1, typ1)]) -> (* UNOP *)
            let (bv1, len1) = to_stp_bv vc bytes1 in
            let len_of_1_0 = 32 in
            let bv_1 = (Stpc.e_bv_of_int vc len_of_1_0 1) in
            let bv_0 = (Stpc.e_bv_of_int vc len_of_1_0 0) in
            let op_func bv1 len1 = match op with
                | OP_UMINUS -> (Stpc.e_bvneg vc bv1 len1, len1)
                | OP_BNOT ->(Stpc.e_bvnot vc bv1, len1)
                | OP_LNOT ->
                    let bv =
                        (Stpc.e_ite vc
                            (Stpc.e_not vc
                                (Stpc.e_eq vc bv1 (Stpc.e_bv_of_int vc len1 0))
                            )
                            bv_0 bv_1
                        )
                    in
                    (bv, len_of_1_0)
                | _ ->
                    FormatPlus.failwith "%a is not a unary operator" BytesPrinter.operator op
            in
            op_func bv1 len1

        | Bytes_Op(OP_LAND, bytesTypList) -> (* Let AND be variadic *)
            let bvLenList =
                List.map (fun (bytes, _) -> to_stp_bv vc bytes) bytesTypList
            in
            let len_of_1_0 = 32 in
            let bv_1 = (Stpc.e_bv_of_int vc len_of_1_0 1) in
            let bv_0 = (Stpc.e_bv_of_int vc len_of_1_0 0) in
            (* If any is false, then 0; otherwise 1. *)
            let bv =
                (Stpc.e_ite vc
                    (List.fold_left
                        (fun expr (bv, len) -> Stpc.e_or vc expr (Stpc.e_cfalse vc len bv))
                        (Stpc.e_false vc)
                        bvLenList
                    )
                    bv_0 bv_1
                )
            in
            (bv, len_of_1_0)

        | Bytes_Op(op, _) ->
            FormatPlus.failwith "Invalid number of operands for %a" BytesPrinter.operator op

        | Bytes_Read (content, offset, len) ->
            let arr = new_array vc content in
            let array_content = to_stp_array vc arr content in
            let bv_offset, _ = to_stp_bv vc offset in
            let rec read bv_offset len =
                if len < 1 then
                    failwith "to_stp_bv: Bytes_Read len < 1"
                else if len = 1 then
                    (Stpc.e_read vc array_content bv_offset, 8)
                else
                    let (bv_head, len_head) = (Stpc.e_read vc array_content bv_offset, 8)  in
                    let (bv_tail, len_tail) = read (Stpc.e_bvplus vc 32 bv_offset (Stpc.e_bv_of_int vc 32 1)) (len - 1) in
                    (Stpc.e_bvconcat vc bv_head bv_tail, len_head + len_tail)
            in
            read bv_offset len

        | Bytes_FunPtr (fundec, f_addr) ->
            (*
            Output.set_mode Output.MSG_DEBUG;
            Output.print_endline ("STP encodes addr of "^ (To_string.fundec fundec) ^ " to :" ^ (To_string.bytes f_addr));
            *)
            to_stp_bv vc f_addr

        | Bytes_Write _ ->
            FormatPlus.failwith "Impossible: converting a Bytes_Write to a bitvector. Is this a Write not under a Read?\n%a" BytesPrinter.bytes bytes

        | Bytes_Unbounded (name, id, size) ->
            failwith "Oh no!"

(* TODO (martin) improve Stp Cache *)
(* Map a pc and a query *)
module StpCache = Map.Make (struct
    type t = bytes list * guard * guard * bool (* pc * pre * guard * truth_value *)
    (* It might be better to have this be set-based equality rather than list-based. *)
    let compare ((pc1, pre1, guard1, truth1) : t) ((pc2, pre2, guard2, truth2) : t) = (* Type annotation to remove polymorphism *)
        let result = Pervasives.compare truth1 truth2 in
        if result <> 0 then result else
        let result = Pervasives.compare guard1 guard2 in
        if result <> 0 then result else
        let result = Pervasives.compare pre1 pre2 in
        if result <> 0 then result else
        Pervasives.compare pc1 pc2
 end)

let cacheHits = ref 0
let stpCacheRef = ref StpCache.empty

(* This is extracted from consult_stp, modified to work with query_stp (guard).
 * Will be replaced. *)
let stpcache_find pc pre guard truth =
    try
        let ans = StpCache.find (pc, pre, guard, truth) !stpCacheRef in
        incr cacheHits;
        Some (ans)
    with Not_found ->
        None

let stpcache_add answer pc pre guard truth =
    stpCacheRef := StpCache.add (pc, pre, guard, truth) answer !stpCacheRef


(** return (True) False if bytes (not) evaluates to all zeros. Unknown otherwise.  *)
let query_stp pc pre guard =
    let query_stp pc pre guard =
        let pc = getRelevantAssumptions pc (guard__to_bytes guard) in
        (* TODO (yit):
         * The way doassert returns global_vc feels dangerous to me,
         * since it hides the fact that there's a single, shared,
         * stateful vc. Is it that costly to create a new vc?
         * The "pop; push" pattern is also very strange and unintuitive.
         *)
        let doassert pc =
            let vc = global_vc in
            Stpc.e_pop vc;
            Stpc.e_push vc;

            Output.set_mode Output.MSG_STP;
            Output.printf "%% STP Program: %%@\n";

            let rec do_assert pc = match pc with
                | [] -> ()
                | head::tail ->
                    let (bv, len) = Stats.time "STP construct" (fun () -> to_stp_bv vc head) () in (* 1 *)
                    Stats.time "STP doassert" (fun () -> Stpc.assert_ctrue vc len bv) () ; (* 2 *)
                    Output.set_mode Output.MSG_STP;
                    Output.printf "ASSERT(NOT(%t = 0hex00000000)); @\n" (fun ff -> Format.pp_print_string ff (Stpc.to_string bv));
                    do_assert tail
            in
            (*Stats.time "STP assert" do_assert relevantAssumptions; *)
            Stats.time "STP assert" do_assert pc;
            vc
        in
        (* TODO: avoid creating vc if cache hit for both T and F *)
        let vc = doassert pc in
        if pre != Guard_True then begin
            let pre_exp = Stats.time "convert pre-condition" (to_stp_guard vc) pre in
            Stats.time "STP.do_assert pre-condition" (Stpc.do_assert vc) pre_exp
        end;

        let guard_exp = Stats.time "convert guard" (to_stp_guard vc) guard in
        Output.set_mode Output.MSG_STP;
        Output.printf "QUERY(%t); @\n" (fun ff -> Format.pp_print_string ff (Stpc.to_string guard_exp));

        let query exp truth_value =
            match stpcache_find pc pre guard truth_value with
                | Some answer ->
                    answer
                | None ->
                    Stpc.e_push vc;
                    incr stp_count;
                    let startTime = Unix.gettimeofday () in
                    let answer = Stats.time "STP query" begin
                        (* Note: the count is used as a proxy of running time in BackOtter *)
                        DataStructures.NamedCounter.incr "stpc_query";
                        Stpc.query vc
                    end exp in
                    stpcache_add answer pc pre guard truth_value;
                    let endTime = Unix.gettimeofday () in
                    stp_queries := (pc, pre, guard, truth_value, endTime -. startTime)::(!stp_queries);
                    Stpc.e_pop vc;
                    answer
        in
        if query guard_exp true then
            Ternary.True
        else if query (Stpc.e_not vc guard_exp) false then
            Ternary.False
        else
            Ternary.Unknown
    in
    Stats.time "STP" (query_stp pc pre) guard


let query_bytes pc bytes =
    query_stp pc Guard_True (guard__bytes bytes)


(** Given a path condition and a list of symbols, return an
        association list of (symbol, char)s, where each char is the value
        STP gives to that symbol to make the path condition true. *)
let getValues pathCondition symbolList =
    let vc = global_vc in
    Stpc.e_pop vc;
    Stpc.e_push vc;
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
    if Stpc.query vc negatedPcExpr then
        FormatPlus.failwith "The path condition is unsatisfiable!@\n@[  %a@]" (FormatPlus.pp_print_list BytesPrinter.bytes "@\nAND@\n  ") pathCondition;
    (* Extract the value of a symbol from STP's counterexample *)
    (* TODO (martin): getOneVal s fails when s is the guard in an ITE pointer *)
    let getOneVal s =
        let bv = Stpc.e_var vc (make_var s) (Stpc.bitvector_t vc 8) in
        let counter_example = (Stpc.get_counterexample vc bv) in
        (s, Char.chr (Stpc.int_of_e counter_example))
    in
    List.map getOneVal (List.rev symbolList)


let getAllValues pathCondition =
    getValues pathCondition (SymbolSet.elements (allSymbolsInList pathCondition))


(** {1 Command-line options} *)

let options = [
    "--printStpQueries",
        Arg.Set print_stp_queries,
        " Print STP queries (Path condition * pre * query * truth_value)";
]

