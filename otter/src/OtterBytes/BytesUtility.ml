open DataStructures
open OcamlUtilities
open Cil
open Bytes


let rec bytes__read ?test ?pre bytes off len =
    let worst_case = make_Bytes_Read (bytes, off, len) in
    let ret_bytes =
        match bytes, off with
            | Bytes_ByteArray array, Bytes_Constant (CInt64 (i64, k, _)) ->
                let i = Int64.to_int i64 in
                make_Bytes_ByteArray (ImmutableArray.sub array i len)

            | Bytes_Constant constant, Bytes_Constant (CInt64 (i64, k, _)) ->
                let converted_bytes = constant_to_bytes constant in
                begin match converted_bytes with
                    | Bytes_ByteArray array ->
                        let i = Int64.to_int i64 in
                        make_Bytes_ByteArray (ImmutableArray.sub array i len)
                    | _ ->
                        worst_case
                end

            | Bytes_Write (bytes2, off2, len2, newbytes), _ ->
                if off2 = off && len2 = len then
                    newbytes (* being a bit tricky... *)
                else (* CAUTION: assume [off2, len2] and [off, len] don't overlap.  *)
                    worst_case

            | Bytes_Conditional c, _ ->
                let c = conditional__map ?test ~eq:bytes__equal ?pre (fun e -> conditional__bytes (bytes__read e off len)) c in
                make_Bytes_Conditional c

            | _, Bytes_Conditional c ->
                failwith "bytes__read: if-then-else offset doesn't happen"

            | _, _ when (bytes__length bytes = len) && (isConcrete_bytes off) && (bytes_to_int_auto off = 0) ->
                bytes

            | _ ->
                worst_case
    in
    (* try to inflate any Bytes_ByteArray of Byte_Bytes *)
    match ret_bytes with
        | Bytes_ByteArray bytearray ->
            begin match ImmutableArray.get bytearray 0 with
                | Byte_Bytes (condensed_bytes, 0) ->
                    (* Make sure length agrees, and that bytes 1 through len-1 match up. *)
                    let rec check_byte n =
                        if n >= len then
                            true
                        else
                            match ImmutableArray.get bytearray n with
                                | Byte_Bytes (b, i) when i = n && b == condensed_bytes -> check_byte (succ n)
                                | _ -> false
                    in
                    if bytes__length condensed_bytes = len && check_byte 1 then
                        condensed_bytes
                    else
                        ret_bytes

                | _ ->
                    ret_bytes
            end

        | _ ->
            ret_bytes


let bytes__write ?test ?pre bytes off len newbytes =
    let rec do_write bytes off len newbytes =
        match bytes, off, newbytes with
            (* Optimize for memset  *)

            | Bytes_ByteArray oldarray, Bytes_Constant (CInt64 (i64, k, _)), Bytes_ByteArray newarray ->
                (* from j = 0 to len-1 do oldarray[i+j] = newarray[j] *)
                (* EXPERIMENT: if contents from oldarray is unwritable, then pass *)
                let i = Int64.to_int i64 in
                let rec impl j array =
                    if j < 0 then
                        array
                    else
                        let array2 = impl (j - 1) array in
                        (*
                        let oldbyte = ImmutableArray.get array2 (i + j) in
                        match oldbyte with
                            | Byte_Symbolic s when s.symbol_writable=false -> warning (); array2
                            | _ ->
                        *)
                        ImmutableArray.set array2 (i + j) (ImmutableArray.get newarray j)
                in
                make_Bytes_ByteArray (impl (len - 1) oldarray)

            | Bytes_ByteArray oldarray, Bytes_Constant (CInt64 (i64, k, _)), Bytes_Constant const ->
                do_write bytes off len (constant_to_bytes const)

            | Bytes_ByteArray oldarray, Bytes_Constant (CInt64 (i64, k, _)), _ ->
                let rec impl arr i =
                    if i >= len then
                        arr
                    else
                        impl (ImmutableArray.set arr i (make_Byte_Bytes (newbytes, i))) (i + 1)
                in
                do_write bytes off len (make_Bytes_ByteArray (impl (ImmutableArray.make len byte__zero) 0))

            | Bytes_ByteArray oldarray, _, _ when isConcrete_bytes off ->
                let n_off = bytes_to_constant off Cil.intType in
                do_write bytes (make_Bytes_Constant n_off) len newbytes

            (* Without this next case, writing to a constant would introduce
                 a Bytes_Write. Aside from not wanting a Bytes_Write if we can
                 avoid it (for example, writing a concrete byte to the first
                 byte of a concrete int), this could cause problems. The
                 potential problem has to do with writing past the end of an
                 array that is represented as a Bytes_Constant (which could
                 exist if, for example, you have a 4-byte ByteArray and write
                 a Bytes_Constant int to it). *)
            | Bytes_Constant c, _, _ ->
                do_write (constant_to_bytes c) off len newbytes

            | Bytes_Conditional c, _, _ ->
                let c = conditional__map ?test ~eq:bytes__equal ?pre (fun e -> conditional__bytes (do_write e off len newbytes)) c in
                make_Bytes_Conditional c

            | _, Bytes_Conditional c, _ ->
                failwith "bytes__write: if-then-else offset doesn't happen"

            | _ ->
                make_Bytes_Write (bytes, off, len, newbytes)
    in
    if (bytes__length bytes) = len && (isConcrete_bytes off) && (bytes_to_int_auto off = 0) then
        newbytes
    else
        do_write bytes off len newbytes

let is_Bytes_Read = function Bytes_Read _ -> true | _ -> false

(** Create a bytes conditional representing all values that could result from reading from the given bytes at the given index.
    This function assumes that symIndex is within the bounds of the memory region being read; any possible out-of-bounds accesses will be silently discarded.
    @param bytes is the bytes being read from : [bytes]
    @param symIndex is the index into bytes : [bytes]
    @param len is the width of the read : [int]
    @return a conditional representing ITE(symIndex=0, bytes[0..len-1], ITE(symIndex=1, bytes[1..len], ... ITE(symIndex=size-len-1, bytes[size-len-1..size-2], bytes[size-len..size-1])...))
*)
let expand_read_to_conditional2 bytes symIndex len =
    let rec expand index =
        let read_at_index = bytes__read bytes (int_to_bytes index) len in
        if is_Bytes_Read read_at_index then failwith "bytes__read with constant offset unexpectedly evaluated to a Bytes_Read";
        let read_at_index = conditional__bytes read_at_index in
        assert (index >= 0);
        if index = 0 then read_at_index else
        IfThenElse (
            Guard_Bytes (make_Bytes_Op (
                OP_EQ,
                [(symIndex, !Cil.upointType); ((int_to_bytes index), !Cil.upointType)]
            )),
            read_at_index,
            expand (index - 1)
        )
    in
    let block_size = match bytes with
        | Bytes_ByteArray a -> ImmutableArray.length a
        | Bytes_Conditional c -> failwith "Unexpected Bytes_Conditional"
        | _ -> FormatPlus.failwith "Not a valid array:@ @[%a@]" BytesPrinter.bytes bytes
    in
    if block_size < len then failwith "Error in expand_read_to_conditional2: the read is wider than the memory block";
    expand (block_size - len)

let rec expand_read_to_conditional (bytes : bytes) (symIndex : bytes) (len : int) =
    let bytes = match bytes with
        | Bytes_Read (a, x, l) -> make_Bytes_Conditional (expand_read_to_conditional a x l)
        | _ -> bytes
    in
    match bytes with
        | Bytes_Write (a, x, l, v) ->
            IfThenElse (
                Guard_Bytes (make_Bytes_Op (
                    OP_EQ,
                    [(symIndex, Cil.intType); (x, Cil.intType)]
                )),
                (Unconditional v),
                (expand_read_to_conditional a symIndex len)
            )

        | Bytes_Conditional c ->
            let map_func leaf =
                match leaf with
                    | Bytes_Read (a, x, l) ->
                        expand_read_to_conditional a x l
                    | Bytes_Conditional c ->
                        expand_read_to_conditional leaf symIndex len
                        (* ^^ someone hid another conditional tree in here*)

                    (*| Bytes_Write (a, x, l, v) ->
                        IfThenElse (
                            Guard_Bytes (make_Bytes_Op (
                                OP_EQ,
                                [(symIndex, Cil.intType); (x, Cil.intType)]
                            )),
                            (Unconditional v),
                            (expand_read_to_conditional a symIndex len)
                        )*)
                    (* TODO: 'bytes' on the next line looks very suspicious. Should it be 'leaf' instead? *)

                    | _ -> (*expand_read_to_conditional2 bytes symIndex len*)
                      failwith "Error in expand_read_to_conditional"
            in
            conditional__map map_func c

        | _ ->
            expand_read_to_conditional2 bytes symIndex len
