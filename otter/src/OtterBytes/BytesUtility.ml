open DataStructures
open OcamlUtilities
open Cil
open Bytes


let rec bytes__read ?test ?pre acc bytes off len =
    let worst_case = make_Bytes_Read (bytes, off, len) in
    let acc, ret_bytes =
        match bytes, off with
            | Bytes_ByteArray array, Bytes_Constant (CInt64 (i64, k, _)) ->
                let i = Int64.to_int i64 in
                (acc, make_Bytes_ByteArray (ImmutableArray.sub array i len))

            | Bytes_Constant constant, Bytes_Constant (CInt64 (i64, k, _)) ->
                let converted_bytes = constant_to_bytes constant in
                begin match converted_bytes with
                    | Bytes_ByteArray array ->
                        let i = Int64.to_int i64 in
                        (acc, make_Bytes_ByteArray (ImmutableArray.sub array i len))
                    | _ ->
                        (acc, worst_case)
                end

            | Bytes_Write (bytes2, off2, len2, newbytes), _ ->
                if bytes__equal off2 off && len2 = len then
                    (acc, newbytes)
                else (* CAUTION: assume [off2, len2] and [off, len] don't overlap.  *)
                    (acc, worst_case)

            | Bytes_Conditional c, _ ->
                let acc, c = conditional__fold_map ?test ~eq:bytes__equal ?pre begin fun acc pre e ->
                    let acc, e = bytes__read ?test ~pre acc e off len in
                    (acc, conditional__bytes e)
                end acc c in
                (acc, make_Bytes_Conditional c)

            | _, Bytes_Conditional c ->
                failwith "bytes__read: if-then-else offset doesn't happen"

            | _, _ when (bytes__length bytes = len) && (isConcrete_bytes off) && (bytes_to_int_auto off = 0) ->
                (acc, bytes)

            | _ ->
                (acc, worst_case)
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
                        (acc, condensed_bytes)
                    else
                        (acc, ret_bytes)

                | _ ->
                    (acc, ret_bytes)
            end

        | _ ->
            (acc, ret_bytes)


let bytes__write ?test ?pre acc bytes off len newbytes =
    if len = 0 then
        (acc, bytes)
    else if (bytes__length bytes) = len && (isConcrete_bytes off) && (bytes_to_int_auto off = 0) then
        (acc, newbytes)
    else
        let rec do_write ?pre acc bytes off len newbytes =
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
                    (acc, make_Bytes_ByteArray (impl (len - 1) oldarray))

                | Bytes_ByteArray oldarray, Bytes_Constant (CInt64 (i64, k, _)), Bytes_Constant const ->
                    do_write ?pre acc bytes off len (constant_to_bytes const)

                | Bytes_ByteArray oldarray, Bytes_Constant (CInt64 (i64, k, _)), _ ->
                    let rec impl arr i =
                        if i >= len then
                            arr
                        else
                            impl (ImmutableArray.set arr i (make_Byte_Bytes (newbytes, i))) (i + 1)
                    in
                    do_write ?pre acc bytes off len (make_Bytes_ByteArray (impl (ImmutableArray.make len byte__zero) 0))

                | Bytes_ByteArray oldarray, _, _ when isConcrete_bytes off ->
                    let n_off = bytes_to_constant off Cil.intType in
                    do_write ?pre acc bytes (make_Bytes_Constant n_off) len newbytes

                (* Without this next case, writing to a constant would introduce
                     a Bytes_Write. Aside from not wanting a Bytes_Write if we can
                     avoid it (for example, writing a concrete byte to the first
                     byte of a concrete int), this could cause problems. The
                     potential problem has to do with writing past the end of an
                     array that is represented as a Bytes_Constant (which could
                     exist if, for example, you have a 4-byte ByteArray and write
                     a Bytes_Constant int to it). *)
                | Bytes_Constant c, _, _ ->
                    do_write ?pre acc (constant_to_bytes c) off len newbytes

                | Bytes_Conditional c, _, _ ->
                    let acc, c = conditional__fold_map ?test ~eq:bytes__equal ?pre begin fun acc pre e ->
                        let acc, e = do_write ~pre acc e off len newbytes in
                        (acc, conditional__bytes e)
                    end acc c in
                    (acc, make_Bytes_Conditional c)

                | _, Bytes_Conditional c, _ ->
                    failwith "bytes__write: if-then-else offset doesn't happen"

                | _ ->
                    (acc, make_Bytes_Write (bytes, off, len, newbytes))
        in
        do_write ?pre acc bytes off len newbytes

let is_Bytes_Read = function Bytes_Read _ -> true | _ -> false

(** Create a bytes conditional representing all values that could result from reading from the given bytes (which must be a bytearray) at the given index.
    This function assumes that symIndex is within the bounds of the memory region being read; any possible out-of-bounds accesses will be silently discarded.
    @param bytes is the bytes being read from : [bytes]
    @param symIndex is the index into bytes : [bytes]
    @param len is the width of the read : [int]
    @param step_size is the increment by which symIndex should be increased : [int]
    @return a conditional representing a chain of 'if symIndex = n then bytes[n*step_size..n*step_size+len-1] else ...' cases, 
            where n ranges from 0 to ((the size of the bytearray) - len)/step_size.
            When indices i where a < i <= b yield the same bytes, a conditional bytes of the form IfThenElse(a<i<=b,bytes_b,...) is created.
*)
let expand_read_to_conditional2 bytes symIndex len step_size =
    let rec expand index last_opt =
        let (), read_at_index = bytes__read () bytes (int_to_bytes (index * step_size)) len in
        match last_opt with
        | Some(last_bytes, last_index) ->
            if index>=0 && (bytes__equal read_at_index last_bytes) then
                (* Merge with previous read_at_index *)
                expand (index - 1) last_opt
            else if index < 0 then
                (* No need to setup the guard, coz it's the last case in the conditional tree *)
                conditional__bytes last_bytes
            else
                (* Output read_at_index for index < i <= last_index *)
                IfThenElse (
                    guard__bytes (make_Bytes_Op (OP_LAND, [
                        (make_Bytes_Op (OP_GT, [(symIndex, !Cil.upointType); ((int_to_bytes index), !Cil.upointType)]), !Cil.upointType);
                        (make_Bytes_Op (OP_LE, [(symIndex, !Cil.upointType); ((int_to_bytes last_index), !Cil.upointType)]), !Cil.upointType)]
                    )),
                    conditional__bytes last_bytes,
                    expand (index - 1) (Some (read_at_index, index))
                )
        | None -> assert(index>=0);expand (index - 1) (Some (read_at_index, index))
    in
    let block_size = match bytes with
        | Bytes_ByteArray a -> ImmutableArray.length a
        | _ -> 
            let length = Bytes.bytes__length bytes in
            if length = step_size then length (* This handles the case of length-1 arrays. *)
            else FormatPlus.failwith "Not a valid array:@ @[%a@]" BytesPrinter.bytes bytes
    in
    let largest_index = (block_size - len)/step_size in
    if largest_index < 0 then failwith "Error in expand_read_to_conditional2: the read is wider than the memory block";
    expand largest_index None

let rec expand_read_to_conditional (bytes : bytes) (symIndex : bytes) (len : int) =
    let bytes = match bytes with
        | Bytes_Read (a, x, l) -> make_Bytes_Conditional (expand_read_to_conditional a x l)
        | _ -> bytes
    in
    match bytes with
        | Bytes_Write (a, x, l, v) ->
            (* TODO: this seems to be incorrect: shouldn't the length be checked too? *)
            IfThenElse (
                guard__bytes (make_Bytes_Op (
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

        | Bytes_Symbolic _ ->
            (* TODO: We should flag this as an error; it means we are reading an uninitialized value. *)
            Unconditional (make_Bytes_Read (bytes, symIndex, len))

        | _ ->
              (* Handle the common case of indexing into an array of pointers.
                 In this case, the expression is something like [ptrs[i]], and
                 the byte offset becomes (0 + (ptr_size * i)). We know such an
                 offset can only take on values that are multiples of
                 ptr_size, so we don't need to check every single offset. *)
              match symIndex with
                | Bytes_Op (OP_PLUS, [(x, _); (Bytes_Op (OP_MULT, [(Bytes_Constant (CInt64 (n, _, _)), _); (symbolic_index, _)]), _)])
                        when bytes__equal x bytes__zero ->
                      expand_read_to_conditional2 bytes symbolic_index len (Cil.i64_to_int n)
                | _ ->
                      expand_read_to_conditional2 bytes symIndex len 1
