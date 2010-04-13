open Cil
open Bytes
open Types

(**
 *	bytes
 *)

let rec bytes__read ?test ?pre bytes off len =
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
			| Bytes_Conditional c, _ ->
				let c = conditional__map ?test ~eq:bytes__equal ?pre (fun e -> conditional__bytes (bytes__read e off len)) c in
				make_Bytes_Conditional c
			| _, Bytes_Conditional c ->
				failwith "bytes__read: if-then-else offset doesn't happen"
			| _, _ when (bytes__length bytes = len) && (isConcrete_bytes off) && (bytes_to_int_auto off = 0) ->
				bytes
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

let bytes__write ?test ?pre bytes off len newbytes =
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

			| Bytes_Conditional c, _, _ ->
				let c = conditional__map ?test ~eq:bytes__equal ?pre (fun e -> conditional__bytes (do_write e off len newbytes)) c in
				make_Bytes_Conditional c
			| _, Bytes_Conditional c, _ ->
				failwith "bytes__write: if-then-else offset doesn't happen"

			| _ -> make_Bytes_Write (bytes,off,len,newbytes)
	in
	if (bytes__length bytes)=len && (isConcrete_bytes off) && (bytes_to_int_auto off = 0) then 
      newbytes 
	else
		do_write bytes off len newbytes

;;

(*let rec getMaxBlockSizes cond = 
        match cond with
                | IfThenElse (guard, x, y) -> max (getMaxBlockSizes x) (getMaxBlockSizes y)
                | Unconditional bytes -> 
                        match bytes with
                                | Bytes_Address(block, offset) -> (block.memory_block_size)
                                | Bytes_Conditional(c) -> getMaxBlockSizes c
                                | Bytes_ByteArray(a) -> (ImmutableArray.length a)
                                | _ -> failwith ("Not a valid array.  : "^(To_string.bytes bytes))
;;*)
let rec expand_read_to_conditional2 bytes index len symIndex = 
        let max = match bytes with
                | Bytes_Address(block, offset) -> (block.memory_block_size)
                | Bytes_ByteArray(a) -> (ImmutableArray.length a)
                | Bytes_Conditional(c) -> failwith "Unexpected Bytes_Conditional"(*getMaxBlockSizes c*)
                | _ -> failwith ("Not a valid array. : "^(To_string.bytes bytes))
        in
        if (index < max - len) then (*don't read past the end of the array*)
                IfThenElse(
                        Guard_Bytes(make_Bytes_Op (
                                OP_EQ,
                                [(symIndex, Cil.intType); ((lazy_int_to_bytes index), Cil.intType)]
                        )),
                        (
                                match bytes__read bytes (lazy_int_to_bytes (index)) len with
                                        | Bytes_Conditional(c) -> c
                                        | b -> Unconditional(b)
                        ),
                        (expand_read_to_conditional2 bytes (index+1(*len*)) len symIndex)
                )
        else (*last read that fits in the memory block*)
                (
                        match bytes__read bytes (lazy_int_to_bytes (index)) len with
                                | Bytes_Conditional c -> c
                                | b -> Unconditional(b)
                )
;;
let rec expand_read_to_conditional (bytes:bytes) (symIndex:bytes) (len:int) = 
        let bytes = match bytes with
                | Bytes_Read(a, x, l) -> Bytes_Conditional(expand_read_to_conditional a x l)
                | _ -> bytes
        in
	match bytes with
		| Bytes_Write(a, x, l, v) -> 
				IfThenElse(
					Guard_Bytes(make_Bytes_Op (
						OP_EQ,
						[(symIndex, Cil.intType); (x, Cil.intType)]
					)),	
					(Unconditional v),	
					(expand_read_to_conditional a symIndex len)
				)
			
		| Bytes_Conditional c -> 
			let map_func leaf =
				match leaf with
					| Bytes_Read(a, x, l) -> expand_read_to_conditional a x l
					| Bytes_Conditional c -> expand_read_to_conditional leaf symIndex len
						(* ^^ someone hid another conditional tree in here*)
					(*| Bytes_Write(a, x, l, v) -> 
						IfThenElse(
							Guard_Bytes(make_Bytes_Op (
								OP_EQ,
								[(symIndex, Cil.intType); (x, Cil.intType)]
							)),	
							(Unconditional v),	
							(expand_read_to_conditional a symIndex len)
						)*)
	                                | _ -> expand_read_to_conditional2 bytes 0 len symIndex
			in
			conditional__map map_func c
		| _ -> expand_read_to_conditional2 bytes 0 len symIndex
;;



let rec prune_conditional_bytes state cond =
	let fold_func acc pre leaf =
		((), Unconditional(leaf))
	in
	snd (conditional__map_fold ~test:(fun a b -> Stp.query_guard state.path_condition a b) fold_func () cond)
;;
let prune_bytes_conditional state bytes = 
	match bytes with
		| Bytes_Conditional c -> Bytes_Conditional (prune_conditional_bytes state c)
		| _ -> bytes (*failwith "prune_bytes_conditional : not a Bytes_Conditional"*)
;;	

