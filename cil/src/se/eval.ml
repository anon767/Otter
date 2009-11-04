open Cil
open Types
open Operation

let rec

rval state exp : state * bytes =
	(*try*)
	let result =
		match exp with
			| Const (constant) -> 
					begin match constant with
						| CStr(str) ->
							let bytes = Convert.constant_to_bytes constant in
							let block = MemOp.string_table__add bytes in
							(state, make_Bytes_Address(Some(block),MemOp.bytes__zero))
						| _ ->
							(state, Convert.lazy_constant_to_bytes constant)
					end

			| Lval (cil_lval) ->
					let state, lvals = lval state cil_lval in
					let size = (Cil.bitsSizeOf (Cil.typeOfLval cil_lval))/8 in
					let rec get_bytes state = function
						| Lval_Block (block, offset) ->
							MemOp.state__get_bytes_from_lval state (block, offset, size)
						| Lval_May (indicator, lvals1, lvals2) ->
							let state, bytes1 = get_bytes state lvals1 in
							let state, bytes2 = get_bytes state lvals2 in
							state, make_Bytes_MayBytes (indicator, bytes1, bytes2)
						| Lval_IfThenElse (c, lvals1, lvals2) ->
							let state, bytes1 = get_bytes state lvals1 in
							let state, bytes2 = get_bytes state lvals2 in
							state, make_Bytes_IfThenElse (c, bytes1, bytes2)
					in
					get_bytes state lvals
					
			|	SizeOf (typ) ->
					let exp2 = Cil.sizeOf typ in
					begin match exp2 with
						| SizeOf(_) ->
							failwith ("Cannot determine sizeof("^(To_string.typ typ)^")")
						| _ ->
							let state, bytes = rval state exp2 in
							begin match bytes with
								| Bytes_Constant(CInt64(n,_,stropt)) ->
									state, make_Bytes_Constant(CInt64(n,!kindOfSizeOf,stropt))
								| b ->
									state, b
							end
					end
					
			|	SizeOfE (exp2) ->
					rval state (SizeOf (Cil.typeOf exp2))
					
			|	SizeOfStr (str) ->
					let len = (String.length str)+1  in
					let exp2 = Cil.integer len in
					rval state  exp2			
					
			|	AlignOf (typ) ->
					failwith "__align_of not implemented"
			|	AlignOfE (exp2) ->
					failwith "__align_of not implemented"
			|	UnOp (unop, exp1, _) ->
					rval_unop state unop exp1
			|	BinOp (binop, exp1, exp2, _) ->
					rval_binop state binop exp1 exp2
			|	AddrOf (Var varinfo, _) when Cil.isFunctionType (varinfo.Cil.vtype) ->
					let fundec = Cilutility.search_function varinfo in
					let f_addr = MemOp.bytes__random Types.word__size in (* TODO: assign an addr for each function ptr *)
					(state, make_Bytes_FunPtr(fundec,f_addr))
			|	AddrOf (cil_lval)
			|	StartOf (cil_lval) ->
					let state, lvals = lval state cil_lval in
					let rec get_addrof = function
						| Lval_Block (block, offset) ->
							make_Bytes_Address(Some(block), offset)
						| Lval_May (indicator, lvals1, lvals2) ->
							make_Bytes_MayBytes (indicator, get_addrof lvals1, get_addrof lvals2)
						| Lval_IfThenElse (c, lvals1, lvals2) ->
							make_Bytes_IfThenElse (c, get_addrof lvals1, get_addrof lvals2)
					in
					(state, get_addrof lvals)
			|	CastE (typ, exp2) ->
					let state, bytes = rval state exp2 in
					(state, rval_cast typ bytes (Cil.typeOf exp2))
	in
	result
	
and
   
(** rvtyp: the type of rv. if rv is a signed int then we will need logical shifting; no otherwise. *)
rval_cast typ rv rvtyp =
	begin
	match rv,typ with
		(* optimize for casting among int family *)
		| Bytes_Constant(CInt64(n,ikind,_)),TInt(new_ikind,_) -> 
			begin match Cil.kinteger64 new_ikind n with
			    Const (const) -> make_Bytes_Constant(const)
			  | _ -> failwith "rval_cast, const: unreachable"
			end						
		(* optimize for casting among float family *)
		| Bytes_Constant(CReal(f,fkind,s)),TFloat(new_fkind,_) -> 
			let const = CReal(f,new_fkind,s) in
         make_Bytes_Constant(const)
		(* added so that from now on there'll be no make_Bytes_Constant *)
		| Bytes_Constant(const),_ ->
			rval_cast typ (Convert.constant_to_bytes const) rvtyp
			
		| _ ->	
			begin
			let old_len = MemOp.bytes__length rv in
			let new_len = (Cil.bitsSizeOf typ) / 8 in
			let worst_case () = (* as function so that it's not always evaluated *)
					if new_len < old_len 
					then MemOp.bytes__read rv (Convert.lazy_int_to_bytes 0) new_len
					else 
						(* TODO: should call STP's sign extension operation *)
						MemOp.bytes__write (MemOp.bytes__make new_len) (Convert.lazy_int_to_bytes 0) old_len rv 

			in
			if new_len = old_len then rv (* do nothing *)
			else begin match rv with
				| Bytes_ByteArray(bytearray) ->	
					if new_len > old_len 
					then
						if Convert.isConcrete_bytearray bytearray 
						then 
							begin			
								let newbytes = (ImmutableArray.sub bytearray 0 new_len) in
								let isSigned = match rvtyp with TInt(ikind,_) when Cil.isSigned ikind -> true | _ -> false in
								let leftmost_is_one = 
									match ImmutableArray.get bytearray ((ImmutableArray.length bytearray)-1) with
										| Byte_Concrete (c) -> Char.code c >= 0x80
										| _ -> failwith "unreachable (bytearray is concrete)"
								in
								let sth = if isSigned && leftmost_is_one 
										then MemOp.byte__111 (* For some reason, this seems not to happen in practice *)
										else MemOp.byte__zero
								in
								let rec pack_sth newbytes old_len new_len =
									if old_len>=new_len then newbytes else
									let newbytes2 = ImmutableArray.set newbytes old_len sth in
										pack_sth newbytes2 (old_len+1) new_len
								in
									make_Bytes_ByteArray (pack_sth newbytes old_len new_len)
							end
						else worst_case () (* don't know how to do if new_len > old_len && bytearray is NOT concrete *)
					else (* new_len < old_len *)
						make_Bytes_ByteArray (ImmutableArray.sub bytearray 0 new_len) (* simply truncate *)
				| Bytes_Constant(const) -> failwith "unreachable"
				| _ -> worst_case ()
				end
			end
	end
(*																														
			else if new_len > old_len then

*)
and

lval state (lhost, offset_exp) = match lhost with
	| Var(varinfo) ->
		let block = MemOp.state__varinfo_to_block state varinfo in
		let state, offset, _ = flatten_offset state varinfo.vtype offset_exp in
		(state, Lval_Block (block, offset))
	| Mem(exp) ->
		let state, rv = rval state exp in
		let lvals = deref state rv in
		let state, offset, _ = flatten_offset state (Cil.typeOf exp) offset_exp in
		let rec add_offset = function
			| Lval_Block (block, offset2) ->
				Lval_Block (block, Operation.plus [(offset,Cil.intType);(offset2,Cil.intType)])
			| Lval_May (indicator, lvals1, lvals2) ->
				Lval_May (indicator, add_offset lvals1, add_offset lvals2)
			| Lval_IfThenElse (c, lvals1, lvals2) ->
				Lval_IfThenElse (c, add_offset lvals1, add_offset lvals2)
		in
		(state, add_offset lvals)

and

(* harder than I thought! *)
deref state bytes =
	(*Output.set_mode Output.MSG_MUSTPRINT;  tmp *)
	match bytes with
		| Bytes_Constant (c) -> failwith ("Dereference something not an address (constant) "^(To_string.bytes bytes))
		| Bytes_ByteArray(bytearray) -> 
            (* 
            * Special treatment: look for
             * "==(Bytearray(bytearray),make_Bytes_Address(b,f))" in PC.
             * If found, return deref state make_Bytes_Address(b,f).
             * Otherwise, throw exception
            * *)
            let rec find_match pc = match pc with [] -> 
                failwith ("Dereference something not an address (bytearray) "^(To_string.bytes bytes))
              | Bytes_Op(OP_EQ,(bytes1,_)::(bytes2,_)::[])::pc' -> 
                  begin
                    let bytes_tentative = if bytes1=bytes then bytes2 else if bytes2=bytes then bytes1 else MemOp.bytes__zero in 
                      match bytes_tentative with Bytes_Address(_,_) -> deref state bytes_tentative | _ -> find_match pc'
                  end
              | Bytes_Op(OP_LAND,btlist)::pc' ->
                  find_match (List.rev_append (List.fold_left (fun a (b,_) -> b::a) [] btlist) pc')
              | _::pc' -> find_match pc'
            in
              find_match state.path_condition
		| Bytes_Address(Some(block), offset) ->
			if MemOp.state__has_block state block
			then Lval_Block (block, offset)
			else failwith "Dereference into an expired stack frame"
		| Bytes_Address(None, offset) -> failwith "Dereference a dangling pointer"
		| Bytes_MayBytes (indicator, bytes1, bytes2) ->
			(* TODO: update the pointer after the below pruning *)
			begin match Stp.query_indicator state.path_condition indicator with
				| Stp.True -> deref state bytes1
				| Stp.False -> deref state bytes2
				| Stp.Unknown -> Lval_May (indicator, deref state bytes1, deref state bytes2)
			end
		| Bytes_IfThenElse (bytes0, bytes1, bytes2) ->
			begin match Stp.consult_stp state.path_condition bytes0 with
				| Stp.True -> deref state bytes1
				| Stp.False -> deref state bytes2
				| Stp.Unknown -> Lval_IfThenElse (bytes0, deref state bytes1, deref state bytes2)
			end
		| Bytes_Op(op, operands) -> failwith ("Dereference something not an address (op) "^(To_string.bytes bytes))
		| Bytes_Read(bytes,off,len) ->failwith "Dereference: Not implemented"
		| Bytes_Write(bytes,off,len,newbytes) ->failwith "Dereference: Not implemented"
		| Bytes_FunPtr(_) -> failwith "Dereference funptr not support"
		| Bytes_Unbounded(_,_,_) ->failwith "Dereference: Not implemented"
(*		| Bytes_PtrToConstantBytes (content,off) ->failwith "Dereference: Can't dereference constant array in lval."*)
			

and

(* Assume index's ikind is IInt *)
flatten_offset state lhost_typ offset : state * bytes * typ (* type of (lhost,offset) *) =
  let (state, final_bytes,final_typ) = 
	match offset with
		| NoOffset -> (state, MemOp.bytes__zero, lhost_typ) (* TODO: bytes__zero should be defined in Convert *)
		| _ -> 
			let (state, index, base_typ, offset2) =
				begin match offset with
					| Field(fieldinfo, offset2) ->
							let n = field_offset fieldinfo in
							let index = Convert.lazy_int_to_bytes n in
							let base_typ = fieldinfo.ftype in
							(state, index, base_typ, offset2)
					| Index(exp, offset2) ->
							let state, rv0 = rval state exp in
							(* TODO: right thing to do?*)
							let rv =
								if MemOp.bytes__length rv0 <> word__size
								then rval_cast Cil.intType rv0 (Cil.typeOf exp)
								else rv0
							in
							let typ = Cil.typeOf exp in
							let base_typ = match Cilutility.unrollType lhost_typ with TArray(typ2, _, _) -> typ2 | _ -> failwith "Must be array" in
							let base_size = (Cil.bitsSizeOf base_typ) / 8 in (* must be known *)
							(* TODO: if typ is not IInt, should we change it to? *)
							let index = Operation.mult [(Convert.lazy_int_to_bytes base_size,Cil.intType);(rv,typ)] in 
							(state, index, base_typ, offset2)
					| _ -> failwith "Unreachable"
				end
			in
				let (state, index2, base_typ2) = flatten_offset state base_typ offset2 in
				let index3 = Operation.plus [(index,Cil.intType);(index2,Cil.intType)] in
					(state, index3, base_typ2)
  in
    (* if typ is not IInt, fix it!   <---- this is WRONG
    
    if MemOp.bytes__length final_bytes <> word__size then
        (rval_cast Cil.intType final_bytes, Cil.intType)
    else
     *)
        (state, final_bytes,final_typ)

and

(* This does not align fields based on their type. For example, a
	 struct { char c; int n; } would have n at offset 1, whereas gcc has
	 ints 4-byte aligned, so it is at offset 4. I'm not exactly sure
	 what gcc does generally, though. Also, the C specification seems to
	 leave alignment implementation-defined [6.7.2.1.12], so not
	 padding for alignment should be okay. *)
field_offset f : int =
	if not f.fcomp.cstruct
	then 0 (* This is a field in a union, so the offset is 0. *)
	else (* For structs, we have to actually figure out the offset *)
		let rec helper fields acc =
			match fields with
			| [] -> failwith "unreachable: field doesn't appear in struct!"
			| head:: tail ->
					if head == f then acc
					else helper tail (acc + (Cil.bitsSizeOf head.ftype) / 8)
		in
		helper f.fcomp.cfields 0
 
and

rval_unop state unop exp =
	let state, rv = rval state exp in
	let typ = Cil.typeOf exp in
	let result = Operation.run (Operation.of_unop unop) [(rv,typ)] in
	(state, result)

and

rval_binop state binop exp1 exp2 =
	let op = (Operation.of_binop binop) in
	let state, rv1 = rval state exp1 in
	let typ1 = Cil.typeOf exp1 in
	(* shortcircuiting *)
	if op == Operation.logand && Convert.isConcrete_bytes rv1 && Convert.bytes_to_bool rv1 = false then
		(state, Convert.lazy_int_to_bytes 0)
	else if op == Operation.logor && Convert.isConcrete_bytes rv1 && Convert.bytes_to_bool rv1 = true then
		(state, Convert.lazy_int_to_bytes 1)
	else 
		let state, rv2 = rval state exp2 in
		let typ2 = Cil.typeOf exp2 in
		(state, Operation.run op [(rv1,typ1);(rv2,typ2)])
;;
	
