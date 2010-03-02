open Executeargs
open Cil
open Bytes
open Types
open Operation

(* Print an error message saying that the assertion [bytes] failed in
	 state [state]. [exps] is the expression representing [bytes].
	 [isUnknown] specifies whether the assertion returned
	 Ternary.Unknown (rather than Ternary.False). *)
let print_failed_assertion state bytes exps ~isUnknown =
	Output.set_mode Output.MSG_MUSTPRINT;
	Output.print_endline "Assertion not-satisfied (see error log).";
	let oldPrintNothingVal = print_args.arg_print_nothing in
	Output.set_mode Output.MSG_MUSTPRINT;
	print_args.arg_print_nothing <- false; (* Allow printing for the log *)
	Executedebug.log "\n(****************************";
	Executedebug.log "Assertion:";
	Executedebug.log (Utility.print_list To_string.exp exps " and "); 
	Executedebug.log "which becomes";
	Executedebug.log (To_string.bytes bytes);
	Executedebug.log ((if isUnknown then "can be" else "is") ^ " false with the path condition:");
	(*let pc_str = To_string.humanReadablePc state.path_condition exHist.bytesToVars in *)
	(*let pc_str = (Utility.print_list To_string.bytes state.path_condition " AND ") in*)
	(*let pc_str = Utility.print_list To_string.bytes (Stp.getRelevantAssumptions state.path_condition post) " AND " in*)
	let pc_str = (Utility.print_list To_string.bytes state.path_condition " AND ") in
	Executedebug.log (if pc_str = "" then "true" else pc_str);
	Executedebug.log "****************************)";
	print_args.arg_print_nothing <- oldPrintNothingVal
;;

(** Check an assertion in a given state
    @param state            the state in which to check the assertion : state
    @param bytes            the assertion : bytes
    @param exps             the expressions being asserted (used for printing a readable error message) : exp list
    @raise Failure          if the assertion is always false
    @return state           if the assertion is always true, this is the input state; otherwise, an error message is printed and the return value is the input state with [bytes] added to the path condition
*)
let check state bytes exps =
	match Stp.eval state.path_condition bytes with
		Ternary.True -> (* The assertion is true *)
			Output.set_mode Output.MSG_REG;
			Output.print_endline "Assertion satisfied.";
			state
	| Ternary.False -> (* The assertion is definitely false *)
			print_failed_assertion state bytes exps ~isUnknown:false;
			failwith "Assertion was false"
	| Ternary.Unknown -> (* The assertion is false in some states but not all *)
			print_failed_assertion state bytes exps ~isUnknown:true;
			(* Assume the assertion *)
			MemOp.state__add_path_condition state bytes true (* This [true] marks the assumption as though it came from an actual branch in the code *)
;;

(* We can't check each leaf of {lvals}'s conditional tree on its own,
	 because we don't want to completely fail just because *some*
	 possibility can be out of bounds. (We only want to do that if *all*
	 possibilities are out of bounds.) So instead, we compute two
	 conditional trees, {sizes} and {offsets}, with the same shape and
	 guards as {lvals} and which contain at the leaves, respectively,
	 the size of the memory block and the offset of the Bytes_Address at
	 the correpond leaf in {lvals}. The bounds check then becomes
	 {offsets < sizes}. *)
(* TODO: Should we prune the resulting conditional trees, removing
	 infeasible subtrees, when a bounds check fails? This question
	 applies both to {lvals} itself and to the bytes representing the
	 assertion being checked (which will then be assumed into the path
	 condition). *)
let rec getBlockSizesAndOffsets lvals = match lvals with
		| IfThenElse (guard, x, y) ->
				let blockSizesX, offsetsX = getBlockSizesAndOffsets x
				and blockSizesY, offsetsY = getBlockSizesAndOffsets y in
				IfThenElse (guard, blockSizesX, blockSizesY),
				IfThenElse (guard, offsetsX, offsetsY)
		| Unconditional (block,offset) ->
				Unconditional (int64_to_offset_bytes block.memory_block_size),
				Unconditional offset
;;

(* offsets are treated as values of type !upointType, so it is
	 impossible for them to be negative. This means we only need to
	 check for overflow, not underflow. *)
let checkBounds state lvals cil_lval =
	let blockSizeTree, offsetTree = getBlockSizesAndOffsets lvals in
	let offsetLtBlockSize =
		make_Bytes_Op (OP_LT, [(make_Bytes_Conditional offsetTree, !Cil.upointType);
													 (make_Bytes_Conditional blockSizeTree, !Cil.upointType)])
	and expRepresentingBoundsCheck = BinOp (Eq, Lval cil_lval, SizeOfStr "is in bounds", voidType) in
	check state offsetLtBlockSize [expRepresentingBoundsCheck]
;;

let add_offset state offset lvals : state * (Types.MemoryBlockMap.key * Bytes.bytes) Bytes.conditional =
	conditional__map_fold begin fun newState _ (block, offset2) ->
		let newOffset = Operation.plus [(offset, !Cil.upointType); (offset2, !Cil.upointType)] in
		(newState, conditional__lval_block (block, newOffset))
	end state lvals
;;

let rec getMaxBlockSizes cond = 
        match cond with
                | IfThenElse (guard, x, y) -> max (getMaxBlockSizes x) (getMaxBlockSizes y)
                | Unconditional bytes -> 
                        match bytes with
                                | Bytes_Address(block, offset) -> (block.memory_block_size)
                                | Bytes_Conditional(c) -> getMaxBlockSizes c
                                | Bytes_ByteArray(a) -> (ImmutableArray.length a)
                                | _ -> failwith ("Not a valid array.  : "^(To_string.bytes bytes))
;;
let rec expand_read_to_conditional2 state bytes index len symIndex = 
        let max = match bytes with
                | Bytes_Address(block, offset) -> (block.memory_block_size)
                | Bytes_ByteArray(a) -> (ImmutableArray.length a)
                | Bytes_Conditional(c) -> getMaxBlockSizes c
                | _ -> failwith ("Not a valid array. : "^(To_string.bytes bytes))
        in
        if (index < max - len) then
                IfThenElse(
                        Guard_Bytes(make_Bytes_Op (
                                OP_EQ,
                                [(symIndex, Cil.intType); ((lazy_int_to_bytes index), Cil.intType)]
                        )),
                        (*deref state (make_Bytes_Read (bytes, Bytes_Constant(index), len)),*)
                        (
                                match bytes__read bytes (lazy_int_to_bytes (index)) len with
                                        | Bytes_Conditional(c) -> c
                                        | b -> Unconditional(b)
                        ),
                        (expand_read_to_conditional2 state bytes (index+len) len symIndex)
                )
        else
                (*deref state (make_Bytes_Read (bytes, Bytes_Constant(index), len))*)
                (
                        match bytes__read bytes (lazy_int_to_bytes (index)) len with
                                | Bytes_Conditional c -> c
                                | b -> Unconditional(b)
                )
;;
let rec expand_read_to_conditional state bytes len symIndex = 
        let bytes = match bytes with
                | Bytes_Read(a, x, l) -> Bytes_Conditional(expand_read_to_conditional state a l x)
                | _ -> bytes
        in
                expand_read_to_conditional2 state bytes 0 len symIndex
;;

let rec

rval state exp : state * bytes =
	(*try*)
	let result =
		match exp with
			| Const (constant) -> 
					begin match constant with
						| CStr(str) ->
							let bytes = constant_to_bytes constant in
							let block = MemOp.string_table__add bytes in
							(state, make_Bytes_Address(block, bytes__zero))
						| _ ->
							(state, lazy_constant_to_bytes constant)
					end

			| Lval (cil_lval) ->
					let state, lvals = lval state cil_lval in
					MemOp.state__deref state lvals

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
					let f_addr = bytes__random (Cil.bitsSizeOf Cil.voidPtrType / 8) in (* TODO: assign an addr for each function ptr *)
					(state, make_Bytes_FunPtr(fundec,f_addr))
			|	AddrOf (cil_lval)
			|	StartOf (cil_lval) ->
					let state, (lvals, _) = lval ~justGetAddr:true state cil_lval in
					let c = conditional__map begin fun (block, offset) ->
						conditional__bytes (make_Bytes_Address(block, offset))
					end lvals in
					(state, make_Bytes_Conditional c)
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
			rval_cast typ (constant_to_bytes const) rvtyp
			
		| _ ->	
			begin
			let old_len = bytes__length rv in
			let new_len = (Cil.bitsSizeOf typ) / 8 in
			let worst_case () = (* as function so that it's not always evaluated *)
					if new_len < old_len 
					then bytes__read rv (lazy_int_to_bytes 0) new_len
					else 
						(* TODO: should call STP's sign extension operation *)
						bytes__write (bytes__make new_len) (lazy_int_to_bytes 0) old_len rv 

			in
			if new_len = old_len then rv (* do nothing *)
			else begin match rv with
				| Bytes_ByteArray(bytearray) ->	
					if new_len > old_len 
					then
						if isConcrete_bytearray bytearray 
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
										then byte__111 (* For some reason, this seems not to happen in practice *)
										else byte__zero
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

(* justGetAddr is set to true when the lval is the operand of the
	 address-of operator, '&'. For example, &x[i] is legal even if i is
	 not in bounds. (Sort of. The spec (6.5.6.8) implies that this is
	 only defined if i is *one* past the end of x.) *)
lval ?(justGetAddr=false) state (lhost, offset_exp as cil_lval) =
	let size = (Cil.bitsSizeOf (Cil.typeOfLval cil_lval))/8 in
	let state, lvals, lhost_type =
	match lhost with
		| Var(varinfo) ->
			let state, lvals = MemOp.state__varinfo_to_lval_block state varinfo in
			(state, lvals, varinfo.vtype)
		| Mem(exp) ->
			let state, rv = rval state exp in
			let lvals = deref state rv in
			(state, lvals, Cil.typeOf exp)
	in
	let state, offset, _ = flatten_offset state lhost_type offset_exp in
	(* Add the offset, then see if it was in bounds *)
	let state, lvals = add_offset state offset lvals in
	(* Omit the bounds check if we're only getting the address of the
		 lval---not actually reading from or writing to it---or if bounds
		 checking is turned off *)
	if justGetAddr || not run_args.arg_bounds_checking
	then state, (lvals, size)
	else (
		(* Optimization: An offset of 0 is always in bounds. We could do a
			 full check of {lvals} to see if every leaf of the tree is zero,
			 but that might be expensive. Instead, do a quick check to see if
			 {lvals} has the form {Unconditional (_,bytes__zero)}. This comes
			 up, for example, on *every* variable lookup, so it is probably
			 worth optimizing. *)
		match lvals with
				Unconditional (_, offset) when offset = bytes__zero ->
					state, (lvals, size)
			| _ ->
					checkBounds state lvals cil_lval, (lvals, size)
	)

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
                    let bytes_tentative = if bytes1=bytes then bytes2 else if bytes2=bytes then bytes1 else bytes__zero in 
                      match bytes_tentative with Bytes_Address(_,_) -> deref state bytes_tentative | _ -> find_match pc'
                  end
              | Bytes_Op(OP_LAND,btlist)::pc' ->
                  find_match (List.rev_append (List.fold_left (fun a (b,_) -> b::a) [] btlist) pc')
              | _::pc' -> find_match pc'
            in
              find_match state.path_condition
		| Bytes_Address(block, offset) ->
			if MemOp.state__has_block state block
			then conditional__lval_block (block, offset)
			else failwith "Dereference into an expired stack frame"
		| Bytes_Conditional c ->
			conditional__map (deref state) c
		| Bytes_Op(op, operands) -> failwith ("Dereference something not an address (op) "^(To_string.bytes bytes))
		| Bytes_Read(bytes,off,len) ->failwith "Dereference: Not implemented"
		| Bytes_Write(bytes,off,len,newbytes) ->failwith "Dereference: Not implemented"
		| Bytes_FunPtr(_) -> failwith "Dereference funptr not support"
		| Bytes_Unbounded(_,_,_) ->failwith "Dereference: Not implemented"
			

and

(* Assume index's ikind is IInt *)
flatten_offset state lhost_typ offset : state * bytes * typ (* type of (lhost,offset) *) =
	match offset with
		| NoOffset -> (state, bytes__zero, lhost_typ)
		| _ -> 
			let (state, index, base_typ, offset2) =
				begin match offset with
					| Field(fieldinfo, offset2) ->
							let n = field_offset fieldinfo in
							let index = int64_to_offset_bytes n in
							let base_typ = fieldinfo.ftype in
							(state, index, base_typ, offset2)
					| Index(exp, offset2) ->
							let state, rv0 = rval state exp in
							(* We require that offsets be values of type !upointType *)
							let rv =
								if bytes__length rv0 <> (Cil.bitsSizeOf !Cil.upointType / 8) 
								then rval_cast !Cil.upointType rv0 (Cil.typeOf exp)
								else rv0
							in
							let base_typ = match Cilutility.unrollType lhost_typ with TArray(typ2, _, _) -> typ2 | _ -> failwith "Must be array" in
							let base_size = (Cil.bitsSizeOf base_typ) / 8 in (* must be known *)
							let index = Operation.mult [(int64_to_offset_bytes base_size,!Cil.upointType);(rv,!Cil.upointType)] in 
							(state, index, base_typ, offset2)
					| _ -> failwith "Unreachable"
				end
			in
				let (state, index2, base_typ2) = flatten_offset state base_typ offset2 in
				let index3 = Operation.plus [(index,!Cil.upointType);(index2,!Cil.upointType)] in
					(state, index3, base_typ2)

and

(* Calculate field offsets according to Cil, which should mimic the underlying compiler (e.g., gcc).
   The C specification leave alignment implementation-defined [6.7.2.1.12]. *)
field_offset f : int =
    let offset, _ = bitsOffset (TComp (f.fcomp, [])) (Field (f, NoOffset)) in
    offset / 8
 
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
	if op == Operation.logand && isConcrete_bytes rv1 && bytes_to_bool rv1 = false then
		(state, lazy_int_to_bytes 0)
	else if op == Operation.logor && isConcrete_bytes rv1 && bytes_to_bool rv1 = true then
		(state, lazy_int_to_bytes 1)
	else 
		let state, rv2 = rval state exp2 in
		let typ2 = Cil.typeOf exp2 in
		(state, Operation.run op [(rv1,typ1);(rv2,typ2)])
;;
	
