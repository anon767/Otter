open DataStructures
open OcamlUtilities
open Cil
open OtterBytes
open Bytes
open BytesUtility
open Types
open Operator

exception ConditionalFailureException of Bytes.bytes * string

(* Print an error message saying that the assertion [bytes] failed in
	 state [state]. [exps] is the expression representing [bytes].
	 [isUnknown] specifies whether the assertion returned
	 Ternary.Unknown (rather than Ternary.False). *)
let print_failed_assertion state bytes exps ~isUnknown =
	Output.set_mode Output.MSG_MUSTPRINT;
	Output.printf "Assertion not-satisfied (see error log).@\n";
	Output.set_mode Output.MSG_MUSTPRINT;
	FormatPlus.ksprintf Executedebug.log
		"\
		(****************************@\n\
		Assertion:@\n\
		\  @[%a@]@\n\
		@\n\
		which becomes@\n\
		\  @[%a@]@\n\
		@\n\
		%s false with the path condition:@\n\
		\  @[  %t@]@\n\
		****************************)@\n\
		"
		(FormatPlus.pp_print_list Printer.exp "@ and ") exps
		BytesPrinter.bytes bytes
		(if isUnknown then "can be" else "is")
		begin fun ff ->
			if state.path_condition = [] then
				Format.pp_print_string ff "true"
			else
				FormatPlus.pp_print_list BytesPrinter.bytes "@\nAND@\n  " ff state.path_condition
		end


(** Check an assertion in a given state
    @param state            the state in which to check the assertion : state
    @param bytes            the assertion : bytes
    @param exps             the expressions being asserted (used for printing a readable error message) : exp list
    @raise Failure          if the assertion is always false
    @return state           if the assertion is always true, this is the input state; otherwise, an error message is printed and the return value is the input state with [bytes] added to the path condition
*)
let check state bytes exps =
  let state,result =  MemOp.state__eval state state.path_condition bytes in
	match result with
		Ternary.True -> (* The assertion is true *)
			Output.set_mode Output.MSG_REG;
			Output.printf "Assertion satisfied.@\n";
			state
	| Ternary.False -> (* The assertion is definitely false *)
			print_failed_assertion state bytes exps ~isUnknown:false;
			failwith "Assertion was false"
	| Ternary.Unknown -> (* The assertion is false in some states but not all *)
			print_failed_assertion state bytes exps ~isUnknown:true;
			(* Assume the assertion *)
			MemOp.state__add_path_condition state bytes true (* This [true] marks the assumption as though it came from an actual branch in the code *)
   (* CCBSE: we can collect failing conditions here *)


(* Bounds-checking *)
(* The next two function are used for bounds-checking. Here are some
	 comments:

	 Offsets are treated as values of type !upointType, which is
	 unsigned, so it is impossible for them to be negative. This means
	 we only need to check for overflow, not underflow.

	 We can't check the bounds for each leaf of a conditional tree on its own,
	 because we don't want to completely fail just because *some*
	 possibility can be out of bounds. (We only want to fail if *all*
	 possibilities are out of bounds.) So instead, we compute two
	 conditional trees, {sizes} and {offsets}, with the same shape and
	 guards as the input {lvals} and which contain at the leaves, respectively,
	 the size of the memory block and the offset of the Bytes_Address at
	 the correpond leaf in {lvals}.

	 You might think that you only need to check
	 {offsets + useSize <= sizes}, where {useSize} is the size with
	 which {lvals} is about to be read or written). But this isn't
	 enough because of possible overflow (e.g., if {offsets} contains
	 the value -1 == 0xffffffff). To catch overflow, you need to
	 additionally check that {offsets < offsets + useSize}.

	 Interestingly, a different but seemingly equivalent pair of checks,
	 {offsets < sizes} and {offsets + useSize <= sizes}, actually does
	 not work properly, because it does not catch the possible overflow
	 of the addition. This pair of checks would fail to catch an error
	 in a case where {offsets + size < offsets < sizes}. This can only
	 happen if {useSize} or {sizes} is very large, which is quite
	 unlikely---{useSize} is the size of a type, so it is almost
	 certainly small; and {sizes} holds the sizes of allocated regions,
	 which are also not likely to be close to the maximum pointer value.
	 However, if we can catch even this, why not? Also, if we eventually
	 allow symbolic sized allocations, {sizes} could in fact be this
	 large.

	 Alternatively, here's another way to do it. Rewrite
	 {offsets + useSize <= sizes} to {offsets <= sizes - useSize}. As
	 before, this gets the job done unless there is overflow---well, in
	 this case, underflow. Now, to catch underflow, the additional check
	 is {useSize <= sizes}. (This has to be non-strict because it is
	 okay if they are equal.) This seems to be simpler than the second
	 check above, {offsets < offsets + useSize}, in two ways: first,
	 there is only one conditional tree in this check instead of two.
	 Second, {size} is concrete, and each leaf of {sizes} is concrete;
	 and even if we allow symbolic sized allocations in the future, most
	 allocations will probably still be concrete. *)

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
				Unconditional (int_to_offset_bytes block.memory_block_size),
				Unconditional offset


let checkBounds state lvals cil_lval useSize =
	Output.printf "Checking bounds of @[%a@]@\n" Printcil.lval cil_lval;

	(* Get the block sizes and offsets *)
	let sizesTree, offsetsTree = getBlockSizesAndOffsets lvals in

	(* Make the relevant Bytes *)
	let offsetsBytes = make_Bytes_Conditional offsetsTree
	and sizesBytes = make_Bytes_Conditional sizesTree
	and useSizeBytes = int_to_offset_bytes useSize in

	(* Prepare the first bounds check: {offsets <= sizes - useSize} *)
	let sizesMinusUseSize = Operator.minus [(sizesBytes, !Cil.upointType); (useSizeBytes, !Cil.upointType)] in
	let offsetsLeSizesMinusUseSize = Operator.le [(offsetsBytes, !Cil.upointType); (sizesMinusUseSize, !Cil.upointType)]
	and expRepresentingBoundsCheck1 = BinOp (Eq, Lval cil_lval, SizeOfStr "Checking that offset is in bounds", voidType) in

	(* Do the check and keep the resulting state *)
	let state = check state offsetsLeSizesMinusUseSize [expRepresentingBoundsCheck1] in

	(* Prepare the second bounds check: {useSize <= sizes} *)
	(* TODO: If {sizes} is a conditional tree (rather than a single
		 value), we may want to call conditional__map (with the identity
		 function) to simplify {useSizeLeSizes}. It will almost always
		 have concrete 'true's at every leaf. If we don't simplify, we'll
		 end up calling the solver. *)
	let useSizeLeSizes = Operator.le [(useSizeBytes, !Cil.upointType); (sizesBytes, !Cil.upointType)]
	and expRepresentingBoundsCheck2 = BinOp (Eq, Lval cil_lval, SizeOfStr "Checking that size of type does not exceed allocated size", voidType) in

	(* Do the second check *)
	check state useSizeLeSizes [expRepresentingBoundsCheck2]


let add_offset offset lvals =
	conditional__map begin fun (block, offset2) ->
		conditional__lval_block (block, Operator.plus [(offset, !Cil.upointType); (offset2, !Cil.upointType)])
	end lvals


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
							(state, constant_to_bytes constant)
					end

			| Lval (cil_lval) ->
					let state, lvals = lval state cil_lval in
					MemOp.state__deref state lvals

			|	SizeOf (typ) ->
					let exp2 = Cil.sizeOf typ in
					begin match exp2 with
						| SizeOf(_) ->
							FormatPlus.failwith "Cannot determine sizeof(%a)" Printcil.typ typ
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
			| Question (guard, exp1, exp2, _) ->
					rval_question state guard exp1 exp2
			|	AddrOf (Var varinfo, _) when Cil.isFunctionType (varinfo.Cil.vtype) ->
					let f_addr = bytes__random (Cil.bitsSizeOf Cil.voidPtrType / 8) in (* TODO: assign an addr for each function ptr *)
					(state, make_Bytes_FunPtr(varinfo,f_addr))
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
					then bytes__read rv (int_to_bytes 0) new_len
					else
						(* TODO: should call STP's sign extension operation *)
						bytes__write (bytes__make new_len) (int_to_bytes 0) old_len rv

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
	match cil_lval with
		| Var _, NoOffset -> state, (lvals, size)
		| _ ->
			let state, offset, _ = flatten_offset state lhost_type offset_exp in
			(* Add the offset, then see if it was in bounds *)
			let lvals = add_offset offset lvals in
			(* Omit the bounds check if we're only getting the address of the
				 lval---not actually reading from or writing to it---or if bounds
				 checking is turned off *)
			if justGetAddr || not !Executeargs.arg_bounds_checking
			then state, (lvals, size)
			else (checkBounds state lvals cil_lval size), (lvals, size)

and


deref state bytes =
	match bytes with
		| Bytes_Constant (c) ->
			FormatPlus.failwith "Dereference something not an address:@ constant @[%a@]" BytesPrinter.bytes bytes

		| Bytes_ByteArray(bytearray) ->
         (*
          * Special treatment: look for
          * "==(Bytearray(bytearray),make_Bytes_Address(b,f))" in PC.
          * If found, return deref state make_Bytes_Address(b,f).
          * Otherwise, throw exception
          * *)
         let rec find_match pc = match pc with
           | [] ->
               FormatPlus.failwith "Dereference something not an address (bytearray)@ @[%a@]@." BytesPrinter.bytes bytes
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
			if MemOp.state__has_block state block then
           conditional__lval_block (block, offset)
			else
           failwith "Dereference into an expired stack frame"

		| Bytes_Conditional c ->
            let make_dummy_lval () =
                Unconditional (block__make_string_literal "@dummy_block" 4, bytes__zero)
            in
            let (acc_pass, acc_fail), conditional =
			  conditional__fold_map (fun (acc_pass, acc_fail) guard c ->
                  try
                      (guard::acc_pass, acc_fail), deref state c
                  with
                      Failure msg ->
                          (* Collect a list of failing guard/message pairs *)
                          (acc_pass, (guard, msg)::acc_fail), make_dummy_lval ()
              ) ([],[]) c
            in
                if acc_fail = [] then
                    conditional
                else
                    let failing_condition, aggregated_msg = List.fold_left (
                        fun (failing_condition, aggregated_msg) (guard, msg) ->
                        let bytes_of_guard = Bytes.guard__to_bytes guard in
                            (Bytes.bytes_or failing_condition bytes_of_guard, aggregated_msg ^ "/" ^ msg)
                        ) (Bytes.fls, "(aggregated failure)") acc_fail
                    in
                    if acc_pass = [] then
                        failwith aggregated_msg
                    else
                        (* Caught in Statement.step *)
                        raise (ConditionalFailureException (failing_condition, aggregated_msg))

		| Bytes_Op(op, operands) ->
          FormatPlus.failwith "Dereference something not an address:@ operation @[%a@]" BytesPrinter.bytes bytes

		| Bytes_Read(bytes,off,len) ->
			conditional__map ~test:(Stp.query_guard state.path_condition)
				(deref state) (expand_read_to_conditional bytes off len)

		| Bytes_Write(bytes,off,len,newbytes) ->
          failwith "Dereference of Bytes_Write not implemented"

		| Bytes_FunPtr(_) ->
          failwith "Dereference of Bytes_FunPtr not implemented"

		| Bytes_Unbounded(_,_,_) ->
          failwith "Dereference of Bytes_Unbounded not implemented"

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
							let index = int_to_offset_bytes n in
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
							let base_typ = match Cil.unrollType lhost_typ with TArray(typ2, _, _) -> typ2 | _ -> failwith "Must be array" in
							let base_size = (Cil.bitsSizeOf base_typ) / 8 in (* must be known *)
							let index = Operator.mult [(int_to_offset_bytes base_size,!Cil.upointType);(rv,!Cil.upointType)] in
							(state, index, base_typ, offset2)
					| _ -> failwith "Unreachable"
				end
			in
				let (state, index2, base_typ2) = flatten_offset state base_typ offset2 in
				let index3 = Operator.plus [(index,!Cil.upointType);(index2,!Cil.upointType)] in
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
	let conditional = conditional__bytes (Operator.run (Operator.of_unop unop) [(rv,typ)]) in
	let conditional = conditional__prune ~test:(Stp.query_guard state.path_condition) ~eq:bytes__equal conditional in
	(state, make_Bytes_Conditional conditional)

and

rval_binop state binop exp1 exp2 =
	let op = (Operator.of_binop binop) in
	let state, rv1 = rval state exp1 in
	let typ1 = Cil.typeOf exp1 in
	(* shortcircuiting *)
	if op == Operator.logand && isConcrete_bytes rv1 && bytes_to_bool rv1 = false then
		(state, int_to_bytes 0)
	else if op == Operator.logor && isConcrete_bytes rv1 && bytes_to_bool rv1 = true then
		(state, int_to_bytes 1)
	else
		let state, rv2 = rval state exp2 in
		let typ2 = Cil.typeOf exp2 in
		let conditional = conditional__bytes (Operator.run op [(rv1,typ1);(rv2,typ2)]) in
		let conditional = conditional__prune ~test:(Stp.query_guard state.path_condition) ~eq:bytes__equal conditional in
		(state, make_Bytes_Conditional conditional)

and

(** Evaluate a ?: expression, creating a Bytes_Conditional(IfThenElse _). *)
rval_question	state e1 e2 e3 =
	(* The order in which we evaluate these really shouldn't matter.
	If it does, there's a problem, and I don't know what to do. *)
	let state, rv1 = rval state e1 in
	let state, rv2 = rval state e2 in
	let state, rv3 = rval state e3 in
	(state, make_Bytes_Conditional (IfThenElse (guard__bytes rv1, Unconditional rv2, Unconditional rv3)))
