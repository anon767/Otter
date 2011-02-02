open DataStructures
open OcamlUtilities
open Cil
open OtterBytes
open Bytes
open BytesUtility
open Types

(* Track Stp calls *)
let timed_query_stp name pc pre guard = Timer.time name (fun () -> Stp.query_stp pc pre guard) ()

let prune_bytes state bytes debug_text =
    let conditional = conditional__bytes bytes in
    let conditional = conditional__prune ~test:(timed_query_stp debug_text state.path_condition) ~eq:bytes__equal conditional in
    make_Bytes_Conditional conditional

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
     which {lvals} is about to be read or written. But this isn't
     enough because of possible overflow (e.g., if {offsets} contains
     the value -1 == 0xffffffff). To catch overflow, you need to
     additionally check that {offsets < offsets + useSize}.

     Interestingly, a different but seemingly equivalent pair of checks,
     {offsets < sizes} and {offsets + useSize <= sizes}, actually does
     not work properly, because it does not catch the possible overflow
     of the addition. This pair of checks would fail to catch an error
     in a case where {offsets + useSize < offsets < sizes}. This can only
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
     Second, {useSize} is concrete, and each leaf of {sizes} is concrete;
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


(** [checkBounds state lvals useSize] checks whether a dereference is in bounds.
    @param state the state in which to perform the check
    @param lvals the conditional tree of lvalues being accessed
    @param useSize the width, in bytes, of the access
    @raises [Failure] if the dereference must fail
    @return [state', failing_bytes_opt]. [state'] is [state] augmented
    (if necessary) with the assumption that the check passed.
    [failing_bytes_opt] is [None] if the check certainly passes, or a [Some]
    with the bytes that failed the check. *)
let checkBounds state lvals useSize =
    (* Get the block sizes and offsets *)
    let sizesTree, offsetsTree = getBlockSizesAndOffsets lvals in

    (* Make the relevant Bytes *)
    let offsetsBytes = make_Bytes_Conditional offsetsTree
    and sizesBytes = make_Bytes_Conditional sizesTree
    and useSizeBytes = int_to_offset_bytes useSize in

    (* Prepare the first bounds check: {offsets <= sizes - useSize} *)
    let sizesMinusUseSize = Operator.minus [(sizesBytes, !Cil.upointType); (useSizeBytes, !Cil.upointType)] in
    let offsetsLeSizesMinusUseSize = Operator.le [(offsetsBytes, !Cil.upointType); (sizesMinusUseSize, !Cil.upointType)] in

    (* Prepare the second bounds check: {useSize <= sizes} *)
    (* TODO: If {sizes} is a conditional tree (rather than a single value), we
       may want to call conditional__map (with the identity function) to
       simplify {useSizeLeSizes}. It will almost always have concrete 'true's at
       every leaf. If we don't simplify, we'll end up calling the solver. *)
    let useSizeLeSizes = Operator.le [(useSizeBytes, !Cil.upointType); (sizesBytes, !Cil.upointType)] in

    let both_checks = Operator.bytes__land offsetsLeSizesMinusUseSize useSizeLeSizes in

    (* Perform the check *)
    match MemOp.eval state.path_condition both_checks with
      | Ternary.False -> FormatPlus.failwith "Bounds check failed:\n%a" BytesPrinter.bytes both_checks
      | Ternary.True -> (state, None) (* Check definitely passes. *)
      | Ternary.Unknown -> (* If the check can fail, add it to the path condition *)
            let state = MemOp.state__add_path_condition state both_checks true in
            (state, Some both_checks)

let add_offset offset lvals =
    conditional__map begin fun (block, offset2) ->
        conditional__lval_block (block, Operator.plus [(offset, !Cil.upointType); (offset2, !Cil.upointType)])
    end lvals


let rec

rval state exp errors =
    match exp with
        | Const (constant) ->
            begin match constant with
                | CStr(str) ->
                    let bytes = constant_to_bytes constant in
                    let block = MemOp.string_table__add bytes in
                    (state, make_Bytes_Address(block, bytes__zero), errors)
                | _ ->
                    (state, constant_to_bytes constant, errors)
            end

        | Lval (cil_lval) ->
            let state, lvals, errors = lval state cil_lval errors in
            let state, bytes = MemOp.state__deref state lvals in
            (state, bytes, errors)

        | SizeOf (typ) ->
             let exp2 = Cil.sizeOf typ in
             begin match exp2 with
                 | SizeOf(_) ->
                     FormatPlus.failwith "Cannot determine sizeof(%a)" Printcil.typ typ
                 | Const (CInt64 (n, IInt, stropt)) ->
                       (state, make_Bytes_Constant(CInt64(n, !kindOfSizeOf, stropt)), errors)
                 | _ -> failwith "Impossible case in rval"
             end

        | SizeOfE (exp2) ->
            rval state (SizeOf (Cil.typeOf exp2)) errors

        | SizeOfStr (str) ->
            let len = (String.length str)+1  in
            begin match Cil.integer len with
                | Const (CInt64 (n, IInt, stropt)) ->
                      (state, make_Bytes_Constant(CInt64(n, !kindOfSizeOf, stropt)), errors)
                | _ -> failwith "Impossible case in rval"
            end

        | AlignOf (typ) ->
            failwith "__align_of not implemented"
        | AlignOfE (exp2) ->
            failwith "__align_of not implemented"
        | UnOp (unop, exp1, _) ->
            rval_unop state unop exp1 errors
        | BinOp (binop, exp1, exp2, _) ->
            rval_binop state binop exp1 exp2 errors
        | Question (guard, exp1, exp2, _) ->
            rval_question state guard exp1 exp2 errors
        | AddrOf (Var varinfo, _) when Cil.isFunctionType (varinfo.Cil.vtype) ->
            let f_addr = bytes__random (Cil.bitsSizeOf Cil.voidPtrType / 8) in (* TODO: assign an addr for each function ptr *)
            (state, make_Bytes_FunPtr(varinfo,f_addr), errors)
        | AddrOf (cil_lval)
        | StartOf (cil_lval) ->
            let state, (lvals, _), errors = lval ~justGetAddr:true state cil_lval errors in
            let c = conditional__map (fun (block, offset) ->
                conditional__bytes (make_Bytes_Address(block, offset))
            ) lvals in
            (state, make_Bytes_Conditional c, errors)
        | CastE (typ, exp2) ->
            let state, bytes, errors = rval state exp2 errors in
            let bytes, errors = rval_cast typ bytes (Cil.typeOf exp2) errors in
            (state, bytes, errors)

and

(** rvtyp: the type of rv. if rv is a signed int then we will need logical shifting; no otherwise. *)
rval_cast typ rv rvtyp errors =
    begin match rv,typ with
        (* optimize for casting among int family *)
        | Bytes_Constant(CInt64(n,ikind,_)),TInt(new_ikind,_) ->
            begin match Cil.kinteger64 new_ikind n with
                | Const (const) -> (make_Bytes_Constant(const), errors)
                | _ -> failwith "rval_cast, const: unreachable"
            end
        (* optimize for casting among float family *)
        | Bytes_Constant(CReal(f,fkind,s)),TFloat(new_fkind,_) ->
            let const = CReal(f,new_fkind,s) in
            (make_Bytes_Constant(const), errors)
        (* Casting to _Bool is essentially '!= 0'. See ISO C99 6.3.1.2, except
           that the result, obviously, has type _Bool and not int. I think (or
           hope) that doesn't matter for Otter, though. *)
        | rv, TInt(IBool,_) ->
            let zero = bytes__make ((bitsSizeOf rvtyp) / 8) in
            (Operator.ne [ (rv, rvtyp); (zero, rvtyp) ], errors)
        (* added so that from now on there'll be no make_Bytes_Constant *)
        | Bytes_Constant(const),_ ->
            rval_cast typ (constant_to_bytes const) rvtyp errors
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
                if new_len = old_len then
                    (rv, errors) (* do nothing *)
                else begin match rv with
                    | Bytes_ByteArray(bytearray) when isConcrete_bytearray bytearray ->
                        if new_len > old_len then
                            let newbytes = (ImmutableArray.sub bytearray 0 new_len) in
                            let isSigned = match rvtyp with
                                | TInt(ikind,_) when Cil.isSigned ikind -> true
                                | _ -> false
                            in
                            let leftmost_is_one =
                                match ImmutableArray.get bytearray ((ImmutableArray.length bytearray)-1) with
                                | Byte_Concrete (c) -> Char.code c >= 0x80
                                | _ -> failwith "unreachable (bytearray is concrete)"
                            in
                            let sth = if isSigned && leftmost_is_one then
                                byte__111 (* For some reason, this seems not to happen in practice *)
                            else
                                byte__zero
                            in
                            let rec pack_sth newbytes old_len new_len =
                                if old_len>=new_len then newbytes else
                                let newbytes2 = ImmutableArray.set newbytes old_len sth in
                                pack_sth newbytes2 (old_len+1) new_len
                            in
                            (make_Bytes_ByteArray (pack_sth newbytes old_len new_len), errors)
                        else (* new_len < old_len *)
                            (make_Bytes_ByteArray (ImmutableArray.sub bytearray 0 new_len), errors) (* simply truncate *)

                    | Bytes_Constant(const) ->
                        failwith "unreachable"

                    | _ ->
                        (worst_case (), errors)
                end
            end
    end

and

     (* justGetAddr is set to true when the lval is the operand of the
     address-of operator, '&'. For example, &x[i] is legal even if i is
     not in bounds. (Sort of. The spec (6.5.6.8) implies that this is
     only defined if i is *one* past the end of x.) *)
lval ?(justGetAddr=false) state (lhost, offset_exp as cil_lval) errors =
    let size = (Cil.bitsSizeOf (Cil.typeOfLval cil_lval))/8 in
    let state, lvals, errors = match lhost with
        | Var(varinfo) ->
            let state, lvals = MemOp.state__varinfo_to_lval_block state varinfo in
            (state, lvals, errors)
        | Mem(exp) ->
            let typ = Cil.typeOf exp in
            let state, rv, errors = rval state exp errors in
            let state, lvals, errors = deref state rv typ errors in
            (state, lvals, errors)
    in
    match cil_lval with
        | Var _, NoOffset -> (state, (lvals, size), errors)
        | _ ->
            let lhost_type = Cil.typeOfLval (lhost, NoOffset) in
            let state, offset, _, errors = flatten_offset state lhost_type offset_exp errors in
            (* Add the offset, then see if it was in bounds *)
            let lvals = add_offset offset lvals in
            (* Omit the bounds check if we're only getting the address of the
                 lval---not actually reading from or writing to it---or if bounds
                 checking is turned off *)
            if justGetAddr || not !Executeargs.arg_bounds_checking then
                (state, (lvals, size), errors)
            else
                let final_state, failing_bytes_opt = checkBounds state lvals size in
                let errors = match failing_bytes_opt with
                    | None -> errors
                    | Some b -> (state, logicalNot b, `OutOfBounds (Lval cil_lval)) :: errors
                in
                (final_state, (lvals, size), errors)

and

deref state bytes typ errors =
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
            let rec find_match pc errors = match pc with
                | [] ->
                    FormatPlus.failwith "Dereference something not an address (bytearray)@ @[%a@]@." BytesPrinter.bytes bytes
                | Bytes_Op(OP_EQ,(bytes1,_)::(bytes2,_)::[])::pc' ->
                    begin
                        let bytes_tentative =
                            if bytes1=bytes then bytes2
                            else if bytes2=bytes then bytes1
                            else bytes__zero
                        in
                            match bytes_tentative with
                            | Bytes_Address(_,_) -> deref state bytes_tentative typ errors
                            | _ -> find_match pc' errors
                    end
                | Bytes_Op(OP_LAND,btlist)::pc' ->
                    find_match (List.rev_append (List.fold_left (fun a (b,_) -> b::a) [] btlist) pc') errors
                | _::pc' -> find_match pc' errors
            in
            find_match state.path_condition errors

        | Bytes_Address(block, offset) ->
            if MemOp.state__has_block state block then
                (state, conditional__lval_block (block, offset), errors)
            else
                failwith "Dereference a dangling pointer"

        | Bytes_Conditional c ->
            let (guard, state, errors, _), conditional_opt =
                conditional__fold_map_opt
                    ~test:(timed_query_stp "query_stp/Expression.deref/Bytes_Conditional" state.path_condition)
                    begin fun (guard, state, errors, removed) _ c ->
                        if List.exists (Bytes.bytes__equal c) removed then
                            ((guard, state, errors, removed), None)
                        else
                            try
                                let state, lval, errors = deref state c typ errors in
                                ((guard, state, errors, removed), Some lval)
                            with Failure msg ->
                                (* Guard against this failure, add it to the error list, and remove this leaf. *)
                                let failing_bytes = Operator.eq [ (bytes, typ); (c, typ) ] in
                                let guard = guard__and_not guard (Bytes.guard__bytes failing_bytes) in
                                let errors = (state, failing_bytes, `Failure msg)::errors in
                                let removed = c::removed in
                                ((guard, state, errors, removed), None)
                    end (Bytes.guard__true, state, errors, []) c
            in
            begin match conditional_opt, guard with
                | Some conditional, Guard_True ->
                    (* All conditional branches were dereferenced successfully: return the dereferenced conditional. *)
                    (state, conditional, errors)
                | Some conditional, guard ->
                    (* Not all conditional branches were dereferenced successfully: add the guard and continue. *)
                    let state = { state with
                        path_condition = (Bytes.guard__to_bytes guard)::state.path_condition;
                    } in
                    (state, conditional, errors)
                | None, _ ->
                    (* No conditional branches were dereferenced successfully: just fail. *)
                    (* TODO: Yit: The only way to return a polymorphic error in Ocaml is to use an exception monad,
                        and that takes a wholesale change to Expression. *)
                    failwith "Dereferece of invalid conditional pointer"
            end

        | Bytes_Op(op, operands) ->
            FormatPlus.failwith "Dereference something not an address:@ operation @[%a@]" BytesPrinter.bytes bytes

        | Bytes_Read(bytes,off,len) ->
            (* TODO (martin): create a test that covers this code *)
            let (state, errors), conditional =
                conditional__fold_map ~test:(timed_query_stp "query_stp/Expression.deref/Bytes_Read" state.path_condition)
                    begin fun (state, errors) _ bytes ->
                        let state, conditional, errors = deref state bytes typ errors in
                        ((state, errors), conditional)
                    end
                    (state, errors) (expand_read_to_conditional bytes off len)
            in
            (state, conditional, errors)

        | Bytes_Write(bytes,off,len,newbytes) ->
            failwith "Dereference of Bytes_Write not implemented"

        | Bytes_FunPtr(_) ->
            failwith "Dereference of Bytes_FunPtr not implemented"

and

(* Assume index's ikind is IInt *)
flatten_offset state lhost_typ offset errors =
    match offset with
        | NoOffset -> (state, bytes__zero, lhost_typ, errors)
        | _ ->
            let state, index, base_typ, offset2, errors =
                begin match offset with
                    | Field(fieldinfo, offset2) ->
                            let n = field_offset fieldinfo in
                            let index = int_to_offset_bytes n in
                            let base_typ = fieldinfo.ftype in
                            (state, index, base_typ, offset2, errors)
                    | Index(exp, offset2) ->
                            let state, rv0, errors = rval state exp errors in
                            (* We require that offsets be values of type !upointType *)
                            let rv, errors =
                                if bytes__length rv0 <> (Cil.bitsSizeOf !Cil.upointType / 8)
                                then rval_cast !Cil.upointType rv0 (Cil.typeOf exp) errors
                                else (rv0, errors)
                            in
                            let base_typ = match Cil.unrollType lhost_typ with TArray(typ2, _, _) -> typ2 | _ -> failwith "Must be array" in
                            let base_size = (Cil.bitsSizeOf base_typ) / 8 in (* must be known *)
                            let index = Operator.mult [(int_to_offset_bytes base_size,!Cil.upointType);(rv,!Cil.upointType)] in
                            (state, index, base_typ, offset2, errors)
                    | _ -> failwith "Unreachable"
                end
            in
                let state, index2, base_typ2, errors = flatten_offset state base_typ offset2 errors in
                let index3 = Operator.plus [(index,!Cil.upointType);(index2,!Cil.upointType)] in
                (state, index3, base_typ2, errors)

and

(* Calculate field offsets according to Cil, which should mimic the underlying compiler (e.g., gcc).
   The C specification leave alignment implementation-defined [6.7.2.1.12]. *)
field_offset f : int =
    let offset, _ = bitsOffset (TComp (f.fcomp, [])) (Field (f, NoOffset)) in
        offset / 8

and

rval_unop state unop exp errors =
    let state, rv, errors = rval state exp errors in
    let typ = Cil.typeOf exp in
    let bytes = (Operator.of_unop unop) [(rv,typ)] in
    (state, prune_bytes state bytes "query_stp/Expression.rval_unop", errors)

and

rval_binop state binop exp1 exp2 errors =
    let state, rv1, errors = rval state exp1 errors in
    match binop with
      | LAnd | LOr -> begin (* Short-circuit, if possible *)
            match MemOp.eval state.path_condition rv1 with
              | Ternary.True -> if binop == LAnd then rval state exp2 errors else (state, bytes__one, errors)
              | Ternary.False -> if binop == LOr then rval state exp2 errors else (state, bytes__zero, errors)
              | Ternary.Unknown ->
                    let no_short_circuit = if binop == LAnd then rv1 else logicalNot rv1 in (* Assume no short-circuit *)
                    match evaluate_under_condition state no_short_circuit exp2 with
                      | None, msg -> (* exp2 fails. Report this error, and assume short-circuiting occurs *)
                            let errors = (state, no_short_circuit, `Failure msg) :: errors in
                            let state = MemOp.state__add_path_condition state (logicalNot no_short_circuit) true in
                            let result = if binop == LAnd then bytes__zero else bytes__one in
                            (state, result, errors)
                      | Some (state, rv2), _ -> (* exp2 does not fail. Construct the 'and' or 'or' value. *)
                            let bytes = (Operator.of_binop binop) [ (rv1, typeOf exp1); (rv2, typeOf exp2) ] in
                            (state, prune_bytes state bytes "query_stp/Expression.rval_binop", errors)
        end
      | _ ->
            let state, rv2, errors = rval state exp2 errors in
            let bytes = (Operator.of_binop binop) [ (rv1, typeOf exp1); (rv2, typeOf exp2) ] in
            (state, prune_bytes state bytes "query_stp/Expression.rval_binop", errors)

and

(** Evaluate a ?: expression, creating a Bytes_Conditional(IfThenElse _). *)
rval_question state exp1 exp2 exp3 errors =
    let state, rv1, errors = rval state exp1 errors in
    (* Is the guard true, false, or unknown? Evaluate the branches accordingly. *)
    match MemOp.eval state.path_condition rv1 with
      | Ternary.True -> rval state exp2 errors
      | Ternary.False -> rval state exp3 errors
      | Ternary.Unknown ->
            let not_rv1 = logicalNot rv1 in
            (* Evaluate exp2, assuming rv1 *)
            match evaluate_under_condition state rv1 exp2 with
              | None, msg -> (* exp2 fails. Report this error, assume not_rv1, and evaluate to exp3 *)
                    let errors = (state, rv1, `Failure msg) :: errors in
                    let state = MemOp.state__add_path_condition state not_rv1 true in
                    rval state exp3 errors
              | Some (state, rv2), _ -> (* exp2 does not fail. Evaluate exp3, assuming not_rv1 *)
                    match evaluate_under_condition state not_rv1 exp3 with
                      | None, msg -> (* exp3 fails. Report this error, assume rv1, and evaluate to rv2 *)
                            let errors = (state, not_rv1, `Failure msg) :: errors in
                            let state = MemOp.state__add_path_condition state rv1 true in
                            (state, rv2, errors)
                      | Some (state, rv3), _ -> (* exp3 does not fail. Evaluate to an IfThenElse. *)
                            let bytes = make_Bytes_Conditional (IfThenElse (guard__bytes rv1, conditional__bytes rv2, conditional__bytes rv3)) in
                            (state, prune_bytes state bytes "query_stp/Expression.rval_question", errors)

and

(** [evaluate_under_condition state condition exp] evaluates [exp] in [state]
    augmented with the assumption that [condition] is true. This can be useful if,
    for example, [condition] ensures that a pointer dereferenced in [exp] is
    non-null, or that a dereference is in bounds.
    @param state the state in which evaluate exp
    @param condition the assumption to add to state
    @param exp the expression to evaluate
    @raise Failure if evaluating [exp] can fail but does not have to
    @return [(None, msg)] if evaluating [exp] raises [Failure msg]; otherwise,
    returns [(Some (state', bytes), "")], where [state'] is a state identical to
    [state] except that some lazy memory initialization may have taken place
    while evaluating [exp], and [bytes] is the value obtained by evaluating
    [exp].
*)
evaluate_under_condition state_in condition exp =
    let state = MemOp.state__add_path_condition state_in condition true in
    let checkpointed_path_condition = state.path_condition in
    let result = try (Some (rval state exp []), "") with Failure msg -> (None, msg) in
    match result with
      | None, msg -> None, msg
      | Some (state, bytes, errors), _ ->
            (* TODO: it should be possible to handle errors when evaluating under
               a condition by adding an assumption to the path condition saying
               that condition implies that no error occurs. However, this is
               complicated by lazy memory initialization: in what state are the
               error bytes valid? They *should* all be valid in the final state,
               but it's not entirely clear. *)
            if errors <> [] then failwith "Cannot handle partial errors when evaluating under a condition";
            (* Roll back the assumption that condition holds. Because of lazy
               memory initialization, using the old state is dangerous, because
               new bytes may have been created while evaluating exp. However,
               just reverting the path condition is fine. Just as an extra
               check, though, we make sure the path condition didn't change
               during the evaluation of exp. *)
            if state.path_condition != checkpointed_path_condition
            then failwith "Path condition changed unexpectedly while evaluating under a condition";
            (Some ({ state with path_condition = state_in.path_condition; }, bytes), "")
