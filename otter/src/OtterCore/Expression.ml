open DataStructures
open OcamlUtilities
open Cil
open OtterBytes
open Bytes
open BytesUtility
open State


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


(** [checkBounds job lvals useSize] checks whether a dereference is in bounds.
    @param job the job in which to perform the check
    @param lvals the conditional tree of lvalues being accessed
    @param useSize the width, in bytes, of the access
    @raises [Failure] if the dereference must fail
    @return [job', failing_bytes_opt]. [job'] is [job] augmented
    (if necessary) with the assumption that the check passed.
    [failing_bytes_opt] is [None] if the check certainly passes, or a [Some]
    with the bytes that failed the check. *)
let checkBounds job lvals useSize =
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
    match MemOp.eval job#state.path_condition both_checks with
      | Ternary.False -> FormatPlus.failwith "@[Bounds check failed:@ %a@]" BytesPrinter.bytes both_checks
      | Ternary.True -> (job, None) (* Check definitely passes. *)
      | Ternary.Unknown -> (* If the check can fail, add it to the path condition *)
            let job = MemOp.state__add_path_condition job both_checks true in
            (job, Some both_checks)

let add_offset offset lvals =
    conditional__map begin fun (block, offset2) ->
        conditional__lval_block (block, Operator.plus [(offset, !Cil.upointType); (offset2, !Cil.upointType)])
    end lvals


let rec

rval job exp errors =
    match exp with
        | Const (constant) ->
            begin match constant with
                | CStr(str) ->
                    let bytes = constant_to_bytes constant in
                    let block = MemOp.const_table__find bytes in
                    (job, make_Bytes_Address(block, bytes__zero), errors)
                | _ ->
                    (job, constant_to_bytes constant, errors)
            end

        | Lval (cil_lval) ->
            let job, lvals, errors = lval job cil_lval errors in
            let job, bytes = MemOp.state__deref job lvals in
            (job, bytes, errors)

        | SizeOf (typ) ->
             let exp2 = Cil.sizeOf typ in
             begin match exp2 with
                 | SizeOf(_) ->
                     FormatPlus.failwith "Cannot determine sizeof(%a)" Printcil.typ typ
                 | Const (CInt64 (n, IInt, stropt)) ->
                       (job, make_Bytes_Constant(CInt64(n, !kindOfSizeOf, stropt)), errors)
                 | _ -> failwith "Impossible case in rval"
             end

        | SizeOfE (exp2) ->
            rval job (SizeOf (Cil.typeOf exp2)) errors

        | SizeOfStr (str) ->
            let len = (String.length str)+1  in
            begin match Cil.integer len with
                | Const (CInt64 (n, IInt, stropt)) ->
                      (job, make_Bytes_Constant(CInt64(n, !kindOfSizeOf, stropt)), errors)
                | _ -> failwith "Impossible case in rval"
            end

        | AlignOf (typ) ->
            failwith "__align_of not implemented"
        | AlignOfE (exp2) ->
            failwith "__align_of not implemented"
        | UnOp (unop, exp1, _) ->
            rval_unop job unop exp1 errors
        | BinOp (binop, exp1, exp2, _) ->
            rval_binop job binop exp1 exp2 errors
        | Question (guard, exp1, exp2, _) ->
            rval_question job guard exp1 exp2 errors
        | AddrOf (Var varinfo, _) when Cil.isFunctionType (varinfo.Cil.vtype) ->
            (job, make_Bytes_FunPtr varinfo, errors)
        | AddrOf (cil_lval)
        | StartOf (cil_lval) ->
            let job, (lvals, _), errors = lval ~justGetAddr:true job cil_lval errors in
            let c = conditional__map (fun (block, offset) ->
                conditional__bytes (make_Bytes_Address(block, offset))
            ) lvals in
            (job, make_Bytes_Conditional c, errors)
        | CastE (typ, exp2) ->
            let job, bytes, errors = rval job exp2 errors in
            let bytes, errors = rval_cast typ bytes (Cil.typeOf exp2) errors in
            (job, bytes, errors)

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
                        then snd (bytes__read () rv (int_to_bytes 0) new_len)
                        else
                            (* TODO: should call STP's sign extension operation *)
                            snd (bytes__write () (bytes__make new_len) (int_to_bytes 0) old_len rv)

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
lval ?(justGetAddr=false) job (lhost, offset_exp as cil_lval) errors =
    let size = (Cil.bitsSizeOf (Cil.typeOfLval cil_lval))/8 in
    let job, lvals, errors = match lhost with
        | Var(varinfo) ->
            let job, lvals = MemOp.state__varinfo_to_lval_block job varinfo in
            (job, lvals, errors)
        | Mem(exp) ->
            let typ = Cil.typeOf exp in
            let job, rv, errors = rval job exp errors in
            let job, lvals, errors = deref job rv typ errors in
            (job, lvals, errors)
    in
    match cil_lval with
        | Var _, NoOffset -> (job, (lvals, size), errors)
        | _ ->
            let lhost_type = Cil.typeOfLval (lhost, NoOffset) in
            let job, offset, _, errors = flatten_offset job lhost_type offset_exp errors in
            (* Add the offset, then see if it was in bounds *)
            let lvals = add_offset offset lvals in
            (* Omit the bounds check if we're only getting the address of the
                 lval---not actually reading from or writing to it---or if bounds
                 checking is turned off *)
            if justGetAddr || not !Executeargs.arg_bounds_checking then
                (job, (lvals, size), errors)
            else
                let final_job, failing_bytes_opt = checkBounds job lvals size in
                let errors = match failing_bytes_opt with
                    | None -> errors
                    | Some b -> (job, `OutOfBounds (Lval cil_lval)) :: errors
                in
                (final_job, (lvals, size), errors)

and

deref job bytes typ errors =
    match bytes with
        | Bytes_Constant (c) ->
            FormatPlus.failwith "Dereference something not an address:@ constant @[%a@]" BytesPrinter.bytes bytes

        | Bytes_ByteArray(bytearray) ->
            (*
             * Special treatment: look for
             * "==(Bytearray(bytearray),make_Bytes_Address(b,f))" in PC.
             * If found, return deref job make_Bytes_Address(b,f).
             * Otherwise, throw exception
             * *)
            let rec find_match pc errors = match pc with
                | [] ->
                    FormatPlus.failwith "Dereference something not an address (bytearray)@ @[%a@]@." BytesPrinter.bytes bytes
                | (Bytes_Op(OP_EQ,[(bytes1,_); (bytes2,_)]), _)::pc' ->
                    begin
                        let bytes_tentative =
                            if bytes__equal bytes1 bytes then bytes2
                            else if bytes__equal bytes2 bytes then bytes1
                            else bytes__zero
                        in
                            match bytes_tentative with
                            | Bytes_Address(_,_) -> deref job bytes_tentative typ errors
                            | _ -> find_match pc' errors
                    end
                | (Bytes_Op(OP_LAND,btlist), _)::pc' ->
                    find_match (List.rev_append (List.fold_left (fun a (b,_) -> (b,true)::a) [] btlist) pc') errors
                | _::pc' -> find_match pc' errors
            in
            find_match job#state.path_condition errors

        | Bytes_Address(block, offset) ->
            if MemOp.state__has_block job block then
                (job, conditional__lval_block (block, offset), errors)
            else
                failwith "Dereference a dangling pointer"

        | Bytes_Conditional c ->
            let (guard, job, errors, _), conditional_opt =
                conditional__fold_map_opt
                    ~test:begin fun (guard', job, errors, removed) pre guard ->
                        let job, truth =
                            (job : #Info.t)#profile_call "query_stp/Expression.deref/Bytes_Conditional"
                                (fun job -> (job, Stp.query_stp job#state.path_condition pre guard))
                        in
                        ((guard', job, errors, removed), truth)
                    end
                    begin fun (guard, job, errors, removed) _ c ->
                        if List.exists (Bytes.bytes__equal c) removed then
                            ((guard, job, errors, removed), None)
                        else
                            try
                                let job, lval, errors = deref job c typ errors in
                                ((guard, job, errors, removed), Some lval)
                            with Failure msg ->
                                (* Guard against this failure, add it to the error list, and remove this leaf. *)
                                let failing_bytes = Operator.eq [ (bytes, typ); (c, typ) ] in
                                let guard = guard__and_not guard (Bytes.guard__bytes failing_bytes) in
                                let errors = (job, `Failure msg)::errors in
                                let removed = c::removed in
                                ((guard, job, errors, removed), None)
                    end (Bytes.guard__true, job, errors, []) c
            in
            begin match conditional_opt, guard with
                | Some conditional, Guard_True ->
                    (* All conditional branches were dereferenced successfully: return the dereferenced conditional. *)
                    (job, conditional, errors)
                | Some conditional, guard ->
                    (* Not all conditional branches were dereferenced successfully: add the guard and continue. *)
                    let job = MemOp.state__add_path_condition job (Bytes.guard__to_bytes guard) true in
                    (job, conditional, errors)
                | None, _ ->
                    (* No conditional branches were dereferenced successfully: just fail. *)
                    (* TODO: Yit: The only way to return a polymorphic error in Ocaml is to use an exception monad,
                        and that takes a wholesale change to Expression. *)
                    failwith "Dereference of invalid conditional pointer"
            end

        | Bytes_Op(op, operands) ->
            FormatPlus.failwith "Dereference something not an address:@ operation @[%a@]" BytesPrinter.bytes bytes

        | Bytes_Read(bytes,off,len) ->
            deref job (make_Bytes_Conditional (expand_read_to_conditional bytes off len)) typ errors

        | Bytes_Write(bytes,off,len,newbytes) ->
            failwith "Dereference of Bytes_Write not implemented"

        | Bytes_FunPtr(_) ->
            failwith "Dereference of Bytes_FunPtr not implemented"

and

(* Assume index's ikind is IInt *)
flatten_offset job lhost_typ offset errors =
    match offset with
        | NoOffset -> (job, bytes__zero, lhost_typ, errors)
        | _ ->
            let job, index, base_typ, offset2, errors =
                begin match offset with
                    | Field(fieldinfo, offset2) ->
                            let n = field_offset fieldinfo in
                            let index = int_to_offset_bytes n in
                            let base_typ = fieldinfo.ftype in
                            (job, index, base_typ, offset2, errors)
                    | Index(exp, offset2) ->
                            let job, rv0, errors = rval job exp errors in
                            (* We require that offsets be values of type !upointType *)
                            let rv, errors =
                                if bytes__length rv0 <> (Cil.bitsSizeOf !Cil.upointType / 8)
                                then rval_cast !Cil.upointType rv0 (Cil.typeOf exp) errors
                                else (rv0, errors)
                            in
                            let base_typ = match Cil.unrollType lhost_typ with TArray(typ2, _, _) -> typ2 | _ -> failwith "Must be array" in
                            let base_size = (Cil.bitsSizeOf base_typ) / 8 in (* must be known *)
                            let index = Operator.mult [(int_to_offset_bytes base_size,!Cil.upointType);(rv,!Cil.upointType)] in
                            (job, index, base_typ, offset2, errors)
                    | _ -> failwith "Unreachable"
                end
            in
                let job, index2, base_typ2, errors = flatten_offset job base_typ offset2 errors in
                let index3 = Operator.plus [(index,!Cil.upointType);(index2,!Cil.upointType)] in
                (job, index3, base_typ2, errors)

and

(* Calculate field offsets according to Cil, which should mimic the underlying compiler (e.g., gcc).
   The C specification leave alignment implementation-defined [6.7.2.1.12]. *)
field_offset f : int =
    let offset, _ = bitsOffset (TComp (f.fcomp, [])) (Field (f, NoOffset)) in
        offset / 8

and

rval_unop job unop exp errors =
    let job, rv, errors = rval job exp errors in
    let typ = Cil.typeOf exp in
    let bytes = (Operator.of_unop unop) [(rv,typ)] in
    (job, bytes, errors)

and

rval_binop job binop exp1 exp2 errors =
    let job, rv1, errors = rval job exp1 errors in
    match binop with
      | LAnd | LOr -> begin (* Short-circuit, if possible *)
            match MemOp.eval job#state.path_condition rv1 with
              | Ternary.True -> if binop == LAnd then rval job exp2 errors else (job, bytes__one, errors)
              | Ternary.False -> if binop == LOr then rval job exp2 errors else (job, bytes__zero, errors)
              | Ternary.Unknown ->
                    let no_short_circuit = if binop == LAnd then rv1 else logicalNot rv1 in (* Assume no short-circuit *)
                    match evaluate_under_condition job no_short_circuit exp2 with
                      | None, msg -> (* exp2 fails. Report this error, and assume short-circuiting occurs *)
                            let errors = (job, `Failure msg) :: errors in
                            let job = MemOp.state__add_path_condition job (logicalNot no_short_circuit) true in
                            let result = if binop == LAnd then bytes__zero else bytes__one in
                            (job, result, errors)
                      | Some (job, rv2), _ -> (* exp2 does not fail. Construct the 'and' or 'or' value. *)
                            let bytes = (Operator.of_binop binop) [ (rv1, typeOf exp1); (rv2, typeOf exp2) ] in
                            (job, bytes, errors)
        end
      | _ ->
            let job, rv2, errors = rval job exp2 errors in
            let bytes = (Operator.of_binop binop) [ (rv1, typeOf exp1); (rv2, typeOf exp2) ] in
            (job, bytes, errors)

and

(** Evaluate a ?: expression, creating a Bytes_Conditional(IfThenElse _). *)
rval_question job exp1 exp2 exp3 errors =
    let job, rv1, errors = rval job exp1 errors in
    (* Is the guard true, false, or unknown? Evaluate the branches accordingly. *)
    match MemOp.eval job#state.path_condition rv1 with
      | Ternary.True -> rval job exp2 errors
      | Ternary.False -> rval job exp3 errors
      | Ternary.Unknown ->
            let not_rv1 = logicalNot rv1 in
            (* Evaluate exp2, assuming rv1 *)
            match evaluate_under_condition job rv1 exp2 with
              | None, msg -> (* exp2 fails. Report this error, assume not_rv1, and evaluate to exp3 *)
                    let errors = (job, `Failure msg) :: errors in
                    let job = MemOp.state__add_path_condition job not_rv1 true in
                    rval job exp3 errors
              | Some (job, rv2), _ -> (* exp2 does not fail. Evaluate exp3, assuming not_rv1 *)
                    match evaluate_under_condition job not_rv1 exp3 with
                      | None, msg -> (* exp3 fails. Report this error, assume rv1, and evaluate to rv2 *)
                            let errors = (job, `Failure msg) :: errors in
                            let job = MemOp.state__add_path_condition job rv1 true in
                            (job, rv2, errors)
                      | Some (job, rv3), _ -> (* exp3 does not fail. Evaluate to an IfThenElse. *)
                            let bytes = make_Bytes_Conditional (IfThenElse (guard__bytes rv1, conditional__bytes rv2, conditional__bytes rv3)) in
                            (job, bytes, errors)

and

(** [evaluate_under_condition job condition exp] evaluates [exp] in [job]
    augmented with the assumption that [condition] is true. This can be useful if,
    for example, [condition] ensures that a pointer dereferenced in [exp] is
    non-null, or that a dereference is in bounds.
    @param job the job in which evaluate exp
    @param condition the assumption to add to job
    @param exp the expression to evaluate
    @raise Failure if evaluating [exp] can fail but does not have to
    @return [(None, msg)] if evaluating [exp] raises [Failure msg]; otherwise,
    returns [(Some (job', bytes), "")], where [job'] is a job identical to
    [job] except that some lazy memory initialization may have taken place
    while evaluating [exp], and [bytes] is the value obtained by evaluating
    [exp].
*)
evaluate_under_condition job_in condition exp =
    let job = MemOp.state__add_path_condition job_in condition true in
    let checkpointed_path_condition = job#state.path_condition in
    let result = try (Some (rval job exp []), "") with Failure msg -> (None, msg) in
    match result with
      | None, msg -> None, msg
      | Some (job, bytes, errors), _ ->
            (* TODO: it should be possible to handle errors when evaluating under
               a condition by adding an assumption to the path condition saying
               that condition implies that no error occurs. However, this is
               complicated by lazy memory initialization: in what job are the
               error bytes valid? They *should* all be valid in the final job,
               but it's not entirely clear. *)
            if errors <> [] then failwith "Cannot handle partial errors when evaluating under a condition";
            (* Roll back the assumption that condition holds. Because of lazy
               memory initialization, using the old job is dangerous, because
               new bytes may have been created while evaluating exp. However,
               just reverting the path condition is fine. Just as an extra
               check, though, we make sure the path condition didn't change
               during the evaluation of exp. *)
            if job#state.path_condition != checkpointed_path_condition
            then failwith "Path condition changed unexpectedly while evaluating under a condition";
            (Some (job#with_state { job#state with
                                        path_condition = job_in#state.path_condition;
                                  }, bytes), "")


(** [evaluate_initializer job typ init] evaluates an initializer in a given job.
        @param job is the job in which evaluate init
        @param typ is the type of the initializer
        @param init is the initializer to evaluate
        @return [(job', bytes)] is the updated job and the result of evaluating [init].
*)
let evaluate_initializer job typ init =
    let size = Cil.bitsSizeOf typ / 8 in
    let size = if size <= 0 then 1 else size in

    let bytes = Bytes.bytes__make size in

    let rec evaluate_initializer ciloffset init (job, bytes) = match init with
        | Cil.SingleInit expr ->
            let job, offset, offset_type, errors  = flatten_offset job typ ciloffset [] in
            let job, offset_bytes, errors = rval job expr errors in
            assert(errors = []); (* there shouldn't be any errors during initialization *)

            let offset_size = Cil.bitsSizeOf offset_type / 8 in
            let offset_size = if offset_size <= 0 then 1 else offset_size in
            let (), init_bytes = BytesUtility.bytes__write () bytes offset offset_size offset_bytes in
            (job, init_bytes)
        | Cil.CompoundInit (ct, initl) ->
            Cil.foldLeftCompound
                ~implicit:false ~ct ~initl
                ~doinit:(fun ciloffset' init _ (job, bytes) -> evaluate_initializer (Cil.addOffset ciloffset' ciloffset) init (job, bytes))
                ~acc:(job, bytes)
    in
    evaluate_initializer Cil.NoOffset init (job, bytes)
