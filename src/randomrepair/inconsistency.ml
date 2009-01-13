open Types



(* convert a list of facts to a conjunction of facts *)
let bvlist_to_bv = function
    | hd::tl -> List.fold_left (fun a x -> Bytes_Op(OP_LAND, [ (a, Cil.intType); (x, Cil.intType) ])) hd tl
    | [] -> Bytes_Constant(Cil.CInt64(Int64.one, Cil.IInt, None)) (* true *)



(* convert facts to negation-normal-form *)
let rec bv_to_nnf bytes = match bytes with
  (* recurse *)
  | Bytes_Op((OP_LOR|OP_LAND) as op, [ (left_op, left_type); (right_op, right_type) ]) ->
      Bytes_Op(op, [ (bv_to_nnf left_op, left_type); (bv_to_nnf right_op, right_type) ])

  | Bytes_Op(OP_LNOT, [ (not_op, not_op_type) ]) ->
      begin match not_op with
        (* cancel not-not *)
        | Bytes_Op(OP_LNOT, [ (not_not_op, _) ] ) ->
            bv_to_nnf not_not_op

        (* distribute not through and/or using De Morgan's law *)
        | Bytes_Op(OP_LOR, [ (_, left_type) as left_ops; (_, right_type) as right_ops ]) ->
            let left_ops = bv_to_nnf (Bytes_Op(OP_LNOT, [ left_ops ])) in
            let right_ops = bv_to_nnf (Bytes_Op(OP_LNOT, [ right_ops ])) in
            Bytes_Op(OP_LAND, [ (left_ops, left_type); (right_ops, right_type) ])

        | Bytes_Op(OP_LAND, [ (_, left_type) as left_ops; (_, right_type) as right_ops ]) ->
            let left_ops = bv_to_nnf (Bytes_Op(OP_LNOT, [ left_ops ])) in
            let right_ops = bv_to_nnf (Bytes_Op(OP_LNOT, [ right_ops ])) in
            Bytes_Op(OP_LOR, [ (left_ops, left_type); (right_ops, right_type) ])

        (* leave everything else; hopefully, not/and/or doesn't appear in arithmetic ops *)
        | _ -> bytes
      end
  | _ -> bytes



(* convert facts to disjunctive-/conjunctive-normal form *)
let (bv_to_cnf, bv_to_dnf) =
    (* convert to "top-over"-normal-form *)
    let rec bv_to_xnf (top, over) bytes =
        let bv_to_xnf = bv_to_xnf (top, over) in
        let rec recurse = function
            (* recurse into "top" *)
            | Bytes_Op(op, [ (left_ops, left_type); (right_ops, right_type) ]) when op = top ->
                Bytes_Op(op, [ (bv_to_xnf left_ops, left_type); (bv_to_xnf right_ops, right_type) ])

            (* push "over" into "top" *)
            | Bytes_Op(op, [ (left_ops, left_type); (right_ops, right_type) ]) when op = over ->
                distribute ((bv_to_xnf left_ops, left_type), (bv_to_xnf right_ops, right_type))

            (* leave everything else *)
            | nnf -> nnf
        and distribute = function
            (* distribute "over" over "top", and recurse *)
            | (Bytes_Op(op, [ left_ops; right_ops ]), x_type), y when op = top ->
                Bytes_Op(op, [ distribute (left_ops, y), x_type; distribute (right_ops, y), x_type ])
            | x, (Bytes_Op(op, [ left_ops; right_ops ]), y_type) when op = top ->
                Bytes_Op(op, [ distribute (x, left_ops), y_type; distribute (x, right_ops), y_type ])

            (* no more "top" to distribute over *)
            | x, y ->
                Bytes_Op(over, [ x; y ])
        in
        recurse (bv_to_nnf bytes)
    in
    (bv_to_xnf (OP_LAND, OP_LOR), bv_to_xnf (OP_LOR, OP_LAND))



(* calculate Gulwani's inconsistency measure for the implication ant => con *)
let gulwani ant con =
    (* flatten "top" of bv into a list *)
    let bv_to_xlist top bv =
        let rec collect = function
            | Bytes_Op(op, [ (left_ops, _); (right_ops, _) ]) when op = top ->
               List.concat [ collect left_ops; collect right_ops ]
            | disj -> [ disj ]
        in
        collect bv
    in
    let dnflist = match ant with _::_ -> bv_to_xlist OP_LOR  (bv_to_dnf (bvlist_to_bv ant)) | [] -> [] in
    let cnflist = match con with _::_ -> bv_to_xlist OP_LAND (bv_to_cnf (bvlist_to_bv con)) | [] -> [] in
    (*
    Output.print_endline ("||||\n"^(To_string.bytes_nf (bv_to_dnf (bvlist_to_bv ant))));
    Output.print_endline ("&&&&\n"^(To_string.bytes_nf (bv_to_cnf (bvlist_to_bv con))));
    *)

    let (=>) d c = begin
        (* create new STP checker, convert bv to stp, compare != 0 *)
        let vc = Stpc.create_validity_checker () in
        let (d', len) = Stp.to_stp_bv vc d in
        let (c', len) = Stp.to_stp_bv vc c in
        Stpc.assert_ctrue vc len d';
        Stpc.query_ctrue vc len c'
    end in
    List.fold_left (fun a d ->
        List.fold_left (fun a c ->
            a + if d => c then 0 else 1) a cnflist) 0 dnflist



(* create a new advice for calculating the inconsistency of assertions *)
let make_inconsistency_advice () =
    let total = ref 0 in
    let result = Hashtbl.create 8 in
    let advice = fun state argv instr ->
        let pc = state.path_condition in
        let assertion = argv in
        let truth = Stp.eval pc (bvlist_to_bv assertion) in
        let score = (gulwani pc assertion) in
        if truth == Stp.True then
            Output.print_endline "Assertion satisfied.\n"
        else begin
            Output.print_endline "Assertion not satisfied (see error log).";
            Executedebug.log "\n****************************";
            Executedebug.log (Format.sprintf "Assertion not-satisfied: (Gulwani inconsistency: %d)" score);
            Executedebug.log (To_string.bytes_list assertion);
            Executedebug.log "Is unsatisfiable with the path condition:";
            Executedebug.log (To_string.bytes_list pc);
            Executedebug.log "****************************"
        end;
        let old_score = try Hashtbl.find result instr with Not_found -> 0 in
        let new_score = old_score + score in
        total := !total + score;
        Hashtbl.replace result instr new_score;
        Output.print_endline (Format.sprintf "Culmulative Gulwani inconsistency: %d" new_score);
        state
    in
    (result, total, advice)



(* evaluate and return a table of inconsistency scores by __ASSERT site *)
let evaluate file func =
    Executedata.file := file;
    let (result, total, advice) = make_inconsistency_advice () in
    Function.with_aspect ("__ASSERT", advice) begin fun () ->
        let initstate = Executemain.init_globalvars MemOp.state__empty file.Cil.globals in
        (* let argvs = Executemain.init_cmdline_argvs func in *)
        let endstate = try Driver.exec_function initstate func [] Types.MainEntry
            with Function.Notification_Exit(state_exit,_) -> state_exit in
        (endstate, result, !total)
    end



(* evaluate and print a report *)
let doit file =
    let rec find_main_func = function
        | Cil.GFun(fundec, _)::_ when fundec.Cil.svar.Cil.vname = "main" -> fundec
        | _::tl -> find_main_func tl
        | _ -> failwith "main function not found!"
    in
    let main_func = find_main_func file.Cil.globals in
    let (_, result, total) = evaluate file main_func in
    Output.print_endline (Executedebug.get_log ());
    Output.print_endline "Summary of Gulwani inconsistency scores:";
    Hashtbl.iter (fun instr score ->
        match instr with
            | Cil.Call(_, _, _, loc) ->
                Output.print_endline (Format.sprintf "@[@[%30s:%-5d@]@ score %d@]" loc.Cil.file loc.Cil.line score);
            | _ -> failwith "Impossible!"
    ) result;
    Output.print_newline ();
    Output.print_endline (Format.sprintf "@[Total: %d@]" total)



let feature : Cil.featureDescr =  {
    Cil.fd_name = "inconsistency";
    Cil.fd_enabled = ref false;
    Cil.fd_description = "Analyze and show inconsistency score for __ASSERT";
    Cil.fd_extraopt = [];
    Cil.fd_post_check = false;
    Cil.fd_doit = doit
}
