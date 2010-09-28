open CilUtilities

(* set up the file for symbolic execution *)
let prepare_file file =
	(* makeCFGFeature must precede the call to Coverage.prepare_file. *)
	Cilly.makeCFGFeature.Cil.fd_doit file;

	Cil.iterGlobals file begin function
		| Cil.GFun(fundec,_) ->
			(* Reset sids to be unique only within a function, rather than globally, so that they will be given
				consistent sids across different analysis runs even if different files are merged
			*)
			ignore (List.fold_left (fun n stmt -> stmt.Cil.sid <- n; succ n) 0 fundec.Cil.sallstmts);

		| _ ->
			()
	end;

	if !Executeargs.arg_noinit_unreachable_globals then
		Coverage.computeReachableCode file;

	Coverage.prepare_file file

