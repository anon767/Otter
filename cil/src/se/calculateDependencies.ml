open Cil
open Types

let fileToRead = ref ""

module BytesSet = Set.Make
	(struct
		 type t = bytes
		 let compare = compare
	 end)

let calculateDeps coverage =
	let firstResult,restResults =
		match coverage with
			| hd::tl -> hd,tl
			| _ -> failwith "No coverage information"
	in
	let (alwaysExecuted,everExecuted) =
		List.fold_left
			(fun (interAcc,unionAcc) { result_history = hist } ->
				 LineSet.inter interAcc hist.coveredLines,
				 LineSet.union unionAcc hist.coveredLines)
			(firstResult.result_history.coveredLines,
			 firstResult.result_history.coveredLines)
			restResults
	in

	(* We need a bytesToVars in order to print out the conditions in a
		 readable way.  I think the way we are doing things now, all of the
		 bytesToVars should be the same, so picking an arbitrary one should
		 be fine, but is there a better, more general way to map bytes to
		 variables? *)
	let bytesToVars = firstResult.result_history.bytesToVars in

	Output.print_endline "Lines always executed:";
	LineSet.iter
		(fun (file,line) -> Output.printf "%s:%d\n" file line)
		alwaysExecuted;

	(* Gather all propositions from all path conditions, after
		 removing initial 'NOT's. *)
	let rec stripNots bytes = match bytes with
		| Bytes_Op(OP_LNOT,[(b',_)]) -> stripNots b'
		| _ -> bytes
	in
	let getPropSetFromPc pc =
		List.fold_left
			(fun acc bytes -> BytesSet.add (stripNots bytes) acc)
			BytesSet.empty
			pc
	in
	let allProps =
		List.fold_left
			(fun acc { result_state={ path_condition=pc } } ->
				 BytesSet.union (getPropSetFromPc pc) acc)
			BytesSet.empty
			coverage
	in

	BytesSet.iter (* For each proposition... *)
		(fun prop ->
			 (* partition the set of path conditions into ones with [prop]
					set to true on the one hand, and anything else on the
					other. (A path condition which doesn't mention [prop] goes
					into the 'other' category because it includes the case
					where [prop] is false.) *)
			 let rec saysPropIs t_or_f q =
				 match q with
					 | Bytes_Op(OP_LNOT,[(q',_)]) -> saysPropIs (not t_or_f) q'
					 | _ -> t_or_f && q = prop
			 in
			 let (propTrue,others) =
				 List.partition
					 (fun { result_state={ path_condition=pc } } ->
							List.exists (saysPropIs true) pc)
					 coverage
			 in
			 let (propFalse,propUnmentioned) =
				 List.partition
					 (fun { result_state={ path_condition=pc } } ->
							List.exists (saysPropIs false) pc)
					 others
			 in
			 let bigUnion =
				 List.fold_left LineSet.union LineSet.empty
			 and bigInter setList =
				 match setList with
					 | [] -> everExecuted (* An empty intersection leaves you with everything *)
					 | h::t -> List.fold_left LineSet.inter h t
							 (* Slight optimization: if the list is not empty, start
									with the first set, rather than with everything. *)
			 in
			 let lineSetsWithPropTrue =
				 List.map (fun jobRes -> jobRes.result_history.coveredLines) propTrue
			 and lineSetsWithPropFalse =
				 List.map (fun jobRes -> jobRes.result_history.coveredLines) propFalse
			 and lineSetsWithoutProp =
				 List.map (fun jobRes -> jobRes.result_history.coveredLines) propUnmentioned
			 in
			 let trueUnion = bigUnion lineSetsWithPropTrue
			 and trueInter = bigInter lineSetsWithPropTrue
			 and falseUnion = bigUnion lineSetsWithPropFalse
			 and falseInter = bigInter lineSetsWithPropFalse
			 and unmentionedUnion = bigUnion lineSetsWithoutProp
			 and unmentionedInter = bigInter lineSetsWithoutProp in
			 let pImpliesE = LineSet.inter trueInter unmentionedInter
			 and notPImpliesE = LineSet.inter falseInter unmentionedInter
			 and pImpliesNotE = LineSet.diff falseUnion (LineSet.union trueUnion unmentionedUnion)
			 and notPImpliesNotE = LineSet.diff trueUnion (LineSet.union falseUnion unmentionedUnion) in
			 let vennSplit set1 set2 =
				 let theIntersection = LineSet.inter set1 set2 in
				 (LineSet.diff set1 theIntersection, theIntersection, LineSet.diff set2 theIntersection)
			 in
			 let (pImplE,pIffE,notPImplNotE) = vennSplit pImpliesE notPImpliesNotE and
					 (notPImplE,notPIffE,pImplNotE) = vennSplit notPImpliesE pImpliesNotE in
			 Output.printf "For the proposition\nP = %s\n"
				 (To_string.humanReadableBytes bytesToVars prop);
			 let printLineSet messageFormat lineSet =
				 if not (LineSet.is_empty lineSet) then (
					 Output.printf messageFormat (LineSet.cardinal lineSet);
					 LineSet.iter
						 (fun (file,line) -> Output.printf "%s:%d\n" file line)
						 lineSet;
					 Output.print_newline ()
				 )
			 in
			 printLineSet "these %d lines are executed iff P:\n\n" pIffE;
			 printLineSet "and these %d lines are executed iff (not P):\n\n" notPIffE;
			 if not (List.for_all LineSet.is_empty [pImplE;notPImplNotE;notPImplE;pImplNotE])
			 then (
				 Output.printf "In addition, ";
				 printLineSet "these %d lines are executed if P:\n\n" pImplE;
				 printLineSet "these %d lines are executed only if P:\n\n" notPImplNotE;
				 printLineSet "these %d lines are executed if (not P):\n\n" notPImplE;
				 printLineSet "these %d lines are executed only if (not P):\n\n" pImplNotE
			 )
		)
		allProps

let readInDataAndGo _ = (* This needs to take a Cil.file, but I don't use it *)
	let inChan = open_in_bin !fileToRead in
	(* Read in the coverage data structure *)
	let coverage = (Marshal.from_channel inChan : job_result list) in
	calculateDeps coverage

let feature : Cil.featureDescr = {
  Cil.fd_enabled = ref false;
  Cil.fd_name = "calculateDeps";
  Cil.fd_description = "Calculate what lines depend on what symbolic variables.\n";
  Cil.fd_extraopt = [
		("--fromFile",
		 Arg.Set_string fileToRead,
		 "<filename> The file from which to read the coverage information")
	];
  Cil.fd_doit = readInDataAndGo;
  Cil.fd_post_check = false;
}
