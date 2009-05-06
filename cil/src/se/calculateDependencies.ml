open Cil
open Types

let fileToRead = ref ""

(* We need a bytesToVars in order to print out the conditions in a
	 readable way.  I think the way we are doing things now, all of the
	 bytesToVars should be the same, so picking an arbitrary one should be
	 fine, but is there a better, more general way to map bytes to
	 variables? *)
let bytesToVars = ref []

module BytesSet = Set.Make
	(struct
		 type t = bytes
		 let compare = compare
	 end)

(* CAUTION: Requires exponential amount of memory! *)
let rec allTruthValues bytesList =
	match bytesList with
		| [] -> [[]]
		| b::tail ->
				let tvTail = allTruthValues tail in
				List.rev_append
					(List.map (fun lst -> (b, true) :: lst) tvTail)
					(List.map (fun lst -> (b,false) :: lst) tvTail)
(*
(* CAUTION: Requires exponential amount of memory! *)
let allSubsets set =
	let rec allSubsets set =
		if BytesSet.is_empty set then [BytesSet.empty]
		else let x = BytesSet.choose set in
		let smallerSubsets = allSubsets (BytesSet.remove x set) in
		List.rev_append
			smallerSubsets
			(List.map (fun subset -> BytesSet.add x subset) smallerSubsets)
	in
	List.sort (* Sort in increasing order of cardinality *)
		(fun set1 set2 -> compare (BytesSet.cardinal set1) (BytesSet.cardinal set2))
		(allSubsets set)
*)
let rec subsetsOfSize n set =
	if n = 0 then [BytesSet.empty] else
	match BytesSet.cardinal set with
		| k when k < n -> failwith "set not big enough"
		| k when k = n -> [set]
		| _ -> let x = BytesSet.choose set in
			let setWithoutX = BytesSet.remove x set in
			let subsetsWithoutX = subsetsOfSize n setWithoutX
			and smallerSubsets = subsetsOfSize (pred n) setWithoutX in
			List.rev_append
				subsetsWithoutX
				(List.map (fun subset -> BytesSet.add x subset) smallerSubsets)

type truthValue = True | False | Either

(* Memoization table recording whether a path condition implies a
	 proposition, implies its negation, or neither *)
let implicationHash = Hashtbl.create 1000

(* If q is essentially prop, return the truthValue corresponding to t_or_f.
	 If q is essentially NOT(prop), return the truthValue corresponding to (not t_or_f).
	 Othwerise, return Either. *)
let rec saysPropIs t_or_f q prop =
	match q with
		| Bytes_Op(OP_LNOT,[(q',_)]) -> saysPropIs (not t_or_f) q' prop
(*
		(* Are these next 2 correct? *)
		| Bytes_Op(OP_LAND,[(q1,_);(q2,_)]) when t_or_f ->
				saysPropIs true q1 || saysPropIs true q2
		| Bytes_Op(OP_LOR,[(q1,_);(q2,_)]) when not t_or_f ->
				saysPropIs false q1 || saysPropIs false q2
*)
		| _ ->
				if q = prop then (
					if t_or_f then True else False
				) else Either

(* Is pc consistent with prop being t_or_f? *)
let isConsistentWithProp pc (prop,t_or_f) =
	let returnResult tv =
		match tv with
			| True -> t_or_f (* Consistent with prop being true, but not false *)
			| False -> not t_or_f (* Consistent with prop being false, but not true *)
			| Either -> true (* Consistent in either case *)
	in
	try (* See if the answer is memoized *)
		returnResult (Hashtbl.find implicationHash (pc,prop))
	with Not_found ->
		let rec helper subPC =
			match subPC with
				| clause::t ->
						(match saysPropIs t_or_f clause prop with
							 | True -> if t_or_f then True else False
							 | False -> if t_or_f then False else True
							 | Either -> helper t
						)
				| [] ->
						(* The pc didn't explicitly include prop or not-prop, so ask STP *)
						Executeargs.print_args.Executeargs.arg_print_nothing <- true; (* Suppress printing "Ask STP..." *)
						let stpResult = Stp.eval pc prop in
						Executeargs.print_args.Executeargs.arg_print_nothing <- false;
						Output.set_mode Output.MSG_MUSTPRINT;
						match stpResult with
							| Stp.True -> True
							| Stp.False -> False
							| Stp.Unknown -> Either
		in
		let result = helper pc in (* See whether pc implies prop, not-prop, or neither *)
		Hashtbl.add implicationHash (pc,prop) result; (* Memoize the answer *)
		returnResult result

let isConsistentWithList pc propAndTruthValueList =
	(* Check that each one is consistent with pc; if not, their
		 conjunction can't be. *)
	if not (List.for_all (isConsistentWithProp pc) propAndTruthValueList)
	then false
	else (
		(* If each one is consistent, it is still possible for some to
			 contradict each other (in the context of the pc), so now we ask
			 the SAT solver about the conjunction *)
		match propAndTruthValueList with
			| [] -> failwith "Empty list in isConsistentWithList"
			| [_] -> true (* A single prop cannot contradict itself, so it is consistent *)
			| _ ->
					(let conjunction =
						 Bytes_Op(OP_LAND,
											List.map
												(fun (prop,t_or_f) ->
													 ((if t_or_f then prop else Bytes_Op(OP_LNOT,[(prop,intType)])),
														intType)
												)
												propAndTruthValueList
										 )
					 in
					 isConsistentWithProp pc (conjunction,true))
	)

let consistentPCsAndLines allPCsAndLines propAndTruthValueList =
	List.filter (fun (pc,_) -> isConsistentWithList pc propAndTruthValueList) allPCsAndLines

let bigUnion = List.fold_left LineSet.union LineSet.empty
let bigInter setList =
	match setList with
		| [] -> failwith "Taking intersection of no sets"
		| h::t -> List.fold_left LineSet.inter h t
				(* Slight optimization: if the list is not empty, start
					 with the first set, rather than with everything. *)

let linesControlledBy pcsAndLines allLines propAndTruthValueList =
	let result =
		match List.map snd (consistentPCsAndLines pcsAndLines propAndTruthValueList) with
			| [] -> LineSet.empty (* propAndTruthValueList is a contradiction *)
			| x -> bigInter x
	in
	if not (LineSet.is_empty result) then (
		Printf.printf "\nUnder the condition\n%s\nthese lines are hit\n"
			(String.concat "\n"
				 (List.map
						(fun (prop,truthValue) ->
							 Printf.sprintf "%s -> %b"
								 (To_string.humanReadableBytes !bytesToVars prop)
								 truthValue)
						propAndTruthValueList));
		LineSet.iter (fun (file,line) -> Printf.printf "%s:%d\n" file line) result
	);
	result

exception Stop of (bytes list * LineSet.t) list * LineSet.t

let findDependenciesAndUpdate (pcsAndLines,linesToExplain) propSet =
	Printf.printf "\n%d lines left\n" (LineSet.cardinal linesToExplain);

	if LineSet.is_empty linesToExplain
	then raise (Stop (pcsAndLines,linesToExplain));

	Printf.printf "\nConsidering\n%s\n" (To_string.humanReadablePc (BytesSet.elements propSet) !bytesToVars);
	let linesControlledByPropSet =
		bigUnion
			(List.map
				 (fun propTvList ->
						linesControlledBy pcsAndLines linesToExplain propTvList)
				 (allTruthValues (BytesSet.elements propSet)))
	in
(*
 (* Removing path conditions is not okay, even when they don't have any lines left: consider
		if (x) {
		  exit();
		}
		if (y) {
		  ...;
		}
 *)
	(* Remove the lines that have been accounted for from each pair and
		 from linesToExplain. Also, if any path condition now has no lines
		 left, remove it. *)
	(List.fold_left
		 (fun lst (pc,lines) ->
				let remainingLines = LineSet.diff lines linesControlledByPropSet in
				if LineSet.is_empty remainingLines
				then lst
				else (pc,remainingLines)::lst)
		 []
		 pcsAndLines,
*)
	(* Remove the lines that have been accounted for from each pair and
		 from linesToExplain. *)
	(List.map (fun (pc,lines) -> pc,LineSet.diff lines linesControlledByPropSet) pcsAndLines,

	 LineSet.diff linesToExplain linesControlledByPropSet)

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

	(* See comment at top of file *)
	bytesToVars := firstResult.result_history.bytesToVars;

	print_endline "Lines always executed:";
	LineSet.iter
		(fun (file,line) -> Printf.printf "%s:%d\n" file line)
		alwaysExecuted;

	(* Gather all propositions from all path conditions, after
		 removing initial 'NOT's. Also, ignore __ASSUMEs.*)
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

  let rec eliminate_untracked apc apct =
    match apc,apct with 
      | [],[]->([],[])
      | apch::apct,apcth::apctt -> 
          let (apct1,apct2) = eliminate_untracked apct apctt in
          if apcth then (apch::apct1,apct2) else  (apct1,apch::apct2)
      | _,_ -> failwith "Impossible: path_condition and path_condition_tracked must be of equal length"
  in

	let allProps =
		List.fold_left
			(fun acc { result_state=state } ->
				 BytesSet.union
					 (getPropSetFromPc
							(fst (eliminate_untracked (state.path_condition) (state.path_condition_tracked))))
					 acc)
			BytesSet.empty
			coverage
	in
	print_endline "\nallProps:";
	BytesSet.iter (fun bytes -> print_endline (To_string.humanReadableBytes !bytesToVars bytes)) allProps;
	print_newline();

	(* At this point, all we really need are the path conditions and the
		 coverage information. Also, we can remove the lines that are
		 always executed from each path's coverage info. *)
	let pcsAndLines = List.map
		(fun jobRes ->
			 jobRes.result_state.path_condition,
			 LineSet.diff jobRes.result_history.coveredLines alwaysExecuted)
		coverage
	in
(*
	(* Explain all the lines we can *)
	let (remainingPCsAndLines,remainingLines) =
		try
		List.fold_left
			findDependenciesAndUpdate
			(pcsAndLines, LineSet.diff everExecuted alwaysExecuted) (* Initial path conditions and coverage information, and all lines that weren't always executed *)
			(match allSubsets allProps with
					 _::t -> t (* Ignore the empty set *)
				 | _ -> assert false)
		with Stop(x,y) -> x,y
	in
*)
	(* Explain all the lines we can *)
	let remainingPCsAndLines = ref pcsAndLines (* Initial path conditions and coverage information *)
	and remainingLines = ref (LineSet.diff everExecuted alwaysExecuted) in (* All lines that weren't always executed *)
	try
		for size = 1 to 3 do (* We probably won't be able get past 3, if we can even get that far *)
			Printf.printf "Trying subsets of size %d\n" size;
			let (nextPcs,nextLines) =
				List.fold_left
					findDependenciesAndUpdate
					(!remainingPCsAndLines, !remainingLines)
					(subsetsOfSize size allProps)
			in
			remainingPCsAndLines := nextPcs;
			remainingLines := nextLines
		done
	with Stop(_,_) -> ();
	print_endline "These lines remain unexplained:";
	LineSet.iter (fun (file,line) -> Printf.printf "%s:%d\n" file line) !remainingLines

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
