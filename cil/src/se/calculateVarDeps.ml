open Cil
open Types

let covInfoFile = ref ""
let valuesFile = ref ""
(* Map variable names to possible values *)
let varToVals = Hashtbl.create 20

let concreteVarToVal = Hashtbl.create 20

(* We need a bytesToVars in order to print out the conditions in a
	 readable way.  I think the way we are doing things now, all of the
	 bytesToVars should be the same, so picking an arbitrary one should be
	 fine, but is there a better, more general way to map bytes to
	 variables? *)
let bytesToVars = ref []

let numVars = ref 1

(* CAUTION: Requires exponential amount of memory! *)
let rec allPossibleValues localBytesToVars hashtbl =
	match localBytesToVars with
		| [] -> [[]]
		| (bytes,var)::tail ->
				let allTailAssignments = allPossibleValues tail hashtbl in
				let listOfLists =
					List.map (* Attach each possible value to each possible value for the tail *)
						(fun value ->
							 List.map
								 (fun tailAssignment -> (bytes, value) :: tailAssignment)
								 allTailAssignments)
						(Hashtbl.find_all hashtbl var.vname)
				in
				List.concat listOfLists

let rec subsetsOfSize n lst =
	if n = 0 then [[]] else
	match List.length lst with
		| k when k < n -> failwith "lst not big enough"
		| k when k = n -> [lst]
		| _ -> let x = List.hd lst in (* lst cannot be empty here *)
			let withoutX = List.tl lst in
			let subsetsWithoutX = subsetsOfSize n withoutX
			and smallerSubsets = subsetsOfSize (pred n) withoutX in
			List.rev_append
				subsetsWithoutX
				(List.map (fun subset -> x :: subset) smallerSubsets)

(* Memoization table recording whether a path condition is consistent
	 with an assignment *)
let implicationHash = Hashtbl.create 1000

let makeEquality bytes value =
	(* Is intType okay here? *)
	Bytes_Op(OP_EQ,[(bytes,intType);
									(Convert.lazy_int_to_bytes value, intType)])

(* Is it possible for bytes to true, given the pc? Ask STP. *)
let mightBeTrue pc bytes =
	let result =
		match Stp.consult_stp pc bytes Stp.FalseOrNot with
			| Stp.False -> false (* Must be false *)
			| _ -> true (* Might be true *)
	in
	Output.set_mode Output.MSG_MUSTPRINT;
	result

(* Is pc consistent with 'var = value'? *)
let isConsistentWithAssignment pc (varBytes,value) =
	try (* See if the answer is memoized *)
		Hashtbl.find implicationHash (pc,varBytes,value)
	with Not_found ->
		let result = mightBeTrue pc (makeEquality varBytes value) in
		Hashtbl.add implicationHash (pc,varBytes,value) result; (* Memoize the answer *)
		result

(* Is the pc consistent with the assignments? *)
let isConsistentWithList pc assignments =
	(* Check that each assignment is consistent with pc; if any one
		 isn't consistent, then the set of assignments isn't either. *)
	if not (List.for_all (isConsistentWithAssignment pc) assignments)
	then false
	else (
		(* If each assignment is consistent, it is still possible for one
			 to contradict another (either directly or in the context of the
			 pc), so now we ask the SAT solver about all of the assignments
			 at once. *)
		match assignments with
			| [] -> failwith "Empty list in isConsistentWithList"
			| [_] -> true (* A single assignment cannot contradict itself *)
			| _ ->
					(let conjunction =
						 Bytes_Op(OP_LAND,
											List.map
												(fun (varBytes,value) -> (makeEquality varBytes value,
																								 intType))
												assignments)
					 in
					 mightBeTrue pc conjunction
					)
	)

(* Which of the (pc,lines) pairs have pcs which are consistent with
	 the assignments? *)
let consistentPCsAndLines allPCsAndLines assignments =
	List.filter (fun (pc,_) -> isConsistentWithList pc assignments) allPCsAndLines

let bigUnionForSymbols = List.fold_left SymbolSet.union SymbolSet.empty
let bigUnion = List.fold_left LineSet.union LineSet.empty
let bigInter setList =
	match setList with
		| [] -> failwith "Taking intersection of no sets"
		| h::t -> List.fold_left LineSet.inter h t
				(* Slight optimization: if the list is not empty, start
					 with the first set, rather than with everything. *)

let linesControlledBy pcsAndLines assignments =
	let result =
		match List.map snd (consistentPCsAndLines pcsAndLines assignments) with
			| [] -> failwith "Impossible: assignments not compatible with any pc"
			| x -> bigInter x
	in
	if not (LineSet.is_empty result) then (
		Output.printf "\nUnder the condition\n%s\nthese %d lines are hit\n"
			(String.concat "\n"
				 (List.map
						(fun (varBytes,value) ->
							 Printf.sprintf "%s=%d"
								 (To_string.humanReadableBytes !bytesToVars varBytes)
								 value)
						assignments))
			(LineSet.cardinal result);
		LineSet.iter (fun (file,line) -> Output.printf "%s:%d\n" file line) result
	);
	result

exception Stop of (bytes list * LineSet.t) list * LineSet.t

(* This version never removes lines from consideration *)
let findDependencies (pcsAndLines,linesToExplain) localBytesToVars =
	Output.printf "\nConsidering\n%s\n"
		(String.concat ", " (List.map (fun (_,var) -> var.vname) localBytesToVars));
	List.iter
		(fun assignments ->
			 ignore (linesControlledBy pcsAndLines assignments))
		(allPossibleValues localBytesToVars varToVals);
	(pcsAndLines,linesToExplain)
(*
(* This version removes lines once they are controlled by something. *)
let findDependenciesAndUpdate (pcsAndLines,linesToExplain) localBytesToVars =
	Output.printf "\n%d lines left\n" (LineSet.cardinal linesToExplain);

	if LineSet.is_empty linesToExplain
	then raise (Stop (pcsAndLines,linesToExplain));


	Output.printf "\nConsidering\n%s\n"
		(String.concat ", " (List.map (fun (_,var) -> var.vname) localBytesToVars));
	let linesControlledByVars =
		bigUnion
			(List.map
				 (fun assignments ->
						linesControlledBy pcsAndLines assignments)
				 (allPossibleValues localBytesToVars varToVals))
	in
	(* Remove the lines that have been accounted for from each pair of
		 pcsAndLines, and from linesToExplain. *)
	(List.map
		 (fun (pc,lines) -> pc,LineSet.diff lines linesControlledByVars)
		 pcsAndLines,
	 LineSet.diff linesToExplain linesControlledByVars)
*)

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

	(* Other than the bytesToVars, all we really need are the path
		 conditions (without the __ASSUMEs) and the coverage
		 information. Also, we can remove the lines that are always
		 executed from each path's coverage info. *)
  let rec eliminate_untracked apc apct =
    match apc,apct with 
      | [],[]->([],[])
      | apch::apct,apcth::apctt -> 
          let (apct1,apct2) = eliminate_untracked apct apctt in
          if apcth then (apch::apct1,apct2) else  (apct1,apch::apct2)
      | _,_ -> failwith "Impossible: path_condition and path_condition_tracked must be of equal length"
  in
	let pcsAndLines = List.map
		(fun jobRes ->
			 (fst (eliminate_untracked
							 jobRes.result_state.path_condition
							 jobRes.result_state.path_condition_tracked),
				LineSet.diff jobRes.result_history.coveredLines alwaysExecuted)
		)
		coverage
	in

	(* Pick out variables that appear in some path condition, ignoring
		 those that aren't mentioned anywhere. *)
	let symbolsInPcs =
		bigUnionForSymbols
			(List.map (fun (pc,_) -> Stp.allSymbolsInList pc) pcsAndLines)
	in
	bytesToVars :=
		List.filter
			(fun (bytes,_) ->
				 not (SymbolSet.is_empty
								(SymbolSet.inter (Stp.allSymbols bytes) symbolsInPcs)))
			!bytesToVars;
	Output.printf "These %d variables are mentioned in the path conditions:\n"
		(List.length !bytesToVars);
	List.iter (fun (_,var) -> Output.printf "%s\n" var.vname) !bytesToVars;

	Output.printf "\n%d lines always executed:\n" (LineSet.cardinal alwaysExecuted);
	LineSet.iter
		(fun (file,line) -> Output.printf "%s:%d\n" file line)
		alwaysExecuted;

	(* Explain all the lines we can *)
	let remainingPCsAndLines = ref pcsAndLines (* Initial path conditions and coverage information *)
	and remainingLines = ref (LineSet.diff everExecuted alwaysExecuted) in (* All lines that weren't always executed *)

	ignore(
		List.fold_left
			findDependencies
			(!remainingPCsAndLines, !remainingLines)
			(subsetsOfSize !numVars !bytesToVars))

(* (* If you remove explained lines, replace the preceding lines with this: *)
	(try
		 let (nextPcs,nextLines) =
			 List.fold_left
				 findDependenciesAndUpdate
				 (!remainingPCsAndLines, !remainingLines)
				 (subsetsOfSize !numVars !bytesToVars)
		 in
		 remainingPCsAndLines := nextPcs;
		 remainingLines := nextLines
	 with Stop(_,_) -> ()
	)
	Output.printf "These lines are unexplained:\n";
	LineSet.iter (fun (file,line) -> Output.printf "%s:%d\n" file line) !remainingLines
*)

let readInDataAndGo _ = (* This needs to take a Cil.file, but I don't use it *)
	let inChan = open_in_bin !covInfoFile in
	(* Read in the coverage data structure *)
	let coverage = (Marshal.from_channel inChan : job_result list) in
	close_in inChan;
	let inChan = open_in !valuesFile in
	try
		let line = ref (input_line inChan) in
		(* Read in the possible variable values *)
		while !line <> "" do
			begin
			match Str.split (Str.regexp "[\t ]+") !line with
					[] -> failwith "Badly formatted input"
				| var::valList ->
						List.iter
							(fun str -> Hashtbl.add varToVals var (int_of_string str))
							valList
			end;
			line := input_line inChan
		done;
		(* Read in the required values *)
		line := input_line inChan;
		while true do
			begin
				match Str.split (Str.regexp "[\t ]+") !line with
					| [var;value] -> Hashtbl.add concreteVarToVal var (int_of_string value)
					| _ -> failwith "Badly formatted input"
			end;
			line := input_line inChan
		done
	with End_of_file ->
		close_in inChan;
		let bytesToVars = (List.hd coverage).result_history.bytesToVars in
		let bytesToVars_concrete = List.filter (fun (_,varinf) -> Hashtbl.mem concreteVarToVal varinf.vname) bytesToVars in
		let concreteAssignments =
			match allPossibleValues bytesToVars_concrete concreteVarToVal with
					[x] -> x
				| _ -> failwith "There can't be more than one concrete value"
		in
		if concreteAssignments = [] then
			calculateDeps coverage
		else
			calculateDeps (List.filter (fun jobRes -> isConsistentWithList jobRes.result_state.path_condition concreteAssignments) coverage)
;;

let feature : Cil.featureDescr = {
  Cil.fd_enabled = ref false;
  Cil.fd_name = "calculateVarDeps";
  Cil.fd_description = "Calculate what lines depend on what symbolic variables.\n";
  Cil.fd_extraopt = [
		("--fileWithCovInfo",
		 Arg.Set_string covInfoFile,
		 "<filename> The file from which to read the coverage information\n");
		("--fileWithPossibleValues",
		 Arg.Set_string valuesFile,
		 "<filename> File from which to read the possible values for variables
\t\t\tFormat is \"varname val_1 val_2 ... val_k\", with one variable per line,
\t\t\tand the values must all be integers\n");
		("--numVars",
		 Arg.Set_int numVars,
		 "<n> Consider n-tuples of variables (default: 1)\n");
	];
  Cil.fd_doit = readInDataAndGo;
  Cil.fd_post_check = false;
}
