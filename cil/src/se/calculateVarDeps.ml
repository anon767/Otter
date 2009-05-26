open Cil
open Types

(* We need a bytesToVars in order to print out the conditions in a
	 readable way.  I think the way we are doing things now, all of the
	 bytesToVars should be the same, so picking an arbitrary one should be
	 fine, but is there a better, more general way to map bytes to
	 variables? *)
let bytesToVars = ref []

type 'a tree =
	| Node of 'a * 'a tree list (* (data, children) *)

type varDepTree = (bytes * LineSet.t) tree

(* trueBytes will be at the root of the tree *)
let trueBytes = Convert.lazy_int_to_bytes 1

(* This makes a tree which is a single path. lst is represented up
	 from the bottom of the path, with trueBytes at the root. Also, the
	 leaf gets cov as its coverage, while all internal nodes get
	 LineSet.empty *)
let lstToTree lst cov : varDepTree =
	match lst with
		| [] -> failwith "I don't think this should happen" (* Node ((trueBytes,cov), []) *)
		| h::t ->
				let rec helper acc = function
					| [] -> Node ((trueBytes,LineSet.empty), acc)
					| h::t -> helper [Node ((h,LineSet.empty), acc)] t
				in
				helper [Node ((h,cov),[])] t

let myEqual bytes1 (bytes2,_) = MemOp.same_bytes bytes1 bytes2

let findMatchingChild equal elt children : 'a tree * 'a tree list =
	let rec helper acc = function
		| Node(otherElt,_) as x :: t ->
				if equal elt otherElt
				then x, List.rev_append acc t
				else helper (x::acc) t
		| _ -> raise Not_found
	in
	helper [] children

let rec add_aux lst cov tree : varDepTree =
	match lst,tree with
		| [], Node ((bytes, oldCov), children) -> (* lst is already represented in the tree *)
				Node ((bytes, LineSet.union cov oldCov), children) (* Union in the coverage *)
		| h::t, Node (value, children) ->
				try (* See if we can step to a child *)
					let matchingChild,otherChildren =
						findMatchingChild myEqual h children
					in
					(* Try to add the rest of lst to the child that matches *)
					let newTree = add_aux t cov matchingChild in
					Node (value, newTree :: otherChildren)
				with Not_found ->
					(* No child matches; add lst as a new child *)
					let subtree = match lstToTree (List.rev lst) cov with
						| Node (_,[x]) -> x
						| _ -> failwith "lstToTree can't return this"
					in
					Node (value, subtree :: children)

(* Add a list into the tree *)
let add lst cov tree = add_aux (List.rev lst) cov tree

let rec getLeavesWithCoverage_aux pc_acc cov_acc tree = match tree with
	| Node ((bytes,cov), children) ->
			let newPc = bytes::pc_acc
			and newCov = LineSet.union cov cov_acc in
			if children = []
			then [(newPc,newCov)]
			else List.concat (List.map (getLeavesWithCoverage_aux newPc newCov) children)

let getLeavesWithCoverage tree = match tree with
	| Node ((x,y),children) when x == trueBytes && y == LineSet.empty ->
			(* Ignore trueBytes at root of tree *)
			List.concat
				(List.map (getLeavesWithCoverage_aux [] LineSet.empty) children)
	| _ -> failwith "Impossible: root must be (trueBytes,LineSet.empty)"
(*
let rec countLeaves = function
	| Node (_,[]) -> 1
	| Node (_,children) -> List.fold_left (fun sum subtree -> sum + countLeaves subtree) 0 children

let rec countNodes = function
	| Node (_,children) -> List.fold_left (fun sum subtree -> sum + countNodes subtree) 1 children

let rec printTree_aux indent = function
	| Node ((bytes,cov),children) ->
			Format.printf "%s%s (%d)\n" indent (To_string.humanReadableBytes !bytesToVars bytes) (LineSet.cardinal cov);
			List.iter (printTree_aux (indent^"\t")) children

let printTree = printTree_aux ""
*)
let covInfoFiles = ref []
let valuesFile = ref ""
(* Map variable names to possible values *)
let varToVals = Hashtbl.create 20

let numVars = ref 1

(* CAUTION: Requires exponential amount of memory! *)
let rec allPossibleValues localBytesToVars =
	match localBytesToVars with
		| [] -> [[]]
		| (bytes,var)::tail ->
				let allTailAssignments = allPossibleValues tail in
				let listOfLists =
					List.map (* Attach each possible value to each possible value for the tail *)
						(fun value ->
							 List.map
								 (fun tailAssignment -> (bytes, value) :: tailAssignment)
								 allTailAssignments)
						(Hashtbl.find_all varToVals var.vname)
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

let makeEquality bytes value =
	(* Is intType okay here? *)
	Bytes_Op(OP_EQ,[(bytes,intType);
									(Convert.lazy_int_to_bytes value, intType)])
(*
(* Is a pc consistent with a set of assignments? *)
let implicationHash = Hashtbl.create 1000

(* Is it possible for bytes to true, given the pc? Ask STP. *)
let mightBeTrue pc bytes =
	try (* See if the answer is memoized *)
		Hashtbl.find implicationHash (pc,bytes)
	with Not_found -> (* Answer isn't memoized; ask STP *)
		let result =
			match Stp.consult_stp pc bytes Stp.FalseOrNot with
				| Stp.False -> false (* Must be false *)
				| _ -> true (* Might be true *)
		in
		Hashtbl.add implicationHash (pc,bytes) result; (* Memoize the answer *)
		result

(* This checks all subsets to see if they are inconsistent before
	 checking the assignment itself. If subsets are often inconsistent,
	 this should prevent calls to the SAT solver and, hence, speed
	 things up. In some tests I tried, though, this runs much more
	 slowly than just checking the single assignments for consistency,
	 so I'm sticking with the other version for now. *)
let isConsistentWithList pc assignments =
	let rec helper size =
		let isInconsistent =
			List.exists
				(fun (assns:(bytes*int) list) ->
					 let equalities =
						 match assns with
							 | [] -> failwith "Empty list in isConsistentWithList"
							 | [(varBytes,value)] -> makeEquality varBytes value
							 | _ ->
									 Bytes_Op(OP_LAND,
														List.map
															(fun (varBytes,value) -> (makeEquality varBytes value,
																												intType))
															assns)
					 in
					 not (mightBeTrue pc equalities)
				)
				(subsetsOfSize size assignments)
		in
		if isInconsistent then false
		else if size = List.length assignments then true (* The whole thing is consistent *)
		else helper (succ size) (* Try larger subsets *)
	in
	helper 1
*)

(* Memoization table recording whether a path condition is consistent
	 with an assignment *)
let implicationHash = Hashtbl.create 1000

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
		Format.printf "\nUnder the condition\n%s\nthese %d lines are hit\n"
			(String.concat "\n"
				 (List.map
						(fun (varBytes,value) ->
							 Printf.sprintf "%s=%d"
								 (To_string.humanReadableBytes !bytesToVars varBytes)
								 value)
						assignments))
			(LineSet.cardinal result);
		LineSet.iter (fun (file,line) -> Format.printf "%s:%d\n" file line) result
	);
	result

let findDependenciesAndUpdate (pcsAndLines,linesToExplain) localBytesToVars =
	Format.printf "\nConsidering\n%s\n"
		(String.concat ", " (List.map (fun (_,var) -> var.vname) localBytesToVars));
	let linesControlledByVars =
		bigUnion
			(List.map
				 (fun assignments ->
						linesControlledBy pcsAndLines assignments)
				 (allPossibleValues localBytesToVars))
	in
	(* Remove the lines that have been accounted for from each pair of
		 pcsAndLines, and from linesToExplain. *)
	(List.map
		 (fun (pc,lines) -> pc,LineSet.diff lines linesControlledByVars)
		 pcsAndLines,
	 LineSet.diff linesToExplain linesControlledByVars)

exception EverythingCovered

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
	(* Make sure all bytesToVars are the same *)
	List.iter
		(fun {result_history={bytesToVars=b2v}} -> if b2v <> !bytesToVars then failwith "Not all bytesToVars are equal")
		restResults;

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

	let treeOfPcs =
		List.fold_left
			(fun treeSoFar (pc,cov) -> add pc cov treeSoFar)
			(Node ((trueBytes,LineSet.empty), []))
			pcsAndLines
	in
	let maximalPcsAndLines = getLeavesWithCoverage treeOfPcs in
Format.printf "%d (instead of %d) path conditions\n" (List.length maximalPcsAndLines) (List.length pcsAndLines);

	(* Pick out variables that appear in some path condition, ignoring
		 those that aren't mentioned anywhere. *)
	let symbolsInPcs =
		bigUnionForSymbols
			(List.map (fun (pc,_) -> Stp.allSymbolsInList pc) maximalPcsAndLines)
	in
	bytesToVars :=
		List.filter
			(fun (bytes,_) ->
				 not (SymbolSet.is_empty
								(SymbolSet.inter (Stp.allSymbols bytes) symbolsInPcs)))
			!bytesToVars;
	Format.printf "These %d variables are mentioned in the path conditions:\n"
		(List.length !bytesToVars);
	List.iter (fun (_,var) -> Format.printf "%s\n" var.vname) !bytesToVars;

	Format.printf "\n%d lines always executed:\n" (LineSet.cardinal alwaysExecuted);
	LineSet.iter
		(fun (file,line) -> Format.printf "%s:%d\n" file line)
		alwaysExecuted;

	(* Explain all the lines we can *)
	let remainingPCsAndLines = ref maximalPcsAndLines (* Initial path conditions and coverage information *)
	and remainingLines = ref (LineSet.diff everExecuted alwaysExecuted) in (* All lines that weren't always executed *)

	(try
		 let size = ref 1 in
		 while true do
			 Format.printf "\n%d lines left\n" (LineSet.cardinal !remainingLines);
			 LineSet.iter
				 (fun (file,lineNum) -> Format.printf "%s:%d\n" file lineNum)
				 !remainingLines;
			 if LineSet.is_empty !remainingLines
			 then raise EverythingCovered;
			 let (nextPcs,nextLines) =
				 List.fold_left
					 findDependenciesAndUpdate
					 (!remainingPCsAndLines, !remainingLines)
					 (subsetsOfSize !size !bytesToVars)
			 in
			 remainingPCsAndLines := nextPcs;
			 remainingLines := nextLines;
			 incr size
		 done
	 with EverythingCovered -> ()
	)

let readInDataAndGo _ = (* This needs to take a Cil.file, but I don't use it *)
	let coverage =
		List.fold_left
			(fun acc file ->
				 let inChan = open_in_bin file in
				 (* Read in the coverage data structure *)
				 let res = List.rev_append (Marshal.from_channel inChan : job_result list) acc in
				 close_in inChan;
				 res)
			[]
			!covInfoFiles
	in
	let inChan = open_in !valuesFile in
	(* Read in the possible variable values *)
	try
		while true do
			match Str.split (Str.regexp "[\t ]+") (input_line inChan) with
					[] -> failwith "Badly formatted input"
				| var::valList ->
						List.iter
							(fun str -> Hashtbl.add varToVals var (int_of_string str))
							valList
		done
	with End_of_file ->
		close_in inChan;
		calculateDeps coverage
;;

let feature : Cil.featureDescr = {
  Cil.fd_enabled = ref false;
  Cil.fd_name = "calculateVarDeps";
  Cil.fd_description = "Calculate what lines depend on what symbolic variables.\n";
  Cil.fd_extraopt = [
		("--fileWithCovInfo",
		 Arg.String (fun s -> covInfoFiles := s :: !covInfoFiles),
		 "<filename> A file from which to read the coverage information\n");
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
