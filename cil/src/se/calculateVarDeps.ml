open Cil
open Types

let falseBytes = Bytes_Constant (Cil.CInt64 (0L,IInt,None))
let trueBytes  = Bytes_Constant (Cil.CInt64 (1L,IInt,None))

(* Do some minor simplification of logical operators. I'm not being
	 careful to maintain a boolean context when I strip off ANDs or
	 ORs. *)
let rec simplifyLogicalOps bytes = match bytes with
	| Bytes_Op(OP_LNOT,[b,typ]) ->
			Bytes_Op(OP_LNOT, [simplifyLogicalOps b, typ])
	| Bytes_Op(OP_LAND|OP_LOR as op, [b1,typ1 ; b2,typ2]) ->
			begin
				let (shortcutBytes,otherBytes) =
					if op == OP_LAND
					then falseBytes,trueBytes
					else trueBytes,falseBytes
				in
				match simplifyLogicalOps b1 with
					| b1' when b1' == shortcutBytes -> b1'
					| b1' when b1' == otherBytes -> simplifyLogicalOps b2
					| b1' ->
							match simplifyLogicalOps b2 with
								| b2' when b2' == shortcutBytes -> b2'
								| b2' when b2' == otherBytes -> b1'
								| b2' -> Bytes_Op(op, [b1',typ1 ; b2',typ2])
			end
	| _ ->
			try
				if Convert.bytes_to_bool bytes
				then trueBytes
				else falseBytes
			with Failure _ -> bytes

(* We need a bytesToVars in order to print out the conditions in a
	 readable way.  I think the way we are doing things now, all of the
	 bytesToVars should be the same, so picking an arbitrary one should be
	 fine, but is there a better, more general way to map bytes to
	 variables? *)
let bytesToVars = ref []

let totalNumberOfPcs = ref 0

type 'a tree =
	| Node of 'a * 'a tree list (* (data, children) *)

type varDepTree = (bytes * LineSet.t) tree

(* Placeholder value for the root of the tree *)
let rootNodeData = (trueBytes, LineSet.empty)

(* This makes a tree which is a single path. lst is represented up
	 from the bottom of the path, with trueBytes at the root. Also, the
	 leaf gets cov as its coverage, while all internal nodes get
	 LineSet.empty *)
let lstToTree lst cov : varDepTree =
	match lst with
		| [] -> failwith "I don't think this should happen" (* Node ((trueBytes,cov), []) *)
		| h::t ->
				let rec helper acc = function
					| [] -> Node (rootNodeData, acc)
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
	| Node (x,children) when x == rootNodeData ->
			(* Ignore the root of tree, which is just a placeholder *)
			List.concat
				(List.map (getLeavesWithCoverage_aux [] LineSet.empty) children)
	| _ -> failwith "Impossible: bad root of tree"
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

let valuesFile = ref ""
(* Map variable names to possible values *)
let varToVals = Hashtbl.create 20

let makeEquality bytes value =
	(* Is intType okay here? *)
	Bytes_Op(OP_EQ,[(bytes,intType);
									(Convert.lazy_int_to_bytes value, intType)])

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
				(List.rev_map (fun subset -> x :: subset) smallerSubsets)
				subsetsWithoutX

module PcAssn = Set.Make
	(struct
		 type t = bytes list * (bytes*int) list (* pc, assignments *)
		 let compare ((pc1,cov1) as x:t) ((pc2,cov2) as y) = (* Type annotation to remove polymorphism *)
			 if pc1 == pc2 (* Try physical comparison on pc1 and pc2 before structural. *)
			 then compare cov1 cov2
			 else compare x y
	 end)

(* Cache of (pc,assignment) pairs that contradict each other *)
let contradictionCache = ref PcAssn.empty

let numberOfHits = Array.make 5 0
let numberOfMisses = Array.make 5 0
let numberOfSTPCalls = Array.make 5 0

let rec range a b = if a > b then [] else a :: range (succ a) b

let knownContradiction pc equalities =
	let sizesToCheck =
		match equalities with
				[] -> failwith "knownContradiction: empty list"
			| [_] -> [] (* Single equalities are never already known *)
			| [_;_] -> [1]
			| _ -> range 1 (List.length (List.tl equalities)) (* Change this to modify the method of optimization. I'm not sure going beyond size 1, and maybe size 2, will be helpful in practice. *)
	in
	let existsContradictionOfSize n =
		List.exists
			(fun assns ->
				 let contradictory = PcAssn.mem (pc,assns) !contradictionCache in
				 let arr = if contradictory then numberOfHits else numberOfMisses in
				 arr.(n) <- succ arr.(n);
				 contradictory)
			(subsetsOfSize n equalities)
	in
	List.exists existsContradictionOfSize sizesToCheck

let isConsistentWithList pc assignments =
	if Stats.time "knownContradiction" (fun x -> knownContradiction pc x) assignments then false
	else
		let theQuery =
			match assignments with
				| [] -> failwith "Empty list in isConsistentWithList"
				| [var,value] -> makeEquality var value
				| _ ->
						Bytes_Op(OP_LAND,
										 List.map
											 (fun (var,value) -> (makeEquality var value, intType))
											 assignments)
		in
		let size = List.length assignments in
		numberOfSTPCalls.(size) <- succ numberOfSTPCalls.(size);
		let result =
			match Stp.consult_stp pc theQuery Stp.FalseOrNot with
				| Stp.False ->
						contradictionCache := PcAssn.add (pc,assignments) !contradictionCache; (* Memoize the answer *)
						false (* Must be false *)
				| _ -> true (* Might be true *)
		in
		Output.set_mode Output.MSG_MUSTPRINT;
		result

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
						(fun (var,value) ->
							 Printf.sprintf "%s=%d,"
								 (To_string.humanReadableBytes !bytesToVars var)
								 value)
						assignments))
			(LineSet.cardinal result);
		LineSet.iter (fun (file,line) -> Format.printf "%s:%d\n" file line) result
	);
	result

let findDependenciesAndUpdate pcsAndLines linesToExplain localBytesToVars =
	Format.printf "\nConsidering\n%s\n"
		(String.concat ", " (List.map (fun (_,var) -> var.vname) localBytesToVars));
	let linesControlledByVars =
		bigUnion
			(List.rev_map
				 (fun assignments ->
						linesControlledBy pcsAndLines assignments)
				 (allPossibleValues localBytesToVars))
	in
	(* Remove the lines that have been accounted for from linesToExplain. *)
	LineSet.diff linesToExplain linesControlledByVars

exception EverythingCovered

let calculateDeps pcsAndLines =
	let firstResult,restResults =
		match pcsAndLines with
			| hd::tl -> hd,tl
			| _ -> failwith "No coverage information"
	in
	let (alwaysExecuted,everExecuted) =
		List.fold_left
			(fun (interAcc,unionAcc) (_,cov) ->
				 LineSet.inter interAcc cov, LineSet.union unionAcc cov)
			(snd firstResult, snd firstResult)
			restResults
	in

Format.printf "%d path conditions\n" (List.length pcsAndLines);

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
	Format.printf "These %d variables are mentioned in the path conditions:\n"
		(List.length !bytesToVars);
	List.iter (fun (_,var) -> Format.printf "%s\n" var.vname) !bytesToVars;

	Format.printf "\n%d lines always executed:\n" (LineSet.cardinal alwaysExecuted);
	LineSet.iter
		(fun (file,line) -> Format.printf "%s:%d\n" file line)
		alwaysExecuted;

	(* Remove lines that were always executed *)
	let remainingPcsAndLines = ref
		(List.rev_map
			 (fun (pc,lines) -> (pc, LineSet.diff lines alwaysExecuted))
			 pcsAndLines)
	and remainingLines = ref (LineSet.diff everExecuted alwaysExecuted) in

	(try
		 let size = ref 1 in
		 while true do
			 Format.printf "\n%d lines left\n" (LineSet.cardinal !remainingLines);
			 LineSet.iter
				 (fun (file,lineNum) -> Format.printf "%s:%d\n" file lineNum)
				 !remainingLines;
			 if LineSet.is_empty !remainingLines then raise EverythingCovered;
			 let linesStillUncovered =
				 List.fold_left
					 (fun lines b2v -> findDependenciesAndUpdate !remainingPcsAndLines lines b2v)
					 !remainingLines
					 (subsetsOfSize !size !bytesToVars)
			 in
			 let linesJustCovered = LineSet.diff !remainingLines linesStillUncovered in
			 (* Update the pcsAndLines only after all subsets of a given
					size have been examined. This enables us to see if there are
					several ways of hitting guaranteeing coverage of the same
					lines. *)
			 remainingPcsAndLines :=
				 List.rev_map
					 (fun (pc,lines) -> (pc, LineSet.diff lines linesJustCovered))
					 !remainingPcsAndLines;
			 remainingLines := linesStillUncovered;
			 incr size
		 done
	 with EverythingCovered -> ()
	);

	print_endline "numberOfHits";
	for i = 1 to 4 do Format.printf "Size %d: %d\n" i numberOfHits.(i) done;
	print_endline "numberOfMisses";
	for i = 1 to 4 do Format.printf "Size %d: %d\n" i numberOfMisses.(i) done;
	print_endline "numberOfSTPCalls";
	for i = 1 to 4 do Format.printf "Size %d: %d\n" i numberOfSTPCalls.(i) done;
	Format.printf "Optimization total time: %.2f s
  It took %.2f s to construct the formulas for the expressions inside 'if(...)'s,
  %.2f s to construct and assert the path conditions,
  and %.2f s to solve the resulting formulas.\n\n"
		(Stats.lookupTime "knownContradiction")
		(Stats.lookupTime "convert conditional")
		(Stats.lookupTime "STP assert")
		(Stats.lookupTime "STP query");
	()

let treeOfPcs = ref (Node (rootNodeData, []))

let readInDataAndGo _ = (* This needs to take a Cil.file, but I don't use it *)
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
		calculateDeps (getLeavesWithCoverage !treeOfPcs)
;;

(* Get rid of the __ASSUMEs from a path condition. The arguments are
	 the path_condition and path_condition_tracked fields of a state
	 structure. *)
let rec eliminate_untracked apc apct =
  match apc,apct with 
    | [],[]->([],[])
    | apch::apct,apcth::apctt -> 
        let (apct1,apct2) = eliminate_untracked apct apctt in
        if apcth then (apch::apct1,apct2) else  (apct1,apch::apct2)
    | _,_ -> failwith "Impossible: path_condition and path_condition_tracked must be of equal length"

let addCoverageFromFile filename =
	initCIL();
	let inChan = open_in_bin filename in
	(* Read in the coverage data structure *)
	let jobResults = (Marshal.from_channel inChan : job_result list) in
	close_in inChan;
	if !bytesToVars = [] then bytesToVars := (List.hd jobResults).result_history.bytesToVars;
	(* Add each path condition, with its coverage, to the tree of path conditions *)
	List.iter
		(fun { result_state = state ; result_history = hist } ->
			 (* Make sure all bytesToVars are the same *)
			 if hist.bytesToVars <> !bytesToVars then failwith "Not all bytesToVars are equal";
			 incr totalNumberOfPcs;
			 (* Remove __ASSUMEs from the path condition *)
			 let pc = fst (eliminate_untracked
											 state.path_condition
											 state.path_condition_tracked)
			 in
			 treeOfPcs := add (List.map simplifyLogicalOps pc) hist.coveredLines !treeOfPcs)
		jobResults

let feature : Cil.featureDescr = {
  Cil.fd_enabled = ref false;
  Cil.fd_name = "calculateVarDeps";
  Cil.fd_description = "Calculate what lines depend on what symbolic variables.\n";
  Cil.fd_extraopt = [
		("--fileWithCovInfo",
		 Arg.String addCoverageFromFile,
		 "<filename> A file from which to read the coverage information\n");
		("--fileWithPossibleValues",
		 Arg.Set_string valuesFile,
		 "<filename> File from which to read the possible values for variables
\t\t\tFormat is \"varname val_1 val_2 ... val_k\", with one variable per line,
\t\t\tand the values must all be integers\n");
	];
  Cil.fd_doit = readInDataAndGo;
  Cil.fd_post_check = false;
}
