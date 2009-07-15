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
								 (fun tailAssignment -> makeEquality bytes value :: tailAssignment)
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

let rec isConsistentWithList pc = function
		[] -> true (* The pc is consistent with the set of assignments *)
	| assignment::assignments ->
			if Stp.consult_stp pc assignment = Stp.False
			then false
			else isConsistentWithList (assignment::pc) assignments

let consistentPcsAndLines treeOfPcs assignments =
	let rec helper pcCov = function
			Node ((condition,cov),children) ->
				let newPcCov = (condition :: fst pcCov, LineSet.union cov (snd pcCov)) in
				if isConsistentWithList (fst newPcCov) assignments
				then (
					(* We only want to return the leaf nodes *)
					if children = []
					then [newPcCov]
					else List.concat (List.map (fun child -> helper newPcCov child) children)
				) else (
					[]
				)
	in
  match treeOfPcs with
			(* Skip the root node, which is just a placeholder *)
			Node (x,children) when x == rootNodeData ->
				List.concat
					(List.map
						 (fun child -> helper ([],LineSet.empty) child)
						 children)
		| _ -> failwith "Bad root node"

let bigUnionForSymbols = List.fold_left SymbolSet.union SymbolSet.empty
let bigUnion = List.fold_left LineSet.union LineSet.empty
let bigInter setList =
	match setList with
		| [] -> failwith "Taking intersection of no sets"
		| h::t -> List.fold_left LineSet.inter h t

module CoverageMap = Map.Make
	(struct
		 type t = bytes list
		 let compare : t -> t -> int = compare
	 end)
let coverageMap = ref CoverageMap.empty

(*
(* This assumes both sub and super are sorted (according to cmp) *)
let rec isSubset ?(cmp=Pervasives.compare) (sub:bytes list) super =
	match sub,super with
		| [],_ -> true
		| _,[] -> false
		| h1::t1,h2::t2 ->
				match cmp h1 h2 with
					| 0 -> isSubset ~cmp t1 t2
					| n when n < 0 -> false (* Because the lists are sorted *)
					| _ -> isSubset ~cmp sub t2
let notCoveredByASubset assignments resultIn =
	CoverageMap.fold
		(fun subassignments coveredLines resultOut ->
			 if isSubset subassignments assignments
			 then LineSet.diff resultOut coveredLines
			 else resultOut)
		!coverageMap
		resultIn
*)

exception Break
let notCoveredByASubset assignments resultIn =
	let result = ref resultIn in
	(try
		 for i = 0 to pred (List.length assignments) (*downto 0*) do
			 let subsets = subsetsOfSize i assignments in
			 List.iter
				 (fun subset ->
						try
							let covered = CoverageMap.find subset !coverageMap in
							result := LineSet.diff !result covered;
							if LineSet.is_empty !result
							then raise Break
						with Not_found -> ())
				 subsets
		 done
	 with Break -> ());
	!result

let linesControlledBy pcsAndLines assignments =
	let result =
		match List.map snd (consistentPcsAndLines pcsAndLines assignments) with
			| [] -> failwith "Impossible: assignments not compatible with any pc"
			| x -> bigInter x
	in
(*	let assignments = List.sort Pervasives.compare assignments in*)
	let result = Stats.time "filter subsets" (notCoveredByASubset assignments) result in
	if not (LineSet.is_empty result) then (
		coverageMap := CoverageMap.add assignments result !coverageMap;
		Output.set_mode Output.MSG_REG;
		Format.printf "\nUnder the condition\n";
		List.iter
			(fun assignment ->
				 Format.printf "%s\n"
				 (Str.replace_first
						(Str.regexp "==(\\(.*\\),Bytes(\\([^)]*\\)))")
						"\\1=\\2"
						(To_string.humanReadableBytes !bytesToVars assignment)))
			assignments;
		Format.printf "these %d lines are hit\n" (LineSet.cardinal result);
		LineSet.iter (fun (file,line) -> Format.printf "%s:%d\n" file line) result
	);
	result

let getControlledLines pcsAndLines configs =
	bigUnion
		(List.rev_map
			 (fun config -> linesControlledBy pcsAndLines config)
			 configs)

let accumulateControlledLines pcsAndLines controlledLines localBytesToVars =
	Format.printf "\nConsidering\n%s\n"
		(String.concat ", " (List.map (fun (_,var) -> var.vname) localBytesToVars));
	LineSet.union
		controlledLines
		(getControlledLines pcsAndLines (allPossibleValues localBytesToVars))

let allLinesInFile filename =
	let inChan = open_in filename in
	let allLines = ref LineSet.empty in
	(try
		 while true do
			 match Str.split (Str.regexp ":") (input_line inChan) with
					 [var;value] -> allLines := LineSet.add (var,int_of_string value) !allLines
				 | _ -> failwith "Badly formatted ignoreLines file"
		 done
	 with End_of_file -> ()
	);
	!allLines

exception EverythingCovered

let calculateDeps treeOfPcs linesToIgnoreFile configsToTry =
	let pcsAndLines = getLeavesWithCoverage treeOfPcs in
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
	Format.printf "(instead of %d)\n" !totalNumberOfPcs;

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

	coverageMap := CoverageMap.add [] alwaysExecuted !coverageMap;
	Format.printf "\n%d lines always executed:\n" (LineSet.cardinal alwaysExecuted);
	LineSet.iter
		(fun (file,line) -> Format.printf "%s:%d\n" file line)
		alwaysExecuted;

	let linesToIgnore =
		if linesToIgnoreFile = ""
		then LineSet.empty
		else allLinesInFile linesToIgnoreFile
	in

	(* Remove lines that were always executed *)
	let remainingLines = ref
		(LineSet.diff (LineSet.diff everExecuted alwaysExecuted) linesToIgnore)
	in

	let size = ref 0 in
	(try
		 while true do
			 Format.printf "\n%d lines left\n" (LineSet.cardinal !remainingLines);
			 LineSet.iter
				 (fun (file,lineNum) -> Format.printf "%s:%d\n" file lineNum)
				 !remainingLines;
			 if LineSet.is_empty !remainingLines then raise EverythingCovered;
			 incr size;
			 let linesJustCovered =
				 match configsToTry with
						 None ->
							 List.fold_left
								 (fun lines b2v ->
										accumulateControlledLines treeOfPcs lines b2v)
								 LineSet.empty
								 (subsetsOfSize !size !bytesToVars)
					 | Some configs ->
							 getControlledLines
								 treeOfPcs
								 (List.filter (fun config -> List.length config = !size) configs)
			 in
			 remainingLines := LineSet.diff !remainingLines linesJustCovered
		 done
	 with EverythingCovered -> ()
	);

	Format.printf "The cache was hit %d times and missed %d times. (STP was called %d times.)
  It took %.2f s to construct the formulas for the expressions inside 'if(...)'s,
  %.2f s to construct and assert the path conditions,
  and %.2f s to solve the resulting formulas.
  Time for filtering subsets: %.2f s.\n\n"
		!Stp.cacheHits !Stp.cacheMisses !stp_count
		(Stats.lookupTime "convert conditional")
		(Stats.lookupTime "STP assert")
		(Stats.lookupTime "STP query")
		(Stats.lookupTime "filter subsets");
	()

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

let treeOfPcs = ref (Node (rootNodeData, []))

let addCoverageFromFile filename =
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

let readInValues valuesFile =
	let inChan = open_in valuesFile in
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
		close_in inChan

let configsToTryFile = ref ""

(* This requires bytesToVars to have already been set (by
	 addCoverageFromFile) *)
let varNameToBytes str =
	fst (List.find (fun (_,varinfo) -> varinfo.vname = str) !bytesToVars)
let readInConfigsToTry configsFile =
	let inChan = open_in configsFile in
	let configs = ref [] in
	(try
		 (* Read through entire file *)
		 while true do
			 let line = ref (input_line inChan)
			 and config = ref [] in
			 (* Read one configuration *)
			 try
				 while !line <> "" do
					 (match Str.split (Str.regexp "=") !line with
								[var;value] ->
									config :=
										makeEquality (varNameToBytes var) (int_of_string value) :: !config
							| _ -> failwith "Badly formatted configuration"
					 );
					 line := input_line inChan
				 done;
				 if !config <> [] then configs := !config :: !configs
			 with End_of_file ->
				 (* Accept final config if file doesn't end with a blank line *)
				 if !config <> [] then configs := !config :: !configs;
				 raise End_of_file
		 done
	 with End_of_file ->
		 close_in inChan
	);
	List.rev !configs

let linesToIgnoreFile = ref ""

let speclist = [
	("--fileWithPossibleValues",
	 Arg.String readInValues,
	 "<filename> File from which to read the possible values for variables
\t\t\tFormat is \"varname val_1 val_2 ... val_k\", with one variable per line,
\t\t\tand the values must all be integers\n");
	("--ignoreLines",
	 Arg.Set_string linesToIgnoreFile,
	 "<filename> A file listing which lines to ignore when computing guaranteed coverage");
	("--configsToTry",
	 Arg.Set_string configsToTryFile,
	 "<filename> Only check configurations from this file for guaranteed coverage.
\t\t\t(If this option is not set, do a full search.)");
]
;;

let usageMsg =
	"Usage: guaranteedCoverage [options] coverage-file[...]

Options can come before, after, or interspersed with coverage files.
You must specify either a possibleValues file or a configsToTry file.
"

let main () =
	initCIL();
	Arg.parse
		(Arg.align speclist)
		addCoverageFromFile (* Unnamed arguments are treated as coverage files *)
		usageMsg;
	Stats.reset Stats.HardwareIfAvail; (* Enable timing *)
	let configsToTry =
		if !configsToTryFile = ""
		then None
		else Some (readInConfigsToTry !configsToTryFile)
	in
	if Hashtbl.length varToVals = 0 && !configsToTryFile = ""
	then (print_endline usageMsg; exit(1));
	calculateDeps !treeOfPcs !linesToIgnoreFile configsToTry
;;

main ()
