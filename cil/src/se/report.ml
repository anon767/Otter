open Types
open Cil
open Executeargs

type coverageType = Line | Block | Edge | Cond

let covTypeToStr = function
	| Line -> "lines"
	| Block -> "blocks"
	| Edge -> "edges"
	| Cond -> "conditions"

let getTotal = function
	| Line -> run_args.arg_num_lines
	| Block -> run_args.arg_num_blocks
	| Edge -> run_args.arg_num_edges
	| Cond -> run_args.arg_num_conds

let getNumCovered covType hist = match covType with
	| Line -> LineSet.cardinal hist.coveredLines
	| Block -> StmtInfoSet.cardinal hist.coveredBlocks
	| Edge -> EdgeSet.cardinal hist.coveredEdges
	| Cond -> CondSet.cardinal hist.coveredConds

(** Compute set cover greedily.
		[greedySetCover emptyCheck scoreFn setdiff setList universe]
		selects the element [x] from [setList] which gives the highest
		value for [scoreFn x universe], updates [universe] to be
		[setdiff universe x], removes [x] from [setList], and iterates
		until [emptyCheck universe].
		The result is the list of every [x] selected during this process. *)
let greedySetCover emptyCheck scoreFn setdiff setList universe =
	let rec helper acc setList remaining =
		if emptyCheck remaining then acc
		else (
			match setList with
				| [] -> failwith "Impossible to cover universe."
				| h::_ ->
						let nextSet,x =
							List.fold_left
								(fun prev set ->
									 let score = scoreFn set remaining in
									 if score > (snd prev)
									 then (set,score)
									 else prev)
								(h,min_int)
								setList
						in
						(* Add nextSet to acc, remove it from setList, and remove
							 its elements from universe. *)
						helper (nextSet::acc)
							(List.filter ((!=) nextSet) setList)
							(setdiff remaining nextSet)
		)
	in helper [] setList universe

let percentage numer denom = 100. *. float_of_int numer /. float_of_int denom

(* Given a path condition and a list of mappings from make_ByteArrays of
	 symbolic values to variables, print:
	 - the path condition (in terms of those variables, where possible)
	 - a sample set of values for those variables which would lead execution down this path
	 - coverage information for this path *)
let printPath state hist =

    let rec eliminate_untracked apc apct =
      match apc,apct with 
      | [],[]->([],[])
      | apch::apct,apcth::apctt -> 
          let (apct1,apct2) = eliminate_untracked apct apctt in
          if apcth then (apch::apct1,apct2) else  (apct1,apch::apct2)
      | _,_ -> failwith "Impossible: path_condition and path_condition_tracked must be of equal length"
    in

    let pc_all = state.path_condition in
    let (pc_branch,pc_assume) = eliminate_untracked (state.path_condition) (state.path_condition_tracked) in

	Output.printf "Path condition:\n%s\n\n"
		(To_string.humanReadablePc pc_branch hist.bytesToVars);
	Output.printf "Path condition (ASSUMEs):\n%s\n\n"
		(To_string.humanReadablePc pc_assume hist.bytesToVars);

	let mentionedSymbols = Stp.allSymbolsInList pc_branch in
	let valuesForSymbols = Stp.getValues pc_all (SymbolSet.elements mentionedSymbols) in

	(* Keep track of which symbols we haven't given values to.
		 This would happen if there are untracked symbolic values in the
		 path condition. *)
	let unboundSymbols = ref mentionedSymbols in

	(* Get the value of a symbolic make_ByteArray *)
	let getVal = function
		| Bytes_ByteArray bytArr ->
				let byteOptArray =
					ImmutableArray.map
						(function
							 | Byte_Symbolic s ->
									 (try
											let valueForS = List.assq s valuesForSymbols in
											(* Now s is bound *)
											unboundSymbols := SymbolSet.remove s !unboundSymbols;
											Some (make_Byte_Concrete valueForS)
										with Not_found -> None)
							 | _ -> failwith "Impossible: tracked symbolic value must be fully symbolic")
						bytArr
				in
				if ImmutableArray.exists (* Check if any byte is constrained *)
					(function Some _ -> true | _ -> false)
					byteOptArray
				then (
					(* Return a Some with the bytearray, filling in
						 unconstrained bytes with 0. *)
					Some (make_Bytes_ByteArray
									(ImmutableArray.map
										 (function Some b -> b | None -> MemOp.byte__zero)
										 byteOptArray))
				) else (
					(* Return None for a totally unconstrained value *)
					None
				)
		| _ -> failwith "Impossible: symbolic bytes must be a ByteArray"
	in

	Output.printf "Sample value:\n";
	List.iter
		(fun (bytes,varinf) ->
			 match getVal bytes with
				 | None -> () (* Don't print anything for an unconstrained value *)
				 | Some concreteByteArray ->
						 match Convert.bytes_to_constant concreteByteArray varinf.vtype with
							 | CInt64 (n,_,_) ->
									 (* Is it okay to ignore the type? Or might we have to truncate? *)
									 Output.printf "%s=%Ld\n" varinf.vname n
							 | _ -> failwith "Unimplemented: non-integer symbolic")
		hist.bytesToVars;

	(* Check to see if we've bound all of the symbols in the path condition *)
	if not (SymbolSet.is_empty !unboundSymbols)
	then (
		Output.printf "but these symbolic values are unaccounted for by tracked variables:\n";
		SymbolSet.iter
			(fun s -> Output.printf "%d " s.symbol_id)
			!unboundSymbols
	);
	Output.printf "\n"

let printCov covType hist =
	let total = getTotal covType
	and numCovered = getNumCovered covType hist
	in
	Output.printf "%d out of %d %s (%.2f%%)\n\n"
		numCovered total (covTypeToStr covType) (percentage numCovered total)

let printLines lineset =
	Output.printf "The lines hit were:\n";
	LineSet.iter
		(fun (file,lineNum) -> Output.printf "%s:%d\n" file lineNum)
		lineset;
	Output.printf "\n"

let printEdges edgeset =
	Output.printf "The edges hit were:\n";
	EdgeSet.iter
		(fun (srcStmtInfo,destStmtInfo) ->
			 Output.printf "%s -> %s\n"
				 (To_string.stmtInfo srcStmtInfo)
				 (To_string.stmtInfo destStmtInfo))
		edgeset;
	Output.printf "\n"
	
let printBlocks blockset =
	Output.printf "The blocks hit were:\n";
	StmtInfoSet.iter
	  (fun stmtInfo -> Output.printf "%s\n" (To_string.stmtInfo stmtInfo))
		blockset;
	Output.printf "\n"
	
let printConditions condset =
	Output.printf "The conditions hit were:\n";
	CondSet.iter
	  (fun (stmtInfo, truth) -> Output.printf "%s %c\n" (To_string.stmtInfo stmtInfo) (if truth then 'T' else 'F'))
		condset;
	Output.printf "\n"
	
let printCoveringConfigs coveringSet covType =
	let name = covTypeToStr covType in
	if coveringSet = [] then Output.printf "No constraints: any run covers all %s\n" name
	else begin
		Output.printf "Here is a set of %d configurations which covers all the %s ever hit:\n\n"
				(List.length coveringSet) name;
		List.iter
        (fun { result_state=state; result_history=hist} ->
				 printPath state hist;
				 printCov covType hist;
				 (match covType with
							Line -> printLines hist.coveredLines
						| Block -> printBlocks hist.coveredBlocks
						| Edge -> printEdges hist.coveredEdges
						| Cond -> printConditions hist.coveredConds);
				 Output.printf "-------------\n\n")
			 coveringSet
	end

let printCoverageInfo resultList =
	if run_args.arg_line_coverage then (
		Output.printf "Line coverage:\n\n";
		let allLinesCovered =
 			(List.fold_left
				 (fun acc { result_history=hist } ->
						LineSet.union acc hist.coveredLines)
				 LineSet.empty
				 resultList)
		in
		printCov Line { emptyHistory with coveredLines = allLinesCovered; };
		let coveringSet = greedySetCover
			LineSet.is_empty
			(fun job remaining ->
				 LineSet.cardinal (LineSet.inter job.result_history.coveredLines remaining))
			(fun remaining job -> LineSet.diff remaining job.result_history.coveredLines)
			resultList
			allLinesCovered
		in
		printCoveringConfigs coveringSet Line
	);

	if run_args.arg_block_coverage then (
		Output.printf "Block coverage:\n\n";
		let allBlocksCovered =
 			(List.fold_left
				 (fun acc { result_history=hist } ->
						StmtInfoSet.union acc hist.coveredBlocks)
				 StmtInfoSet.empty
				 resultList)
		in
		printCov Block { emptyHistory with coveredBlocks = allBlocksCovered; };
		let coveringSet = greedySetCover
			StmtInfoSet.is_empty
			(fun job remaining ->
				 StmtInfoSet.cardinal (StmtInfoSet.inter job.result_history.coveredBlocks remaining))
			(fun remaining job -> StmtInfoSet.diff remaining job.result_history.coveredBlocks)
			resultList
			allBlocksCovered
		in
		printCoveringConfigs coveringSet Block
	);

	if run_args.arg_edge_coverage then (
		Output.printf "Edge coverage:\n\n";
		let allEdgesCovered =
 			(List.fold_left
				 (fun acc { result_history=hist } ->
						EdgeSet.union acc hist.coveredEdges)
				 EdgeSet.empty
				 resultList)
		in
		printCov Edge { emptyHistory with coveredEdges = allEdgesCovered; };
		let coveringSet = greedySetCover
			EdgeSet.is_empty
			(fun job remaining ->
				 EdgeSet.cardinal (EdgeSet.inter job.result_history.coveredEdges remaining))
			(fun remaining job -> EdgeSet.diff remaining job.result_history.coveredEdges)
			resultList
			allEdgesCovered
		in
		printCoveringConfigs coveringSet Edge
	);

  if run_args.arg_cond_coverage then (
		Output.printf "Condition coverage:\n\n";
		let allCondsCovered =
 			(List.fold_left
				 (fun acc { result_history=hist } ->
						CondSet.union acc hist.coveredConds)
				 CondSet.empty
				 resultList)
		in
		printCov Cond { emptyHistory with coveredConds = allCondsCovered; };
		let coveringSet = greedySetCover
			CondSet.is_empty
			(fun job remaining ->
				 CondSet.cardinal (CondSet.inter job.result_history.coveredConds remaining))
			(fun remaining job -> CondSet.diff remaining job.result_history.coveredConds)
			resultList
			allCondsCovered
		in
		printCoveringConfigs coveringSet Cond
	)


let print_report results =
(*
	if run_args.arg_edge_coverage then
		begin
			Output.print_endline "Edge coverage:";
			let hashtblAsList =
				Hashtbl.fold
					(fun a b acc -> (a,b) :: acc)
					branches_taken
					[]
			and cmpByLoc ((_,loc1),_) ((_,loc2),_) = compareLoc loc1 loc2
			in
			let sortedList = List.sort cmpByLoc hashtblAsList and
					printPcHistSet pcHistSet =
						let counter = ref 0 in
						PcHistSet.iter
							(fun (pc,hist) ->
								let str = To_string.humanReadablePc pc hist.bytesToVars in
								counter := !counter + 1;
								Output.print_endline ("Condition " ^ (string_of_int !counter) ^ ":");
								Output.print_endline (if str = "" then "true" else str);
								Output.print_newline ())
							pcHistSet
			in
			List.iter
				(fun ((exp,loc), (true_pcHistSet_ref,false_pcHistSet_ref)) ->
					Output.print_endline ((To_string.location loc) ^ ", " ^ (To_string.exp exp));
					if not (PcHistSet.is_empty !true_pcHistSet_ref) then
						(Output.print_endline "True branch taken under the following conditions:";
						 printPcHistSet !true_pcHistSet_ref);
					if not (PcHistSet.is_empty !false_pcHistSet_ref) then
						(Output.print_endline "False branch taken under the following conditions:";
						 printPcHistSet !false_pcHistSet_ref);
					Output.print_newline ())
				sortedList
		end;
*)
	let coverage, completed, truncated, abandoned =
		List.fold_left begin fun (coverage, completed, truncated, abandoned) result ->
			match result with
				| Types.Return (_, c)
				| Types.Exit (_, c)      -> (c::coverage, completed + 1, truncated, abandoned)
				| Types.Truncated (c, d) -> (c::d::coverage, completed, truncated + 2, abandoned)
				| Types.Abandoned _      -> (coverage, completed, truncated, abandoned + 1)
		end ([], 0, 0, 0) results in
	if completed = 0 then (
		Output.printf "All %d paths had errors.\n" abandoned
			(* Program execution ends. *)
	) else (
	(* If there were successful runs *)
	Output.printf "%d paths ran to completion; %d had errors.\n" completed abandoned;
	Output.printf "There are %d truncated paths.\n" truncated;

	if run_args.arg_line_coverage || run_args.arg_block_coverage || run_args.arg_edge_coverage || run_args.arg_cond_coverage
	then begin
		(* Print coverage information, if it was gathered, regardless of anything else.*)
		print_args.arg_print_nothing <- false;
		Output.set_mode Output.MSG_MUSTPRINT;
		printCoverageInfo coverage;
(*
	let (alwaysExecuted,everExecuted) =
		match coverage with
			|  hd::tl ->
					 let first_edges = hd.result_history.coveredEdges in
					 List.fold_left
						 (fun (interAcc,unionAcc) { result_history=hist } ->
								EdgeSet.inter interAcc hist.coveredEdges,
								EdgeSet.union unionAcc hist.coveredEdges)
						 (first_edges, first_edges)
						 tl
			| [] -> assert false (* there has to be at least one execution *)
	in

	Output.printf "\nIn all, %d edges were executed, of which %d were always executed.\n"
		(EdgeSet.cardinal everExecuted) (EdgeSet.cardinal alwaysExecuted);
*)

		if run_args.arg_calculate_dependencies then (
			CalculateDependencies.calculateDeps coverage
		);

		Output.printf "Finished.\n";

		(* Marshal out (coverage : Types.job_result list) so that we can
			 read it back in later. *)
		if run_args.arg_marshal_file <> ""
		then (
			Output.printf "Coverage data is in %s.\n" run_args.arg_marshal_file;
			let outChan = open_out_bin run_args.arg_marshal_file in
			Marshal.to_channel outChan coverage [];
			close_out outChan
		)

	end
	)
;;
