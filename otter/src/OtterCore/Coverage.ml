open DataStructures
open OcamlUtilities
open Cil
open CilUtilities
open OtterBytes
open OtterCFG
open Bytes
open State
open Job

let coverage_totals : (Cil.file, <lines : LineSet.t; blocks : StmtInfoSet.t; edges : EdgeSet.t; conds : CondSet.t>) Hashtbl.t = Hashtbl.create 0

module FundecMap = Map.Make (struct
	type t = Cil.fundec
	let compare a b = let id x = x.svar.vid in Pervasives.compare (id a) (id b)
end)

module VarinfoSet = Set.Make (struct
	type t = Cil.varinfo
	let compare a b = Pervasives.compare a.Cil.vid b.Cil.vid
end)

let reachable_functions: (fundec list) FundecMap.t ref = ref FundecMap.empty
let reachable_globals: VarinfoSet.t ref = ref VarinfoSet.empty

(* Implicit definition of basic block *)

(* A statement ends a basic block if
	 (1) it is followed by a join point,
	 (2) it is a goto,
	 (3) it contains a function call, or
	 (4) it has no successor (a Return, or a call to a __noreturn__ function), or more than 1 (an If). *)
let isJoinPoint stmt = match stmt.preds with
		 _::_::_ -> true
	 | _ -> false

let rec stmtAtEndOfBlock stmt = match stmt.succs with
		[succ] when isJoinPoint succ -> stmt (* (1) *)
	| [succ] ->
			(match stmt.skind with
					 Goto _ -> stmt (* (2) *)
				 | Instr il when List.exists (function Call _ -> true | _ -> false) il -> stmt (* (3) *)
				 | _ -> stmtAtEndOfBlock succ)
	| _ -> stmt (* (4) *)

class getStatsVisitor file = object (self)
	val lines = ref LineSet.empty
	val blocks = ref StmtInfoSet.empty
	val edges = ref EdgeSet.empty
	val conds = ref CondSet.empty

	val currFunc = ref Cil.dummyFunDec

	method private stmtInfo_of_stmt stmt =
		{ siFuncName = (!currFunc).svar.vname ; siStmt = stmt }

    val reachable_stmts = Hashtbl.create 0
    method private is_reachable_stmt stmt =
        if Hashtbl.length reachable_stmts = 0 then
            (* Prepare the table *)
            let source =
                let mainFunc = ProgramPoints.get_main_fundec file in 
                Instruction.of_fundec file mainFunc
            in
            let processed_instrs = Hashtbl.create 0 in
            let rec visit instruction =
                if Hashtbl.mem processed_instrs instruction then
                    ()
                else (
                    Hashtbl.add processed_instrs instruction true;
                    Hashtbl.add reachable_stmts instruction.Instruction.stmt true;
                    List.iter visit (Instruction.successors instruction);
                    List.iter visit (Instruction.call_sites instruction);
                    ()
                )
            in
            visit source
        else ()
        ;
        try
            Hashtbl.find reachable_stmts stmt
        with Not_found -> false


	method lines = !lines
	method blocks = !blocks
	method edges = !edges
	method conds = !conds

	inherit nopCilVisitor

	method vinst instr =
		let loc = get_instrLoc instr in
		lines := LineSet.add (loc.Cil.file,loc.Cil.line) !lines;
		SkipChildren (* There's nothing interesting under an [instr] *)

	method vstmt stmt =
        if not !Executeargs.arg_cfg_pruning || self#is_reachable_stmt stmt then (
		    let stmtInfo = self#stmtInfo_of_stmt stmt in
		    (* Gather lines and conditions. *)
		    (match stmt.skind with
		    		 If(_,_,_,loc) ->
		    				conds := CondSet.add (stmtInfo,true) (CondSet.add (stmtInfo,false) !conds);
		    				lines := LineSet.add (loc.Cil.file,loc.Cil.line) !lines;
		    	 | Cil.Return(_,loc)
		    	 | Goto(_,loc)
		    	 | Loop (_,loc,_,_) ->
		    			 lines := LineSet.add (loc.Cil.file,loc.Cil.line) !lines;
		    	 | _ -> ()
		    );
		    (* We represent basic blocks by their final statements, so if stmt
		    	 is the end of a basic block, add it to the set of basic blocks
		    	 and add its outgoing edges to the set of edges. (Edges are
		    	 represented as endOfBlock1 -> endOfBlock2.) *)
		    if stmt == stmtAtEndOfBlock stmt
		    then (
		    	blocks := StmtInfoSet.add stmtInfo !blocks;
		    	List.iter
		    		(fun succ -> edges := EdgeSet.add (stmtInfo, self#stmtInfo_of_stmt (stmtAtEndOfBlock succ)) !edges)
		    		stmt.succs
		    )
        );
		DoChildren (* There could be stmts or instrs inside, which we should visit *)

	method vfunc fundec = currFunc := fundec; DoChildren
end

class getCallerVisitor file = object (self)
	val callee_list : fundec list ref= ref []
	val varinfo_set : VarinfoSet.t ref= ref VarinfoSet.empty

	val currFuncName = ref ""

	method callee_list = !callee_list
	method varinfo_set = !varinfo_set

	inherit nopCilVisitor

	method vlval lval = (match lval with
      | (Var(varinfo),_) -> (
          match varinfo.vtype with
            | TFun _ -> (try callee_list := (FindCil.fundec_by_varinfo file varinfo)::(!callee_list) with Not_found -> ())
            | _ -> if varinfo.vglob then varinfo_set := VarinfoSet.add varinfo (!varinfo_set) else ()
        )
      | _ -> ()
    );
		SkipChildren

end
class getGlobalInitVisitor = object (self)
	val varinfo_set : VarinfoSet.t ref= ref VarinfoSet.empty
	method varinfo_set = !varinfo_set

	inherit nopCilVisitor

	method vlval lval =
      (match lval with
      | (Var(varinfo),_) -> varinfo_set := VarinfoSet.add varinfo (!varinfo_set)
      | _ -> ()
    );
		DoChildren

end


(** Print the name and type of a {!State.stmtInfo}.
		@param ff is the formatter to which to print
		@param fn is the {!State.stmtInfo} to print
*)
let printStmtInfo ff si =
	Format.fprintf ff "%s %d" si.siFuncName si.siStmt.Cil.sid;
	if !Executeargs.arg_print_stmtInfo_locs then
		Format.fprintf ff " (%a)" Printcil.loc (Cil.get_stmtLoc si.siStmt.Cil.skind)

let prepare_file file =
    let trackedFns = TrackingFunctions.trackedFns file in
    let vis = new getStatsVisitor file in
	iterGlobals
		file
		(function (* Visit the bodies of the functions we care about *)
				 GFun(fundec,_) ->
					 if TrackingFunctions.isTracked fundec.svar.vname trackedFns
					 then ignore (visitCilFunction (vis:>cilVisitor) fundec)
			 | _ -> ()
		);
	(* Find all lines, blocks, edges, and conditions. *)
	(* TODO: wrap the listings of Lines,Edges,etc... *)
	let totals = object
		method lines = vis#lines
		method blocks = vis#blocks
		method edges = vis#edges
		method conds = vis#conds
	end in
	Hashtbl.add coverage_totals file totals;

	if !Executeargs.arg_list_lines then begin
		Output.printf "Total number of %s: %d@\n" "Lines" (LineSet.cardinal totals#lines);
		LineSet.iter
			(fun (file, lineNum) -> Output.printf "%s:%d@\n" file lineNum)
			vis#lines;
		Output.printf "@."
	end;
	if !Executeargs.arg_list_blocks then begin
		Output.printf "Total number of %s: %d@\n" "Blocks" (StmtInfoSet.cardinal totals#blocks);
		StmtInfoSet.iter
			(fun stmtInfo -> Output.printf "@[%a@]@\n" printStmtInfo stmtInfo)
			vis#blocks;
		Output.printf "@."
	end;
	if !Executeargs.arg_list_edges then begin
		Output.printf "Total number of %s: %d@\n" "Edges" (EdgeSet.cardinal totals#edges);
		EdgeSet.iter
			(fun (srcStmtInfo, destStmtInfo) ->
				 Output.printf "@[@[%a@]@ -> @[%a@]@]@\n"
					 printStmtInfo srcStmtInfo
					 printStmtInfo destStmtInfo)
			vis#edges;
		Output.printf "@."
	end;
	if !Executeargs.arg_list_conds then begin
		Output.printf "Total number of %s: %d@\n" "Conditions" (CondSet.cardinal totals#conds);
		CondSet.iter
			(fun (stmtInfo, truth) -> Output.printf "@[%a@] %c@\n" printStmtInfo stmtInfo (if truth then 'T' else 'F'))
		vis#conds;
		Output.printf "@."
	end


(* TODO: a global can be reachable by another global init! *)
let computeReachableCode file =
  (* compute reachable globals from main *)
  let main_func = ProgramPoints.get_main_fundec file in
  let rec computeReachableCodeThroughFunCall queue =
    if List.length queue = 0 then () else
      let vis = new getCallerVisitor file in
      let fn = List.hd queue in
      let queue = List.tl queue in
      let fnMap = (!reachable_functions) in
        if FundecMap.mem fn fnMap then computeReachableCodeThroughFunCall queue else
        (
          ignore (visitCilFunction (vis:>cilVisitor) fn);
          reachable_functions := FundecMap.add fn (vis#callee_list) fnMap;
          reachable_globals := VarinfoSet.union (vis#varinfo_set) (!reachable_globals);
          let queue' = List.append queue (vis#callee_list) in
            computeReachableCodeThroughFunCall queue'
        )
  in
  let rec computeReachableCodeThroughGlobalInit globals =
    match globals with
      | [] -> ()
      | g::globals ->
          (match (FindCil.global_varinit_by_varinfo file g).init with
             | None -> computeReachableCodeThroughGlobalInit globals
             | Some init ->
                let vis = new getGlobalInitVisitor in
                ignore (visitCilInit (vis:>cilVisitor) g NoOffset init);
                reachable_globals := VarinfoSet.union (vis#varinfo_set) (!reachable_globals);
                computeReachableCodeThroughGlobalInit (globals@(VarinfoSet.elements (vis#varinfo_set)))
          )
  in
    computeReachableCodeThroughFunCall [main_func];
    computeReachableCodeThroughGlobalInit (VarinfoSet.elements (!reachable_globals))

type coverageType = Line | Block | Edge | Cond | Path

let covTypeToStr = function
	| Line -> "lines"
	| Block -> "blocks"
	| Edge -> "edges"
	| Cond -> "conditions"
	| Path -> "paths"

let getTotal file = function
	| Line  -> LineSet.cardinal     (Hashtbl.find coverage_totals file)#lines
	| Block -> StmtInfoSet.cardinal (Hashtbl.find coverage_totals file)#blocks
	| Edge  -> EdgeSet.cardinal     (Hashtbl.find coverage_totals file)#edges
	| Cond  -> CondSet.cardinal     (Hashtbl.find coverage_totals file)#conds
	| Path  -> invalid_arg "Cannot compute the total number of paths"

let getNumCovered covType hist = match covType with
	| Line -> LineSet.cardinal hist.coveredLines
	| Block -> StmtInfoSet.cardinal hist.coveredBlocks
	| Edge -> EdgeSet.cardinal hist.coveredEdges
	| Cond -> CondSet.cardinal hist.coveredConds
	| Path -> invalid_arg "Should not compute number of paths covered"

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

	Output.printf "Path condition:@\n  @[%a@]@\n@."
		(FormatPlus.pp_print_list (BytesPrinter.bytes_named hist.bytesToVars) "@\n") pc_branch;
	Output.printf "Path condition (ASSUMEs):@\n  @[%a@]@\n@."
		(FormatPlus.pp_print_list (BytesPrinter.bytes_named hist.bytesToVars) "@\n") pc_assume;

	let mentionedSymbols = Stp.allSymbolsInList pc_branch in
	let valuesForSymbols = Stp.getValues pc_all (Stp.SymbolSet.elements mentionedSymbols) in

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
											unboundSymbols := Stp.SymbolSet.remove s !unboundSymbols;
											Some (make_Byte_Concrete valueForS)
										with Not_found -> None)
							 | _ -> failwith "Impossible: tracked symbolic value must be fully symbolic")
						bytArr
				in
				if ImmutableArray.exists (* Check if any byte is constrained *)
					(function Some _ -> true | _ -> false)
					byteOptArray
				then (
					(* Fill in unconstrained bytes with 0, and return a Some with that bytearray. *)
					Some (ImmutableArray.map (function Some b -> b | None -> byte__zero) byteOptArray)
				) else (
					(* Return None for a totally unconstrained value *)
					None
				)
		| _ -> failwith "Impossible: symbolic bytes must be a ByteArray"
	in

    Output.printf "@[Sample value:@\n";
    List.iter
        (fun (bytes,varinf) ->
            match getVal bytes with
              | None -> () (* Don't print anything for an unconstrained value *)
              | Some concreteByteArray ->
                    match varinf.vtype with
                      | TArray _ ->
                            Output.printf "%s@,=@[%a@]@\n" varinf.vname BytesPrinter.bytearray concreteByteArray
                      | _ ->
                            match bytes_to_constant (make_Bytes_ByteArray concreteByteArray) varinf.vtype with
                              | CInt64 (n,_,_) ->
                                    (* Is it okay to ignore the type? Or might we have to truncate? *)
                                    Output.printf "%s@,=%Ld@\n" varinf.vname n
                              | _ -> failwith "Unimplemented: non-integer symbolic")
        (List.rev hist.bytesToVars);

	(* Check to see if we've bound all of the symbols in the path condition *)
	if not (Stp.SymbolSet.is_empty !unboundSymbols)
	then (
		Output.printf "but these symbolic values are unaccounted for by tracked variables:@\n";
		Stp.SymbolSet.iter
			(fun s -> Output.printf "%d " s.symbol_id)
			!unboundSymbols
	);
	Output.printf "@]@."

let printLine ff (file,lineNum) = Format.fprintf ff "%s:%d" file lineNum
let printEdge ff (srcStmtInfo,destStmtInfo) = Format.fprintf ff "%a -> %a" printStmtInfo srcStmtInfo printStmtInfo destStmtInfo
let printBlock = printStmtInfo
let printCondition ff (stmtInfo, truth) = Format.fprintf ff "%a %c" printStmtInfo stmtInfo (if truth then 'T' else 'F')

let printTimedEntity printEntity ff (entity, time) = Format.fprintf ff "[%.2f] %a" time printEntity entity
let printTimedEntities printEntity elements_extract entityset = List.iter (Output.printf "%a@\n" (printTimedEntity printEntity)) (elements_extract entityset)

let printLines = printTimedEntities printLine LineSet.elements
let printEdges = printTimedEntities printEdge EdgeSet.elements
let printBlocks = printTimedEntities printBlock StmtInfoSet.elements
let printConditions = printTimedEntities printCondition CondSet.elements

let printCov file covType hist =
    let complement = !Executeargs.arg_print_complement_coverage in
    let total = getTotal file covType and numCovered = getNumCovered covType hist in
    let vis = Hashtbl.find coverage_totals file in
    Output.printf "%d out of %d %s (%.2f%%)@\n@." numCovered total (covTypeToStr covType) (percentage numCovered total);
    Output.printf "@[The %s %scovered were:@\n" (covTypeToStr covType) (if complement then "un" else "");
    begin match covType with
    | Line -> printLines (if complement then LineSet.diff vis#lines hist.coveredLines else hist.coveredLines)
    | Block -> printBlocks (if complement then StmtInfoSet.diff vis#blocks hist.coveredBlocks else hist.coveredBlocks)
    | Edge -> printEdges (if complement then EdgeSet.diff vis#edges hist.coveredEdges else hist.coveredEdges)
    | Cond -> printConditions (if complement then CondSet.diff vis#conds hist.coveredConds else hist.coveredConds)
    | Path -> failwith "printCoveringConfigs called for path coverage"
    end;
    Output.printf "@]@."

let printCoveringConfigs file coveringSet covType =
    let name = covTypeToStr covType in
    if coveringSet = [] then Output.printf "No constraints: any run covers all %s@." name
    else begin
        Output.printf "Here is a set of %d configurations which covers all the %s ever hit:@\n@."
                (List.length coveringSet) name;
        List.iter
        (fun job_result ->
                 printPath job_result#state job_result#exHist;
                 printCov file covType job_result#exHist;
                 Output.printf "-------------@\n@.")
             coveringSet
    end

let printCoverageInfo resultList =
    let file = (List.hd resultList)#file in
    if not (List.for_all (fun r -> r#file == file) resultList) then
        failwith "Cannot report coverage from different files!";

    if !Executeargs.arg_line_coverage then (
        Output.printf "Line coverage:@\n@.";
        let allLinesCovered =
            (List.fold_left
                 (fun acc job_result ->
                        LineSet.union acc job_result#exHist.coveredLines)
                 LineSet.empty
                 resultList)
        in
        printCov file Line { emptyHistory with coveredLines = allLinesCovered; };
        if !Executeargs.arg_print_covering_sets then
            let coveringSet = greedySetCover
                LineSet.is_empty
                (fun job_result remaining ->
                     LineSet.cardinal (LineSet.inter job_result#exHist.coveredLines remaining))
                (fun remaining job_result -> LineSet.diff remaining job_result#exHist.coveredLines)
                resultList
                allLinesCovered
            in
            printCoveringConfigs file coveringSet Line
    );

    if !Executeargs.arg_block_coverage then (
        Output.printf "Block coverage:@\n@.";
        let allBlocksCovered =
            (List.fold_left
                 (fun acc job_result ->
                        StmtInfoSet.union acc job_result#exHist.coveredBlocks)
                 StmtInfoSet.empty
                 resultList)
        in
        printCov file Block { emptyHistory with coveredBlocks = allBlocksCovered; };
        if !Executeargs.arg_print_covering_sets then
            let coveringSet = greedySetCover
                StmtInfoSet.is_empty
                (fun job_result remaining ->
                     StmtInfoSet.cardinal (StmtInfoSet.inter job_result#exHist.coveredBlocks remaining))
                (fun remaining job_result -> StmtInfoSet.diff remaining job_result#exHist.coveredBlocks)
                resultList
                allBlocksCovered
            in
            printCoveringConfigs file coveringSet Block
    );

    if !Executeargs.arg_edge_coverage then (
        Output.printf "Edge coverage:@\n@.";
        let allEdgesCovered =
            (List.fold_left
                 (fun acc job_result ->
                        EdgeSet.union acc job_result#exHist.coveredEdges)
                 EdgeSet.empty
                 resultList)
        in
        printCov file Edge { emptyHistory with coveredEdges = allEdgesCovered; };
        if !Executeargs.arg_print_covering_sets then
            let coveringSet = greedySetCover
                EdgeSet.is_empty
                (fun job_result remaining ->
                     EdgeSet.cardinal (EdgeSet.inter job_result#exHist.coveredEdges remaining))
                (fun remaining job_result -> EdgeSet.diff remaining job_result#exHist.coveredEdges)
                resultList
                allEdgesCovered
            in
            printCoveringConfigs file coveringSet Edge
    );

  if !Executeargs.arg_cond_coverage then (
        Output.printf "Condition coverage:@\n@.";
        let allCondsCovered =
            (List.fold_left
                 (fun acc job_result ->
                        CondSet.union acc job_result#exHist.coveredConds)
                 CondSet.empty
                 resultList)
        in
        printCov file Cond { emptyHistory with coveredConds = allCondsCovered; };
        if !Executeargs.arg_print_covering_sets then
            let coveringSet = greedySetCover
                CondSet.is_empty
                (fun job_result remaining ->
                     CondSet.cardinal (CondSet.inter job_result#exHist.coveredConds remaining))
                (fun remaining job_result -> CondSet.diff remaining job_result#exHist.coveredConds)
                resultList
                allCondsCovered
            in
            printCoveringConfigs file coveringSet Cond
    );

    if !Executeargs.arg_path_coverage then (
        (* I don't compute covering sets here because I assume each path
             is unique. However, if two paths x and y differ only within
             untracked functions, [x.executionPath = y.executionPath] will
             be true. *)
        Output.printf "Path coverage:@\n@.";
        List.iter
            (fun job_result ->
                 printPath job_result#state job_result#exHist;
                 Output.printf "The path contains %d statements@\n@." (List.length job_result#exHist.executionPath);
                 Output.printf "-------------@\n@.")
            resultList
    )

