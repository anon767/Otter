open Cil
open CilUtilities
open Types
open Cilutility

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

class getStatsVisitor = object (self)
	val lines = ref LineSet.empty
	val blocks = ref StmtInfoSet.empty
	val edges = ref EdgeSet.empty
	val conds = ref CondSet.empty

	val currFuncName = ref ""

	method private stmtInfo_of_stmt stmt =
		{ siFuncName = !currFuncName ; siStmt = stmt }

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
		if stmt == Cilutility.stmtAtEndOfBlock stmt
		then (
			blocks := StmtInfoSet.add stmtInfo !blocks;
			List.iter
				(fun succ -> edges := EdgeSet.add (stmtInfo, self#stmtInfo_of_stmt (Cilutility.stmtAtEndOfBlock succ)) !edges)
				stmt.succs
		);
		DoChildren (* There could be stmts or instrs inside, which we should visit *)

	method vfunc fundec = currFuncName := fundec.svar.vname; DoChildren
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

let getProgInfo (file : Cil.file) fnNames =
	let vis = new getStatsVisitor in
	iterGlobals
		file
		(function (* Visit the bodies of the functions we care about *)
				 GFun(fundec,_) ->
					 if List.mem fundec.svar.vname fnNames
					 then ignore (visitCilFunction (vis:>cilVisitor) fundec)
			 | _ -> ()
		);
	(vis#lines,vis#blocks,vis#edges,vis#conds)

(* TODO: a global can be reachable by another global init! *)
let computeReachableCode file = 
  (* compute reachable globals from main *)
  let main_func =
  	try FindCil.fundec_by_name file "main"
  	with Not_found -> failwith "No main function found!"
  in
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

