open Cil
open Types

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
		lines := LineSet.add (loc.file,loc.line) !lines;
		SkipChildren (* There's nothing interesting under an [instr] *)

	method vstmt stmt =
		let stmtInfo = self#stmtInfo_of_stmt stmt in
		(* Gather lines and conditions. *)
		(match stmt.skind with
				 If(_,_,_,loc) ->
						conds := CondSet.add (stmtInfo,true) (CondSet.add (stmtInfo,false) !conds);
						lines := LineSet.add (loc.file,loc.line) !lines;
			 | Cil.Return(_,loc)
			 | Goto(_,loc)
			 | Loop (_,loc,_,_) ->
					 lines := LineSet.add (loc.file,loc.line) !lines;
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

let getProgInfo (file : Cil.file) fnNameSet =
	let vis = new getStatsVisitor in
	iterGlobals
		file
		(function (* Visit the bodies of the functions we care about *)
				 GFun(fundec,_) ->
					 if Types.StringSet.mem fundec.svar.vname fnNameSet
					 then ignore (visitCilFunction (vis:>cilVisitor) fundec)
			 | _ -> ()
		);
	(vis#lines,vis#blocks,vis#edges,vis#conds)
