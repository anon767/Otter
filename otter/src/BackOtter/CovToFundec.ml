open Cil
open OtterCore
open Job
open CoverageData

module CondMap = Map.Make (CondData)
module EdgeMap = Map.Make (EdgeData)
module StmtInfoMap = Map.Make (StmtInfoData)
module LineMap = Map.Make (LineData)

(* TODO: combine this with getStatsVisitor in Coverage.ml *)
class getStatsVisitor = object (self)
	val lines = ref LineMap.empty
	val blocks = ref StmtInfoMap.empty
	val edges = ref EdgeMap.empty
	val conds = ref CondMap.empty

	val currFunc = ref Cil.dummyFunDec

	method private stmtInfo_of_stmt stmt =
		{ siFuncName = (!currFunc).svar.vname; siStmt = stmt }

	method lines = !lines
	method blocks = !blocks
	method edges = !edges
	method conds = !conds

	inherit nopCilVisitor

	method vinst instr =
		let loc = get_instrLoc instr in
		lines := LineMap.add (loc.Cil.file,loc.Cil.line) (!currFunc) !lines;
		SkipChildren (* There's nothing interesting under an [instr] *)

	method vstmt stmt =
		let stmtInfo = self#stmtInfo_of_stmt stmt in
		(* Gather lines and conditions. *)
		(match stmt.skind with
				 If(_,_,_,loc) ->
						conds := CondMap.add (stmtInfo,true) (!currFunc) (CondMap.add (stmtInfo,false) (!currFunc) !conds);
						lines := LineMap.add (loc.Cil.file,loc.Cil.line) (!currFunc) !lines;
			 | Cil.Return(_,loc)
			 | Goto(_,loc)
			 | Loop (_,loc,_,_) ->
					 lines := LineMap.add (loc.Cil.file,loc.Cil.line) (!currFunc) !lines;
			 | _ -> ()
		);
		(* We represent basic blocks by their final statements, so if stmt
			 is the end of a basic block, add it to the Map of basic blocks
			 and add its outgoing edges to the Map of edges. (Edges are
			 represented as endOfBlock1 -> endOfBlock2.) *)
		if stmt == Coverage.stmtAtEndOfBlock stmt
		then (
			blocks := StmtInfoMap.add stmtInfo (!currFunc) !blocks;
			List.iter
				(fun succ -> edges := EdgeMap.add (stmtInfo, self#stmtInfo_of_stmt (Coverage.stmtAtEndOfBlock succ)) (!currFunc) !edges)
				stmt.succs
		);
		DoChildren (* There could be stmts or instrs inside, which we should visit *)

	method vfunc fundec = currFunc := fundec; DoChildren
end

let vis = new getStatsVisitor

let prepare_file file =
	iterGlobals
		file
		(function (* Visit the bodies of the functions we care about *)
				 GFun(fundec,_) -> ignore (visitCilFunction (vis:>cilVisitor) fundec)
			 | _ -> ()
		)

let of_line line = LineMap.find line vis#lines
let of_block block = StmtInfoMap.find block vis#blocks
let of_edge edge = EdgeMap.find edge vis#edges
let of_cond cond = CondMap.find cond vis#conds

