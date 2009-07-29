open Cil
open Types

class getStatsVisitor = object
	val lines = ref LineSet.empty
(*	val locations = ref LocSet.empty*)
	val stmts = ref IntSet.empty
	val edges = ref EdgeSet.empty
	val conds = ref CondSet.empty

	method lines = !lines
	method stmts = !stmts
(*	method locations = !locations*)
	method edges = !edges
	method conds = !conds

	inherit nopCilVisitor

	method vinst instr =
		let loc = get_instrLoc instr in
		lines := LineSet.add (loc.file,loc.line) !lines;
(*		locations := LocSet.add loc !locations;*)
		SkipChildren (* There's nothing interesting under an [instr] *)

	method vstmt stmt =
		(* First get the edges *)
		(match stmt.skind with
				 Instr (_::_) (* Ignore [Instr] with empty list *)
			 | Goto _
			 | If _ ->
					 List.iter
						 (fun succ ->
								edges := EdgeSet.add (stmt,succ) !edges)
						 stmt.succs
			 | _ -> ()
		);
		(* Then get the lines, statements, and locations *)
		(match stmt.skind with
				Instr (_::_) -> (* Ignore [Instr] with empty list *)
					stmts := IntSet.add stmt.sid !stmts
			|	If(_,_,_,loc)
			| Cil.Return(_,loc)
			| Goto(_,loc) ->
					lines := LineSet.add (loc.file,loc.line) !lines;
(*					locations := LocSet.add loc !locations;*)
					stmts := IntSet.add stmt.sid !stmts
			| _ -> ()
		);
		(* Get the conditions *)
		(match stmt.skind with
				If _ ->
					conds := CondSet.add (stmt,true) (CondSet.add (stmt,false) !conds)
				| _ -> ()
		);
		DoChildren (* There could be stmts or instrs inside, which we should visit *)
end

let getProgInfo (file : Cil.file) fnNameSet =
	let vis = new getStatsVisitor in
	iterGlobals
		file
		(function (* Visit the bodies of the functions we care about *)
				 GFun(fundec,_) ->
					 if Types.StringSet.mem fundec.svar.vname fnNameSet
					 then ignore (visitCilBlock (vis:>cilVisitor) fundec.sbody)
			 | _ -> ()
		);
	(vis#lines,(*vis#locations,*)vis#stmts,vis#edges,vis#conds)
