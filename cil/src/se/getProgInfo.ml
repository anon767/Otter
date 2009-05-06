open Cil

(* Is there a better way to do this than using globals? Trying to put
	 then in the class gives me errors. *)
let lines = Hashtbl.create 20
let locations = Hashtbl.create 20
let stmts = Hashtbl.create 20
let edges = Hashtbl.create 20

class getStatsVisitor = object
	inherit nopCilVisitor

	(* I use Hashtbl.replace instead of Hashtbl.add because I'm using
		 the hash table as a set, so I don't want duplicates. *)
	method vinst instr =
		let loc = get_instrLoc instr in
		Hashtbl.replace lines (loc.file,loc.line) ();
		Hashtbl.replace locations loc ();
		SkipChildren (* There's nothing interesting under an [instr] *)

	method vstmt stmt =
		(* First get the edges *)
		(match stmt.skind with
				 Instr (_::_) (* Ignore [Instr] with empty list *)
			 | Goto _
			 | If _ ->
					 List.iter
						 (fun succ ->
								if Hashtbl.mem edges (stmt,succ) then failwith "edge already in table";
								Hashtbl.replace edges (stmt,succ) ())
						 stmt.succs
			 | _ -> ()
		);
		(* Then get the lines, statements, and locations *)
		(match stmt.skind with
				Instr (_::_) -> (* Ignore [Instr] with empty list *)
					Hashtbl.replace stmts stmt ()
			|	If(_,_,_,loc)
			| Return(_,loc)
			| Goto(_,loc) ->
					Hashtbl.replace lines (loc.file,loc.line) ();
					Hashtbl.replace locations loc ();
					Hashtbl.replace stmts stmt ()
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
					 then ignore (visitCilBlock vis fundec.sbody)
			 | _ -> ()
		);
	(lines,locations,stmts,edges)
