open Cil

(* Is there a better way to do this than using globals? Trying to put
	 then in the class gives me errors. *)
let lines = Hashtbl.create 20
let locations = Hashtbl.create 20
let stmts = Hashtbl.create 20
let edges = Hashtbl.create 20
let funs = ref []

class countStmtsVisitor = object
	inherit nopCilVisitor

	(* I use Hashtbl.replace instead of Hashtbl.add because I'm using
		 the hash table as a set, so I don't want duplicates. *)
	method vinst instr =
		let loc = get_instrLoc instr in
		Hashtbl.replace lines (loc.file,loc.line) ();
		Hashtbl.replace locations loc ();
		SkipChildren (* There's nothing interesting under an [Instr] *)

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
		DoChildren

	method vfunc fundec =
		funs := fundec.svar.vname :: !funs;
		DoChildren
end

(* Maybe this should go in Executeargs, rather than being global. *)
let outFile = ref ""

let getStats (file : Cil.file) : unit =
	let outChan = if !outFile = "" then stdout else open_out !outFile in
	Cilly.makeCFGFeature.fd_doit file;
	let vis = new countStmtsVisitor in
	visitCilFileSameGlobals vis file;
	let numLines = Hashtbl.length lines in
	let numLocs = Hashtbl.length locations in
	let numStmts = Hashtbl.length stmts in
	let numEdges = Hashtbl.length edges in
	Printf.fprintf outChan
		"%d lines\n%d locations\n%d statements\n%d edges\n%d functions:\n"
		numLines numLocs numStmts numEdges (List.length !funs);
	List.iter (fun str -> output_string outChan (str ^ "\n")) !funs;
  close_out outChan

let feature : Cil.featureDescr = {
  Cil.fd_enabled = ref false;
  Cil.fd_name = "progStats";
  Cil.fd_description = "Print out the number of lines, locations, statements, and edges in the program.\n\t\t\tAlso, print the number of functions and list them all.\n";
  Cil.fd_extraopt = [
		("--statsFile",
		Arg.Set_string outFile,
		"<filename> The file to which to write the statistics (default is stdout)")
	];
  Cil.fd_doit = getStats;
  Cil.fd_post_check = false;
}
