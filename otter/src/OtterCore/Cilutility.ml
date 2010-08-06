open Cil

module VarinfoMap = Map.Make (struct
	type t = Cil.varinfo
	let compare a b = Pervasives.compare a.Cil.vid b.Cil.vid
end)

module VarinfoSet = Set.Make (struct
	type t = Cil.varinfo
	let compare a b = Pervasives.compare a.Cil.vid b.Cil.vid
end)

module TypeMap = Map.Make (struct
	type t = typ
	let compare x y =
		let canonicalize t = typeSigWithAttrs (fun _ -> []) t in
		Pervasives.compare (canonicalize x) (canonicalize y)
end)

module FundecMap = Map.Make (struct
	type t = Cil.fundec
	let compare a b = let id x = x.svar.vid in Pervasives.compare (id a) (id b)
end)

module FundecSet = Set.Make (struct
	type t = Cil.fundec
	let compare a b = let id x = x.svar.vid in Pervasives.compare (id a) (id b)
end)

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


let rec isConstType typ =
  let rec isconst atts = match atts with
      [] -> false
    | Attr(s,_)::atts -> if s = "const" then true else isconst atts
  in
  match typ with
   | TVoid a 
   | TInt(_, a) 
   | TFloat(_, a) 
   | TNamed (_, a) 
   | TEnum(_,a) 
   | TFun(_,_,_,a) 
   | TComp (_, a) 
   | TBuiltin_va_list a -> isconst a
   | TArray(t,_,a) 
   | TPtr(t, a) -> isConstType t


(** Memoization tables for searching files. *)
let file_memotables = Hashtbl.create 0


(** Get the memoization tables for searching in a file, initializing them if necessary.
		@param file the file to get the memoization tables for
		@return [(vafinfo_to_fundec, varinfo_to_varinit, name_to_fundec)] a tuple containing the memoization
				tables for searching the file
*)
let get_file_memotables file =
	try
		Hashtbl.find file_memotables file
	with Not_found ->
		let varinfo_to_fundec = Hashtbl.create 100 in
		let varinfo_to_varinit = Hashtbl.create 100 in
		let name_to_fundec = Hashtbl.create 100 in
		let name_to_global_varinfo = Hashtbl.create 100 in

		Cil.iterGlobals file begin function
			| GFun(fundec,_) ->
				Hashtbl.add varinfo_to_fundec fundec.svar fundec;
				Hashtbl.add name_to_fundec fundec.svar.vname fundec;
			| GVar(varinfo,initinfo,_) ->
				Hashtbl.add varinfo_to_varinit varinfo initinfo;
				Hashtbl.add name_to_global_varinfo varinfo.vname varinfo;
			| GVarDecl(varinfo,_) ->
				Hashtbl.add varinfo_to_varinit varinfo { init=None };
				Hashtbl.add name_to_global_varinfo varinfo.vname varinfo;
			| _ -> ()
		end;

		let memotables = object
			method varinfo_to_fundec = varinfo_to_fundec
			method varinfo_to_varinit = varinfo_to_varinit
			method name_to_fundec = name_to_fundec
			method name_to_global_varinfo = name_to_global_varinfo
		end in
		Hashtbl.add file_memotables file memotables;
		memotables


(** Find a {!Cil.fundec} by {!Cil.varinfo} from a {!Cil.file}.
		@param file the {!Cil.file} to find the {!Cil.fundec} in
		@param varinfo the {!Cil.varinfo} of the {!Cil.fundec} to find
		@return the {!Cil.fundec}
		@raise Not_found if a {!Cil.fundec} for [varinfo] does not exist in [file]
*)
let find_fundec_by_varinfo file varinfo =
	Hashtbl.find (get_file_memotables file)#varinfo_to_fundec varinfo


(** Find the {!Cil.initinfo} for a {!Cil.varinfo} from a {!Cil.file}.
		@param file the {!Cil.file} to find the {!Cil.initinfo} in
		@param varinfo the {!Cil.varinfo} of the {!Cil.initinfo} to find
		@return the {!Cil.initinfo}
		@raise Not_found if [varinfo] does not exist in [file]
*)
let find_varinit file varinfo =
	Hashtbl.find (get_file_memotables file)#varinfo_to_varinit varinfo


(** Find a {!Cil.fundec} by name from a {!Cil.file}.
		@param file the {!Cil.file} to find the {!Cil.fundec} in
		@param name the name of the {!Cil.fundec} to find
		@return the {!Cil.fundec}
		@raise Not_found if a {!Cil.fundec} named [name] does not exist in [file]
*)
let find_fundec_by_name file name =
	Hashtbl.find (get_file_memotables file)#name_to_fundec name


(** Find a {!Cil.varinfo} by name from a {!Cil.file}.
		@param file the {!Cil.file} to find the {!Cil.fundec} in
		@param name the name of the {!Cil.fundec} to find
		@return the {!Cil.varinfo}
		@raise Not_found if a {!Cil.varinfo} named [name] does not exist in [file]
*)
let find_global_varinfo_by_name file name =
	Hashtbl.find (get_file_memotables file)#name_to_global_varinfo name


let make_callgraph file : Cil.fundec list FundecMap.t =
  let get_callees fundec =
    List.fold_left 
      begin
        fun lst stmt -> 
          match stmt.skind with
            | Instr (instrs) -> 
                List.fold_left 
                  begin
                    fun lst instr -> 
                      match instr with
                        | Call (_,Lval(Var(varinfo),NoOffset),_,_) -> 
                            (try (find_fundec_by_name file varinfo.vname)::lst with Failure _ -> lst)
                        | _ -> lst
                  end
                  lst instrs
            | _ -> lst
      end
      [] fundec.sallstmts
  in
  List.fold_left 
    begin
      fun cg -> function
        | GFun(fundec,_) -> FundecMap.add fundec (get_callees fundec) cg
        | _ -> cg
    end
    (FundecMap.empty) file.globals


let make_callergraph file : Cil.fundec list FundecMap.t =
  let callgraph = make_callgraph file in
    FundecMap.fold
      begin
        fun caller callees cg ->
          List.fold_left
            begin
              fun cg callee ->
                let callers = if FundecMap.mem callee cg then FundecMap.find callee cg else [] in
                  FundecMap.add callee (caller::callers) cg
            end
            cg callees
      end
      callgraph FundecMap.empty


let get_callers callergraph func =
  try 
    FundecMap.find func callergraph
  with Not_found -> []




