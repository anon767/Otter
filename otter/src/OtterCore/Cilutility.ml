open Cil
open CilUtilities

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
                            (try (FindCil.fundec_by_name file varinfo.vname)::lst with Failure _ -> lst)
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




