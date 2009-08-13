open Cil

module VarinfoMap =
	Utility.MakeMap (
	struct
		type t = Cil.varinfo
		let compare a b =
			(* try Output.print_endline ("Compare "^a.Cil.vname^" and            *)
			(* "^b.Cil.vname);                                                   *)
			let c = Pervasives.compare a.Cil.vid b.Cil.vid in
				(* Output.print_endline ("Result = "^(string_of_int c)); *)
			c
	end
	)

let bitsSizeOfExp exp =
	(Cil.bitsSizeOf (Cil.typeOf exp)) / 8;;

let fundecHashtbl : (varinfo, fundec) Hashtbl.t = Hashtbl.create 100;;

let search_function varinfo = Hashtbl.find fundecHashtbl varinfo;;

module FundecMap =
	Utility.MakeMap (
	struct
		type t = Cil.fundec
		let compare a b = let id x = x.svar.vid in Pervasives.compare (id a) (id b)
	end
	)

(** This is like {!Cil.unrollType} except that it only keeps the
		attributes in the base (unnamed) type. *)
let unrollType = function
	| TNamed (typInfo,_) -> unrollType typInfo.ttype
	| x -> x

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
