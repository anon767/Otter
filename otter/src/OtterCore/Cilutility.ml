open Cil

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

