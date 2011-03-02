(* With statement ids which are unique only within functions, we need
	 to know the function's name in addition to the sid to uniquely
	 identify a stmt. *)
type stmtInfo = { siFuncName : string ; siStmt : Cil.stmt }
let compareStmtInfo x y =
	let tmp = Pervasives.compare x.siFuncName y.siFuncName in
	if tmp = 0
	then Pervasives.compare x.siStmt.Cil.sid y.siStmt.Cil.sid
	else tmp

module CondData = struct
	type t = stmtInfo*bool
	(* the if statement and branch direction *)
	let compare ((stmt1,truth1):t) (stmt2,truth2) =
		let stmtCmp = compareStmtInfo stmt1 stmt2 in
		if stmtCmp = 0
		then Pervasives.compare truth1 truth2
		else stmtCmp
end

module EdgeData = struct
	type t = stmtInfo*stmtInfo (* These should always be the final stmts in their basic blocks *)
	(* Order edges primarily by source, then by destination *)
	let compare ((src1,dst1):t) (src2,dst2) =
		let srcCmp = compareStmtInfo src1 src2 in
		if srcCmp = 0
		then compareStmtInfo dst1 dst2
		else srcCmp
end

module StmtInfoData = struct
    type t = stmtInfo
    let compare = compareStmtInfo
end

module LineData = struct
    type t = string * int (** (filename,line number) pair *)
    let compare ((f1,l1):t) (f2,l2) =
   	 let tmp = String.compare f1 f2 in
   	 if tmp = 0
   	 then Pervasives.compare l1 l2
   	 else tmp
end
