(** Contains 4 basic coverage metrics: line, edge, stmtinfo (block) and cond *)

(* TODO: move stmtInfo type into StmtInfoData *)
type stmtInfo = { siFuncName : string ; siStmt : Cil.stmt }

(* With statement ids which are unique only within functions, we need
     to know the function's name in addition to the sid to uniquely
     identify a stmt. *)
module StmtInfoData = struct
    type t = stmtInfo
    let compare x y = 
        match Pervasives.compare x.siFuncName y.siFuncName with
        | 0 -> Pervasives.compare x.siStmt.Cil.sid y.siStmt.Cil.sid
        | i -> i
end

module CondData = struct
    type t = StmtInfoData.t*bool
    (* the if statement and branch direction *)
    let compare ((stmt1,truth1):t) (stmt2,truth2) =
        match StmtInfoData.compare stmt1 stmt2 with
        | 0 -> Pervasives.compare truth1 truth2
        | i -> i 
end

module EdgeData = struct
    type t = StmtInfoData.t*StmtInfoData.t (* These should always be the final stmts in their basic blocks *)
    (* Order edges primarily by source, then by destination *)
    let compare ((src1,dst1):t) (src2,dst2) =
        match StmtInfoData.compare src1 src2 with
        | 0 -> StmtInfoData.compare dst1 dst2
        | i -> i
end

module LineData = struct
    type t = string * int (** (filename,line number) pair *)
    let compare ((f1,l1):t) (f2,l2) =
        match String.compare f1 f2 with
        | 0 -> Pervasives.compare l1 l2
        | i -> i
end

type t =
    | Line of LineData.t
    | StmtInfo of StmtInfoData.t
    | Edge of EdgeData.t
    | Cond of CondData.t

(* TODO: move this into StmtInfoData *)
(** Print the name and type of a {!State.stmtInfo}.
        @param ff is the formatter to which to print
        @param fn is the {!State.stmtInfo} to print
*)
let printStmtInfo ff si =
    Format.fprintf ff "%s %d" si.siFuncName si.siStmt.Cil.sid;
    if !Executeargs.arg_print_stmtInfo_locs then
        Format.fprintf ff " (%a)" Printcil.loc (Cil.get_stmtLoc si.siStmt.Cil.skind)

(* TODO: change Coverage.ml to use this to print stuff *)
let printer ff (covdata : t) = match covdata with
    | Line (file, lineno) -> Format.fprintf ff "%s:%d" file lineno
    | StmtInfo stmtInfo -> Format.fprintf ff "@[%a@]" printStmtInfo stmtInfo
    | Edge (src, dest) -> Format.fprintf ff "@[@[%a@]@ -> @[%a@]@]" printStmtInfo src printStmtInfo dest
    | Cond (stmtInfo, truth) -> Format.fprintf ff "@[%a@] %c" printStmtInfo stmtInfo (if truth then 'T' else 'F')

