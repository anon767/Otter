open CilUtilities 
open Cil

type t =
    | DecisionConditional of Cil.stmt * bool (* if-statement *)
    | DecisionFuncall of Cil.instr * Cil.varinfo (* function call (including functions without bodies, e.g., builtins) *)
    (* TODO (martin): define DecisionLongjmp *)

(* TODO: derive equal from compare *)
let equal d1 d2 =
    match d1, d2 with
    | DecisionConditional (stmt1, bool1), DecisionConditional (stmt2, bool2) when stmt1 == stmt2 && bool1 = bool2 -> true
    | DecisionFuncall (instr1, varinfo1), DecisionFuncall (instr2, varinfo2) when instr1 == instr2 && varinfo1.vid = varinfo2.vid -> true
    | _, _ -> false

let compare d1 d2 =
    match d1, d2 with
    | DecisionConditional (stmt1, bool1), DecisionConditional (stmt2, bool2) -> Pervasives.compare (stmt1, bool1) (stmt2, bool2)
    | DecisionConditional _, _ -> 1
    | _, DecisionConditional _ -> -1
    | DecisionFuncall (instr1, varinfo1), DecisionFuncall (instr2, varinfo2) -> Pervasives.compare (instr1, varinfo1.vid) (instr2, varinfo2.vid)

let print_decisions ff decisions =
    let print_decision ff decision =
        match decision with
        | DecisionConditional(stmt,truth) ->
            Format.fprintf ff "Decision: @[%a@]: %s@\n" CilPrinter.stmt_abbr stmt (if truth then "true" else "false")
        | DecisionFuncall(instr,varinfo) ->
            Format.fprintf ff "Decision: @[%a@]@\n" CilPrinter.varinfo varinfo
    in
    if decisions = [] then
        Format.fprintf ff "Decision: (none)@\n"
    else
        List.iter (print_decision ff) decisions
