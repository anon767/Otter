type t =
    | DecisionConditional of Cil.stmt * bool (* if-statement *)
    | DecisionFuncall of Cil.instr * Cil.fundec (* function call *)
    | DecisionEnd (* Mark the end of a decision path *)
    (* TODO (martin): define DecisionLongjmp *)

let equals d1 d2 =
    match d1, d2 with
    | DecisionConditional (stmt1, bool1), DecisionConditional (stmt2, bool2) when stmt1 == stmt2 && bool1 = bool2 -> true
    | DecisionFuncall (instr1, fundec1), DecisionFuncall (instr2, fundec2) when instr1 == instr2 && fundec1 == fundec2 -> true
    | DecisionEnd, DecisionEnd -> true
    | _, _ -> false

let print_decisions ff decisions =
    let print_decision ff decision =
        match decision with
        | DecisionConditional(stmt,truth) ->
            Format.fprintf ff "Decision: @[%a@]: %s@\n" Printer.stmt_abbr stmt (if truth then "true" else "false")
        | DecisionFuncall(instr,fundec) ->
            Format.fprintf ff "Decision: @[%a@]@\n" Printer.fundec fundec
        | DecisionEnd ->
            Format.fprintf ff "Decision: END@\n"
    in
    if decisions = [] then
        Format.fprintf ff "Decision: (none)@\n"
    else
        List.iter (print_decision ff) decisions
