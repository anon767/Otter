open OcamlUtilities
open OtterCore
open Job
open Cil

type instr_stmt =
  | Instr of Cil.instr list * Cil.stmt (* stmt: stmt that contains instr *)
  | Stmt of Cil.stmt

let instr_stmt_length instr_stmt =
  match instr_stmt with
    | Stmt (stmt) -> List.length stmt.succs
    | _ -> 1

let make_instr_stmt_Stmt stmt =
  Stmt (stmt)

let make_instr_stmt_Instr (instrs,stmt) =
  Instr (instrs,stmt)

let get_instr_stmt job =
  match job.instrList with
    | instr::instrs -> 
        make_instr_stmt_Instr (job.instrList,List.hd job.stmt.preds)
    | [] -> make_instr_stmt_Stmt (job.stmt)

let print_instr_stmt ?(obj=None) instr_stmt =
  let location_special ff loc =
    let v = if loc.line > 0 then loc.line else -(abs ((Hashtbl.hash obj) mod 256))
    in
      Format.fprintf ff "%0.3d" v
  in
  match instr_stmt with
    | Instr ([],stmt)     -> FormatPlus.sprintf "%a(%s,%a)" location_special (Cil.get_stmtLoc stmt.skind) "[]" Printer.stmt_kind stmt
    | Instr (instrs,stmt) -> FormatPlus.sprintf "%a(%s,%a)" location_special (Cil.get_instrLoc (List.hd instrs)) "instrs" Printer.stmt_kind stmt
    | Stmt (stmt)         -> FormatPlus.sprintf "%a(%s,%a)" location_special (Cil.get_stmtLoc stmt.skind) "stmt" Printer.stmt_kind stmt


