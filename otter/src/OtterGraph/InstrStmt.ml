open OcamlUtilities
open Cil

type t =
  | Instr of Cil.instr list * Cil.stmt (* stmt: stmt that contains instr *)
  | Stmt of Cil.stmt

let make_stmt stmt = Stmt (stmt)

let make_instr (instrs,stmt) = Instr (instrs,stmt)

let print ?(obj=None) instr_stmt =
  let location_special ff loc =
    let v = if loc.line > 0 then loc.line else -(abs ((Hashtbl.hash obj) mod 256))
    in
      Format.fprintf ff "%0.3d" v
  in
  match instr_stmt with
    | Instr ([],stmt)     -> FormatPlus.sprintf "%a(%s,%a)" location_special (Cil.get_stmtLoc stmt.skind) "[]" OtterCore.Printer.stmt_kind stmt
    | Instr (instrs,stmt) -> FormatPlus.sprintf "%a(%s,%a)" location_special (Cil.get_instrLoc (List.hd instrs)) "instrs" OtterCore.Printer.stmt_kind stmt
    | Stmt (stmt)         -> FormatPlus.sprintf "%a(%s,%a)" location_special (Cil.get_stmtLoc stmt.skind) "stmt" OtterCore.Printer.stmt_kind stmt


