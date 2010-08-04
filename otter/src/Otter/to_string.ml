(* TODO: perhaps introduce laziness? *) 
open Format
open Cil
open Bytes
open Types

let strlen = 1000
let force_print = ref false
let donotprint() = (not (!force_print)) && (not (Output.need_print (!Output.current_msg_type)))


let location loc = 
	loc.Cil.file^":"^(string_of_int loc.Cil.line)
	

let varinfo v =
	(v.vname)^"@"^(location v.vdecl)
	

(* TODO: avoid rec *)
let rec

callstack s =
    match s with 
    | [] -> "\n"
    | context::tail -> (
        let instr_p i = 
            "\t"^(location (get_instrLoc i))
        in
        match context with
        | Runtime -> "\t(END)"
        | Source (_,_,i,_) -> instr_p i
        | NoReturn (i) -> instr_p i
    )^"\n"^(callstack tail)

and

stmt s =
  if donotprint() then "" else
	match s.skind with
		| If(e,_,_,_) -> "IF ("^(exp e)^")"
		| Instr(_) -> "(INSTRS)"
		| Loop(_,_,_,_) -> "(LOOP)"
		| Block(_) -> "(BLOCK)"
		| _ ->	Pretty.sprint strlen (Cil.d_stmt () s)
and

stmtkind skind =
  match skind with
    | Cil.Instr _ -> "instr"
    | Cil.Return _ -> "return"
    | Cil.Goto _ -> "goto"
    | Cil.Break _ -> "break"
    | Cil.Continue _ -> "continue"
    | Cil.If _ -> "if"
    | Cil.Switch  _ -> "switch"
    | Cil.Loop  _ -> "loop"
    | Cil.Block  _ -> "block"
    | Cil.TryFinally  _ -> "tryfinally"
    | Cil.TryExcept  _ -> "tryexcept"

and

instr i = 
  if donotprint() then "" else
	try
	Pretty.sprint strlen (Cil.d_instr () i)
	with
		| Errormsg.Error -> ""

and

typ t = Pretty.sprint strlen (Cil.d_type () t)

and

exp_ff ff = function
	| StartOf _ as e -> fprintf ff "StartOf(%a)" Printcil.f_exp e
	| e -> Printcil.f_exp ff e

and

exp e =
	if donotprint () then
		""
	else begin
		exp_ff str_formatter e;
		flush_str_formatter ()
	end

and

fundec f = 
  if donotprint() then "" else
  f.svar.vname^" : "^(typ f.svar.vtype)


let stmtInfo si =
	let str = si.siFuncName ^ " " ^ (string_of_int si.siStmt.sid) in
	if Executeargs.print_args.Executeargs.arg_print_stmt_locs
	then str ^ " (" ^ (location (get_stmtLoc si.siStmt.skind)) ^ ")"
	else str

