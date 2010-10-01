(** {!Format}-style printers for types in the {!Types} module *)

open OcamlUtilities


(** Print the name and location of a {!Cil.varinfo}.
		@param ff is the formatter to which to print
		@param v is the {!Cil.varinfo} to print
*)
let varinfo ff v =
	Format.fprintf ff "%s@@@[%a@]" v.Cil.vname Printcil.loc v.Cil.vdecl


(** Print a {!Cil.exp}.
		@param ff is the formatter to which to print
		@param e is the {!Cil.exp} to print
*)
let exp ff = function
	| Cil.StartOf _ as e -> Format.fprintf ff "StartOf(@[%a@]@,)" Printcil.exp e
	| e -> Printcil.exp ff e


(** Print an abbreviated representation of a {!Cil.stmt}. In particular, the following {!Cil.stmt} are abbreviated:
	- {!Cil.If} is printed as "[IF (<exp>)]";
	- {!Cil.Instr} is printed as "[(INSTR)]";
	- {!Cil.Loop} is printed as "[(LOOP)]";
	- {!Cil.Block} is printed as "[(BLOCK)]".
		@param ff is the formatter to which to print
		@param s is the {!Cil.stmt} to print
*)
let stmt_abbr ff s = match s.Cil.skind with
	| Cil.If (e, _, _, _) -> Format.fprintf ff "IF (@[%a@]@,)" exp e
	| Cil.Instr _ -> Format.pp_print_string ff "(INSTRS)"
	| Cil.Loop _ -> Format.pp_print_string ff "(LOOP)"
	| Cil.Block _ -> Format.pp_print_string ff "(BLOCK)"
	| _ -> Printcil.stmt ff s


(** Print the kind of a {!Cil.stmt} (i.e., [s.Cil.skind] for a {!Cil.stmt} [s]).
		@param ff is the formatter to which to print
		@param s is the {!Cil.stmt} to print
*)
let stmt_kind ff s =
	let kind = match s.Cil.skind with
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
	in
	Format.pp_print_string ff kind


(** Print the name and type of a {!Cil.fundec}.
		@param ff is the formatter to which to print
		@param fn is the {!Cil.fundec} to print
*)
let fundec ff fn =
	Format.fprintf ff "%s: @[%a@]" fn.Cil.svar.Cil.vname Printcil.typ fn.Cil.svar.Cil.vtype


(** Print a list of {!Types.callingContext}.
		@param sep is the separator string, which may include \@ control operators of {!Format.fprintf}
		@param ff is the formatter to which to print
		@param list is the list of {!Types.callingContext} to print
*)
let callingContext_list sep ff list =
	let context ff = function
		| Types.Runtime -> Format.pp_print_string ff "(Runtime)"
		| Types.Source (_, _, instr, _) -> Printcil.loc ff (Cil.get_instrLoc instr)
		| Types.NoReturn instr -> Format.fprintf ff "NoReturn@@%a" Printcil.loc (Cil.get_instrLoc instr)
	in
	FormatPlus.pp_print_list context sep ff list


