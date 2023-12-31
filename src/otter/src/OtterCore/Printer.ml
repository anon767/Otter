(** {!Format}-style printers for types in the {!Types} module *)

open OcamlUtilities

(** Print a list of {!State.callingContext}.
		@param sep is the separator string, which may include \@ control operators of {!Format.fprintf}
		@param ff is the formatter to which to print
		@param list is the list of {!State.callingContext} to print
*)
let callingContext_list sep ff list =
	let context ff = function
		| State.Runtime -> Format.pp_print_string ff "(Runtime)"
		| State.Source (_, _, instr, _) -> Printcil.loc ff (Cil.get_instrLoc instr)
		| State.NoReturn (_,instr) -> Format.fprintf ff "NoReturn@@%a" Printcil.loc (Cil.get_instrLoc instr)
	in
	FormatPlus.pp_print_list context sep ff list


