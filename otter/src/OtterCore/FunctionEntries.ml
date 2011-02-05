(* Manages main(), program entry and command line arguments *)

open OcamlUtilities
open CilUtilities

(* TODO: make these settings private *)
(* TODO: move failurefn here *)
let main_function = ref "main"
let entry_function = ref (!main_function)
let command_line = ref []

let get_fundec file ftype fname = 
    try
        FindCil.fundec_by_name file fname
    with Not_found ->
        FormatPlus.failwith "%s function %s not found" ftype fname

let get_main_fundec file = get_fundec file "Main" (!main_function)
let get_entry_fundec file = get_fundec file "Entry" (!entry_function)

let options = [
    "--entryfn",
        Arg.Set_string entry_function,
        "<fname> The entry function at which to begin symbolic execution;";
    "", Arg.Tuple [], " if not \"main\", the state will be initialized symbolically (default: same as --mainfn)";

    "--mainfn",
        Arg.Set_string main_function,
        "<fname> The main function of the program; must either take no argument or (int argc, char** argv) (default: \"main\")";

    "--arg",
        Arg.String (fun argv -> command_line := !command_line @ [argv]),
        "<argv> Run with command line argument <argv> (repeat for each argument);";
    "", Arg.Tuple [], " ignored if --entryfn is given and not \"main\"";
]

