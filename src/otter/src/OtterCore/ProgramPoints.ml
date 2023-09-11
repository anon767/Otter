(* Manages main(), program entry and command line arguments *)

open OcamlUtilities
open CilUtilities

(* TODO: make these settings private *)
let main_fname = ref "main"
let entry_fname = ref ""  (* "" means the same as main_fname *)
let failure_fname = ref "__FAILURE"
let command_line = ref []

let get_fundec file ftype fname = 
    try
        FindCil.fundec_by_name file fname
    with Not_found ->
        FormatPlus.failwith "%s function %s not found" ftype fname

let set_main fname = main_fname := fname
let set_entry fname = entry_fname := fname
let set_failure fname = failure_fname := fname
let set_cli cli = command_line := cli

let get_main_fundec file = get_fundec file "Main" (!main_fname)
let get_entry_fundec file = get_fundec file "Entry" (if !entry_fname = "" then !main_fname else !entry_fname)
let get_failure_fundec file = get_fundec file "Failure" (!failure_fname)

let options = [
    "--entryfn",
        Arg.Set_string entry_fname,
        "<fname> The entry function at which to begin symbolic execution;";
    "", Arg.Tuple [], " if not \"main\", the state will be initialized symbolically (default: same as --mainfn)";

    "--mainfn",
        Arg.Set_string main_fname,
        "<fname> The main function of the program; must either take no argument or (int argc, char** argv) (default: \"main\")";

    ("--failurefn",
        Arg.Set_string failure_fname,
        "<fname> Failure function which raises FailureReached when called (default: __FAILURE)");

    "--arg",
        Arg.String (fun argv -> command_line := !command_line @ [argv]),
        "<argv> Run with command line argument <argv> (repeat for each argument);";
    "", Arg.Tuple [], " ignored if --entryfn is given and not \"main\"";
]

