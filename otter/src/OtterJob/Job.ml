(** Default Otter job based on command-line options. *)

open OcamlUtilities
open CilUtilities

let entry_function = ref "main"
let command_line = ref []

let get_default file =
    if !entry_function = "main" then
        (* create a job for the file, with the commandline arguments set to the file name
         * and the arguments from the '--arg' option *)
        FileJob.make file (file.Cil.fileName::!command_line)
    else
        (* create a job that starts at entry_function *)
        let entryfn =
            try
                FindCil.fundec_by_name file !entry_function
            with Not_found ->
                FormatPlus.failwith "Entry function %s not found" !entry_function
        in
        FunctionJob.make file entryfn

let options = [
    "--entryfn",
        Arg.Set_string entry_function,
        "<fname> Entry function (default: main) \n";

    "--arg",
        Arg.String (fun argv -> command_line := !command_line @ [argv]),
        "<argv> Run with command line argument <argv>\
        \t\t\t\t(This option can be repeated to give multiple arguments.)\n";
]

