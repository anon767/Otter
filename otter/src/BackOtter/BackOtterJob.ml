(** Default Otter job based on command-line options. *)

open OcamlUtilities
open CilUtilities
open OtterJob

module E = OtterCore.ProgramPoints

let get_function_job file fundec =
    new FunctionJob.t file fundec

let get_default file =
    let mainfn = E.get_main_fundec file in
    let entryfn = E.get_entry_fundec file in
    if mainfn == entryfn then
        (* create a job for the file, with the commandline arguments set to the file name
         * and the arguments from the '--arg' option *)
        new FileJob.t file (file.Cil.fileName::!E.command_line)
    else
        (* create a job that starts at entry_function *)
        get_function_job file entryfn

