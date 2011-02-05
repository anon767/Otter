(** Default Otter job based on command-line options. *)

open OcamlUtilities
open CilUtilities

module E = OtterCore.ProgramPoints

let get_default file =
    if !E.entry_function = !E.main_function then
        (* create a job for the file, with the commandline arguments set to the file name
         * and the arguments from the '--arg' option *)
        FileJob.make file (file.Cil.fileName::!E.command_line)
    else
        (* create a job that starts at entry_function *)
        let entryfn = E.get_entry_fundec file in
        FunctionJob.make file entryfn

