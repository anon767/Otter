(** Default Otter job based on command-line options. *)

open OcamlUtilities
open CilUtilities
open OtterJob

module E = OtterCore.ProgramPoints

module StringMap = Map.Make(String)
let points_to_per_function = ref StringMap.empty

let get_function_job file fundec =
    let fname = fundec.Cil.svar.Cil.vname in

    (* Check if we want a specific points-to method for this function *)
    let points_to = try StringMap.find fname (!points_to_per_function) with Not_found -> FunctionJob.(!default_points_to) in

    Output.debug_printf "Initialize %s with %s points-to analysis@." fname 
        (try List.assq points_to (List.map (fun (a,b) -> (b,a)) FunctionJob.points_tos) with Not_found -> "unknown");

    (* Call points_to with file *)
    let points_to = points_to file in
    new BackOtterFunctionJob.t file ~points_to fundec


let get_default file =
    let mainfn = E.get_main_fundec file in
    let entryfn = E.get_entry_fundec file in
    if mainfn == entryfn then
        (* create a job for the file, with the commandline arguments set to the file name
         * and the arguments from the '--arg' option *)
        new BackOtterFileJob.t file (file.Cil.fileName::!E.command_line)
    else
        (* create a job that starts at entry_function *)
        get_function_job file entryfn


let options = [
    "--points-to-analysis-for-function",
        Arg.String begin fun str ->
            let args = Str.split (Str.regexp ",") str in
            let re = Str.regexp "\\(.*\\):\\(.*\\)" in
            List.iter (fun arg ->
                if Str.string_match re arg 0 then
                    let fname = Str.matched_group 1 arg in
                    let points_to_name = Str.matched_group 2 arg in
                    let points_to = List.assoc points_to_name FunctionJob.points_tos in
                    points_to_per_function := StringMap.add fname points_to (!points_to_per_function)
                else
                    failwith "Error in parsing --points-to-analysis-for-function"
            ) args
        end,
        "<fname:points_to,...> Set the points_to analysis for function fname (default: the default set by --points-to-analysis)";
]

