open OcamlUtilities
open CilUtilities
open OtterCFG
open OtterCore
open Job
open Cil

let arg_line_numbers = ref []
module FileMap = Map.Make(CilData.CilFile)
let file_to_line_targets = ref FileMap.empty

let get_line_targets file =
    try
        FileMap.find file (!file_to_line_targets)
    with Not_found ->
        let line_targets =
            List.fold_left begin fun targets (filename, linenumber as line) ->
                let stmts =
                    try
                        FindCil.stmts_by_line file line
                    with Not_found ->
                        let output_mode = Output.get_mode () in
                        Output.set_mode Output.MSG_REG;
                        Output.printf "Warning: line target %s:%d not found.@." filename linenumber;
                        Output.set_mode output_mode;
                        []
                in
                (* Use of_stmt_last to ensure that instructions before the last one are covered *)
                List.fold_left (fun targets (fundec, stmt) -> 
                    let instruction = Instruction.of_stmt_last file fundec stmt in
                    if ListPlus.mem Instruction.equal instruction targets then targets else instruction :: targets
                ) targets stmts
            end [] (!arg_line_numbers)
        in
        file_to_line_targets := FileMap.add file line_targets (!file_to_line_targets);
        line_targets

let add_line_target file instruction =
    let line_targets = instruction :: (get_line_targets file) in 
    file_to_line_targets := FileMap.add file line_targets (!file_to_line_targets)


let process_completed entry_fn (reason, job) =
    let instruction = Job.get_instruction job in
    let line_targets = get_line_targets job#file in
    let reason = 
        if ListPlus.mem Instruction.equal instruction line_targets then
            (* convert executions that report repeated abandoned paths to Truncated *)
            let reason = match reason with
                | Job.Abandoned (#Errors.t as reason) ->
                    let fundec = BackOtterUtilities.get_origin_function job in
                    (* Failing path has least recent decision first. See the comment in BidirectionalQueue. *)
                    let failing_path = DecisionPath.rev job#decision_path in
                    let is_new_path = BackOtterTargets.add_path fundec failing_path in
                    if is_new_path then Job.Abandoned (`TargetReached reason)
                    else Job.Truncated (`SummaryAbandoned reason)
                | _ ->
                    reason
            in reason
        else reason
    in
    let reason = 
        if BackOtterUtilities.get_origin_function job != entry_fn then
            (* convert executions from non-entry functions to Truncated *)
            match reason with
            | Job.Return return_code -> Job.Truncated (`SummaryReturn return_code)
            | Job.Exit return_code -> Job.Truncated (`SummaryExit return_code)
            | Job.Abandoned reason -> Job.Truncated (`SummaryAbandoned reason)
            | _ -> reason
        else reason
    in
    (job : _ #Info.t)#finish reason


(** {1 Command-line options} *)

let add_line_to_line_targets =
    (* [spaces]<filename>:<line number>[spaces] *)
    let re = Str.regexp "^[ \t]*\\(\\(.*\\):\\([0-9]*\\)\\)?[ \t]*$" in
    fun arg ->
        let arg =
            (* remove #-delimited comments *)
            try
                let stop = String.index arg '#' in
                String.sub arg 0 stop
            with Not_found ->
                arg
        in
        if Str.string_match re arg 0 then
            try
                let file = Str.matched_group 2 arg in
                let line = int_of_string (Str.matched_group 3 arg) in
                arg_line_numbers := (file, line)::(!arg_line_numbers)
            with Not_found ->
                () (* blank line *)
        else
            FormatPlus.failwith "Error in parsing line %s" arg

let options = [
    ("--line-targets",
        Arg.String begin fun str ->
            let args = Str.split (Str.regexp ",") str in
            List.iter add_line_to_line_targets args
        end,
        "<line[,lines]> Lines in the form file:linenum[,file:linenum...]. Default is empty list.\n");
    ("--line-targets-file",
        Arg.String begin fun filename ->
            let inChan = open_in filename in
            try
                while true do
                    add_line_to_line_targets (input_line inChan)
                done
            with End_of_file ->
                close_in inChan
        end,
        "<filename> File containing lines in the form file:linenum, one per line.\n");
]

