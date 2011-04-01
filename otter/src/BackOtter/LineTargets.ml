open OcamlUtilities
open CilUtilities 
open OtterCore
open OtterCFG
open State
open Job
open Cil

let arg_line_targets = ref []

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
            end [] (!arg_line_targets)
        in
        file_to_line_targets := FileMap.add file line_targets (!file_to_line_targets);
        line_targets


(** An interceptor that tracks targets when some (file, line) in arg_line_targets is encountered. *)
let line_target_interceptor job interceptor =
    let instruction = Job.get_instruction job in
    let line_targets = get_line_targets job#file in
    begin if ListPlus.mem Instruction.equal instruction line_targets then
        let loc = Job.get_loc job in
        let _ = Output.must_printf "TargetReached %s:%d@\n" loc.Cil.file loc.Cil.line in
        let fundec = BackOtterUtilities.get_origin_function job in
        let entryfn = ProgramPoints.get_entry_fundec job#file in
        let failing_path = DecisionPath.rev job#decision_path in
        let _ = BackOtterTargets.add_path fundec failing_path (Some instruction) in (* TODO: do we care if the path is new or not? *)
        if CilData.CilFundec.equal fundec entryfn then
            (* Remove instruction from line_targets *)
            let _ = Output.must_printf "Remove target %s:%d@\n" loc.Cil.file loc.Cil.line in
            let _ = file_to_line_targets := FileMap.add job#file (List.filter (fun i -> not (Instruction.equal i instruction)) line_targets) (!file_to_line_targets) in
            (* Remove instruction-related failing paths and function targets from BackOtterTargets *)
            BackOtterTargets.remove_target_instruction instruction
    end;
    (* Bounded jobs will eventually be removed, due to "out bound" (Bidirectional.ml) *)
    interceptor job


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
                arg_line_targets := (file, line)::(!arg_line_targets)
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
