open OcamlUtilities
open OtterCore
open State
open Job
open Cil

let arg_line_targets = ref []

(** An interceptor that tracks targets when some (file, line) in arg_line_targets is encountered. *)
let line_target_interceptor job job_queue interceptor =
    let loc = Job.get_loc job in
    let line = (loc.Cil.file, loc.Cil.line) in
    begin if List.mem line (!arg_line_targets) then
        let _ = Output.must_printf "TargetReached %s:%d@\n" loc.Cil.file loc.Cil.line in
        let fundec = BackOtterUtilities.get_origin_function job in
        let entryfn = ProgramPoints.get_entry_fundec job#file in
        let failing_path = List.rev job#decision_path in
        BackOtterTargets.add_path fundec failing_path;
        if CilUtilities.CilData.CilFundec.equal fundec entryfn then (* TODO: remove this target and related function targets *)()
    end;
    (* FIXME: for bounded jobs, they *should* be terminated once reaching line targets, or else we will have copies of the same job running...
     *         OR  we change the meaning of bounded jobs? *)
    interceptor job job_queue


let get_line_targets =
    let open CilUtilities in
    let module Memo = Memo.Make (CilData.CilFile) in
    Memo.memo "LineTargets.get_line_targets"
        begin fun file ->
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
                let targets' = List.map (fun (fundec, stmt) -> OtterCFG.Instruction.of_stmt_first file fundec stmt) stmts in (* TODO: remove duplicates *)
                targets' @ targets
            end [] (!arg_line_targets)
        end


(** {1 Command-line options} *)

let add_line_to_line_targets =
    let re = Str.regexp "\\(.*\\):\\(.*\\)" in
    fun arg ->
        if Str.string_match re arg 0 then
            let file = Str.matched_group 1 arg in
            let line = int_of_string (Str.matched_group 2 arg) in
            arg_line_targets := (file, line)::(!arg_line_targets)
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
