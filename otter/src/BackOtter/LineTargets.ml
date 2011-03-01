open OcamlUtilities
open OtterCore
open State
open Job
open Cil

let arg_line_targets = ref []

(** An interceptor that emits FailureReached when some (file, line) in arg_line_targets is encountered. *)
let line_target_interceptor job job_queue interceptor =
    let loc = Job.get_loc job in
    if List.mem (loc.Cil.file, loc.Cil.line) (!arg_line_targets) then
        Complete (Abandoned (`FailureReached, job)), job_queue
    else
        interceptor job job_queue


let get_line_targets =
    let module Memo = Memo.Make (CilUtilities.CilData.CilFile) in
    Memo.memo "LineTargets.get_line_targets"
        begin fun file ->
            let line_targets = ref [] in
            Cil.visitCilFileSameGlobals begin object
                inherit Cil.nopCilVisitor
                method vfunc fundec =
                    List.iter (fun stmt ->
                        let loc = Cil.get_stmtLoc stmt.skind in
                        begin if List.mem (loc.Cil.file, loc.Cil.line) (!arg_line_targets) then
                            line_targets := (OtterCFG.Instruction.of_stmt_first file fundec stmt)::(!line_targets)
                        end
                    ) fundec.sallstmts;
                    Cil.SkipChildren
            end end file;
            !line_targets
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
