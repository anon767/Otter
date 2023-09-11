open OcamlUtilities
open CilUtilities
open OtterCFG
open OtterCore
open Job
open Cil

let arg_line_numbers = ref []
let arg_convert_non_target_reached_abandoned_to_truncated = ref false
let arg_remove_line_targets_once_found = ref false
let arg_fork_finish_at_targets = ref false

module FileMap = Map.Make(CilData.CilFile)
module InstructionSet = Set.Make(Instruction)

(* An abstraction of CilLocation: only file names and line numbers are considered. *)
module Location = struct
    type t = { file: string; line: int }
    let compare { file = f1; line = l1 } { file = f2; line = l2 } = match Pervasives.compare l1 l2 with 0 -> Pervasives.compare f1 f2 | i -> i
    let of_cilloc { Cil.file = f; Cil.line = l } = { file = f; line = l }
    let of_instruction instruction = of_cilloc (Instruction.location instruction)
    let printer ff loc = Format.fprintf ff "%s:%d" loc.file loc.line
end

(* Mapping a location to a set of instructions located in that location *)
module LocationMap = struct
    module M = Map.Make(Location)
    include M
    let add_to_set loc instr map =
        let iset = try find loc map with Not_found -> InstructionSet.empty in
        let iset = InstructionSet.add instr iset in
        M.add loc iset map
end

(* A global map *)
let file_to_line_targets = ref FileMap.empty

let get_line_targets_loc_map file =
    try
        FileMap.find file (!file_to_line_targets)
    with Not_found ->
        let line_targets =
            List.fold_left begin fun targets (filename, linenumber as line) ->
                try
                    (Instruction.by_line file line) @ targets
                with Not_found ->
                    Output.must_printf "Warning: line target %s:%d not found.@." filename linenumber;
                    targets
            end [] (!arg_line_numbers)
        in
        let loc_map = List.fold_left (
            fun loc_map line_target -> 
                let loc = Location.of_instruction line_target in
                Output.must_printf "Add target at %a@\n" Location.printer loc;
                LocationMap.add_to_set loc line_target loc_map
            ) LocationMap.empty line_targets in
        file_to_line_targets := FileMap.add file loc_map (!file_to_line_targets);
        loc_map

(* Flatten the map to a list of instructions *)
let get_line_targets file =
    let loc_map = get_line_targets_loc_map file in
    LocationMap.fold (fun _ iset line_targets -> InstructionSet.fold (fun i line_targets -> i :: line_targets) iset line_targets) loc_map []

let add_line_target file instruction =
    let loc_map = LocationMap.add_to_set (Location.of_instruction instruction) instruction (get_line_targets_loc_map file) in 
    file_to_line_targets := FileMap.add file loc_map (!file_to_line_targets)

let update_flag = ref false
let remove_count = ref 0

let remove_line_target_loc file loc =
    let loc_map = get_line_targets_loc_map file in
    let loc_map = LocationMap.remove loc loc_map in
    Output.must_printf "Remove target at %a@\n" Location.printer loc;
    update_flag := true;
    remove_count := 1 + !remove_count;
    file_to_line_targets := FileMap.add file loc_map (!file_to_line_targets)

let process_completed entry_fn (reason, job) =
    (* Note: __FAILURE()'s first line is added into line_targets in BackOtterMain *)
    let locs = List.map Location.of_cilloc ((Job.get_loc job) :: (List.map InstructionInfo.Entry.to_loc job#caller_list)) in
    let line_targets_loc_map = get_line_targets_loc_map job#file in 

    (* The job reaches some target if its current location, or any call site along the call stack, reaches the target. *)
    let rec is_target = function 
        | [] -> false 
        | loc :: locs -> 
            if LocationMap.mem loc line_targets_loc_map then begin
                (* Remove line_targets associated with this loc *)
                if (!arg_remove_line_targets_once_found) then remove_line_target_loc job#file loc;
                true
            end else is_target locs 
    in

    (* Track reached targets, and convert executions that report repeated abandoned paths to Truncated *)
    let reason =
        if is_target locs then
            let reason = match reason with
                | Job.Abandoned (#Errors.t as reason) ->
                    let fundec = BackOtterUtilities.get_origin_function job in
                    (* Failing path has least recent decision first. See the comment in BidirectionalQueue. *)
                    let failing_path = DecisionPath.rev job#decision_path in
                    let is_new_path = BackOtterTargets.add_path fundec failing_path in
                    if is_new_path then 
                        begin
                            Output.set_mode Output.MSG_REPORT;
                            Output.printf "@\nFailing path(s) for %s:@." fundec.svar.vname;
                            Output.printf "Failing path: @[%a@]@." DecisionPath.print failing_path;
                            Job.Abandoned (`TargetReached reason)
                        end
                    else Job.Truncated (`SummaryAbandoned reason)
                | _ ->
                    reason
            in reason
        else 
            let _ = match reason with
                | Job.Abandoned (#Errors.t as reason) ->
                    Output.set_mode Output.MSG_REPORT;
                    Output.printf "BackOtterTargetTracker.process_completed (not target): failwith @[%a@]@." BackOtterErrors.printer reason
                | _ -> ()
            in reason
    in
    (* Convert executions from non-entry functions to Truncated *)
    let reason = 
        if BackOtterUtilities.get_origin_function job != entry_fn then
            match reason with
            | Job.Return return_code -> Job.Truncated (`SummaryReturn return_code)
            | Job.Exit return_code -> Job.Truncated (`SummaryExit return_code)
            | Job.Abandoned reason -> Job.Truncated (`SummaryAbandoned reason)
            | _ -> reason
        else reason
    in
    (* Convert non-TargetReached Abandoned reason to Truncated, if arg_convert_non_target_reached_abandoned_to_truncated is true *)
    let reason = 
        if !arg_convert_non_target_reached_abandoned_to_truncated then
            match reason with
            | Job.Abandoned (`TargetReached _) -> reason
            | Job.Abandoned reason -> Job.Truncated reason
            | _ -> reason
        else reason
    in
    (job : _ #Info.t)#finish reason


(* Detect if a target is reached, and if arg_fork_finish_at_targets is set,
 * fork a completed job unconditionally. *)
let interceptor job interceptor = 
    if (!arg_fork_finish_at_targets) then
        let line_targets_loc_map = get_line_targets_loc_map job#file in 
        let loc = Location.of_cilloc (Job.get_loc job) in
        let job = if LocationMap.mem loc line_targets_loc_map then (job : _ #Info.t)#fork_finish (Job.Abandoned (`Failure "Target Reached")) else job in
        interceptor job
    else
        interceptor job

(** {1 Command-line options} *)

(* TODO: combine this with add_line_target *)
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

let add_gcov_line_to_line_targets =
    let re = Str.regexp "^ *#####: *\\([0-9]*\\):.*$" in
    let src = Str.regexp "^        -:    0:Source:\\(.*\\)$" in
    let file = ref "" in
    fun arg ->
        begin if Str.string_match src arg 0 then
            try
                file := Str.matched_group 1 arg
            with Not_found -> ()
        end;
        begin if Str.string_match re arg 0 then
            try
                let line = int_of_string (Str.matched_group 1 arg) in
                arg_line_numbers := (!file, line)::(!arg_line_numbers)
            with Not_found -> ()
        end

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
    ("--line-targets-from-gcov",
        Arg.String begin fun filename ->
            let inChan = open_in filename in
            try
                while true do
                    add_gcov_line_to_line_targets (input_line inChan)
                done
            with End_of_file ->
                close_in inChan
        end,
        "<filename> gcov output showing what lines are uncovered. These lines will be marked as targets.\n");
    ("--convert-non-target-reached-abandoned-to-truncated",
        Arg.Set arg_convert_non_target_reached_abandoned_to_truncated,
        " Convert non TargetReached Abandoned's to Truncated's (default: no)");
    ("--remove-line-targets-once-found",
        Arg.Set arg_remove_line_targets_once_found,
        " Remove line targets once found, so that they no longer are targets in the future");
    ("--fork-finish-at-targets",
        Arg.Set arg_fork_finish_at_targets,
        " Always fork a finished path at target.");
]

