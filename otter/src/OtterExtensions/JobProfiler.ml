open OcamlUtilities
open OtterCore

let gcov_out = ref "./gcov.out/"
let gcov_paths = ref []
let arg_run_gcov = ref false
let arg_min_report_time = ref 1.

module Entry = InstructionInfo.Entry

module type KeyType = sig
    type t
    val hash : t -> int
    val equal : t -> t -> bool
    val of_job : ((_,_) #Job.t -> t)
    val to_string : t -> string
end

module EntryTree = struct
    module type RecordType = sig 
        type t 
        val zero : t 
        val merge : t -> t -> t 
        val merge_caller : t -> t -> t 
    end

    module Make (Record : RecordType) = struct
        module EntryMap = Map.Make(Entry)

        type t = {
            record : Record.t;
            children : t EntryMap.t;
        }

        let empty = {
            record = Record.zero;
            children = EntryMap.empty;
        }

        let update callstack entry record tree = Profiler.global#call "EntryTree.update" begin fun () ->
            let rec update path tree =
                match path with 
                | [] -> assert(false)
                | entry :: [] -> 
                    { tree with
                        record = Record.merge record tree.record;
                    }
                | parent :: child :: path ->
                    let tree' = try EntryMap.find child tree.children with Not_found -> empty in
                    let tree' = update (child :: path) tree' in
                    {
                        record = Record.merge_caller record tree.record;
                        children = EntryMap.add child tree' tree.children;
                    }
            in
            update (List.rev (entry :: callstack)) tree
        end

        let rec preorder ff node tree depth acc = 
            let acc = ff node tree.record depth acc in
            EntryMap.fold (fun entry tree acc -> preorder ff entry tree (depth+1) acc) tree.children acc

        let rec postorder ff node tree depth acc = 
            let acc = EntryMap.fold (fun entry tree acc -> postorder ff entry tree (depth+1) acc) tree.children acc in
            ff node tree.record depth acc

        let fold ff tree acc = postorder (fun node record _ -> ff node record) Entry.dummy tree 0 acc
    end
end

module CilLines = struct
    open OtterCFG
    open Cil
    module LineMap = Map.Make(CoverageData.LineData)
    class getStatsVisitor file = object (self)
        val lines = ref LineMap.empty
        val reachable_stmts = Hashtbl.create 0
        method private is_reachable_stmt stmt =
            if Hashtbl.length reachable_stmts = 0 then
                (* Prepare the table *)
                let source =
                    let mainFunc = ProgramPoints.get_main_fundec file in 
                    Instruction.of_fundec file mainFunc
                in
                let processed_instrs = Hashtbl.create 0 in
                let rec visit instruction =
                    if Hashtbl.mem processed_instrs instruction then
                        ()
                    else (
                        Hashtbl.add processed_instrs instruction true;
                        Hashtbl.add reachable_stmts instruction.Instruction.stmt true;
                        List.iter visit (Instruction.successors instruction);
                        List.iter visit (Instruction.call_sites instruction);
                        ()
                    )
                in
                visit source
            else ()
            ;
            try
                Hashtbl.find reachable_stmts stmt
            with Not_found -> false
    
        method lines = !lines
    
        inherit nopCilVisitor
    
        method vinst instr =
            let loc = get_instrLoc instr in
            lines := LineMap.add (loc.file,loc.line) (FormatPlus.sprintf "%a" CilUtilities.CilPrinter.instr_abbr instr) !lines;
            SkipChildren (* There's nothing interesting under an [instr] *)
    
        method vstmt stmt =
            if self#is_reachable_stmt stmt then (
                let loc = get_stmtLoc stmt.skind in
                lines := LineMap.add (loc.file,loc.line) (FormatPlus.sprintf "%a" CilUtilities.CilPrinter.stmt_abbr stmt) !lines;
            );
            DoChildren (* There could be stmts or instrs inside, which we should visit *)
    end
    let get_lines =
        let module Memo = Memo.Make (CilUtilities.CilData.CilFile) in
        Memo.memo "JobProfiler.get_lines"
            begin fun file ->
                let vis = new getStatsVisitor file in
                Cil.iterGlobals file (function Cil.GFun(fundec,_) -> ignore (Cil.visitCilFunction (vis:>Cil.cilVisitor) fundec) | _ -> ());
                vis#lines
            end
    let file = ref Cil.dummyFile
    let set_file file' = file := file'
    let is_line filename linenum = LineMap.mem (filename, linenum) (get_lines !file)
    let get_line filename linenum = if is_line filename linenum then LineMap.find (filename, linenum) (get_lines !file) else "-"
end

module Profile = struct

    module Int = Module.Int
    module IntSet = Set.Make (Int)

    module Record = struct 
        type t = {
            time : float;
            nodes : IntSet.t;
        }
        let zero = {
            time = 0.;
            nodes = IntSet.empty;
        }
        let merge r1 r2 = {
            time = r1.time +. r2.time;
            nodes = IntSet.union r1.nodes r2.nodes;
        }
        let merge_caller r_new r_old = {
            time = r_new.time +. r_old.time;
            nodes = r_old.nodes;
        }
    end
    module EntryTree = EntryTree.Make (Record)

    type t = {
        name : string;
        tree : EntryTree.t;
    }
    let create name = {
        name = name;
        tree = EntryTree.empty;
    }

    let update profile job time = 

        let record = {
            Record.time = time;
            Record.nodes = IntSet.singleton job#node_id;
        } in { profile with
            (* TODO: combine caller_list and job's current loc *)
            tree = EntryTree.update job#caller_list (Job.get_loc job(*ugly*)) record profile.tree; 
        }

    let flush profile = 
        if !arg_run_gcov then begin

            let top_down_printer ff tree = 
                let entry_printer ff loc = 
                    let label = CilLines.get_line loc.Cil.file loc.Cil.line in
                    if loc == Cil.locUnknown then
                        Format.fprintf ff "%s" label
                    else
                        let loc_label = OcamlUtilities.FormatPlus.sprintf "%a" Printcil.loc loc in
                        let max_length = 26 in
                        let length = String.length loc_label in
                        let loc_label = if length > max_length then "..."^(String.sub loc_label (length - max_length) max_length) else loc_label in
                        Format.fprintf ff "%s (%s)" label loc_label 
                in
                let _ = Format.fprintf ff "%8s\t%s@\n" "time(s)" "Line (Location)" in
                EntryTree.preorder (
                    fun entry record depth _ -> 
                        let time = record.Record.time in
                        if time >= !arg_min_report_time then
                            Format.fprintf ff "%8.1f\t%*s%a@\n" time (2*depth) "" entry_printer entry
                ) Entry.dummy tree 0 () in


            let old_formatter = !Output.formatter in
            Output.set_formatter (new Output.plain);
            Output.set_mode Output.MSG_REPORT;
            Output.printf "@\n";
            Output.printf "Top-down C execution profile for %s: @\n" profile.name;
            Output.printf "%a" top_down_printer profile.tree;
            Output.printf "@\n";
            Output.formatter := old_formatter;


            (* Produce gcov-like output *)
            let module StringMap = Map.Make (String) in
            let module IntMap = Map.Make (Int) in
            let module Line = Module.CombineOrderedTypes (String) (Int) in
            let module LineMap = Map.Make(Line) in

            (* Collapse different locations of same (file, line) into one *)
            let line_map = EntryTree.fold begin
                fun entry record line_map -> 
                    (* Discard unknown locations *)
                    if Entry.equal entry Entry.dummy then line_map 
                    else
                        let loc = Entry.to_loc entry in
                        let line = loc.Cil.file, loc.Cil.line in
                        if LineMap.mem line line_map then
                            let record' = LineMap.find line line_map in
                            LineMap.add line (Record.merge record record') line_map
                        else
                            LineMap.add line record line_map
            end profile.tree LineMap.empty in

            (* Collapse different lines of same file *)
            let string_map = LineMap.fold begin
                fun (filename, linenum) record string_map -> 
                    let linemap = if StringMap.mem filename string_map then StringMap.find filename string_map else IntMap.empty in
                    assert (not (IntMap.mem linenum linemap));
                    let linemap = IntMap.add linenum record linemap in
                    StringMap.add filename linemap string_map
            end line_map StringMap.empty in

            StringMap.iter begin
                fun filename linemap -> (* output to files *)
                    (* Read original source code *)
                    let rec get_file_inchan = function
                        | [] ->
                            Output.must_printf "Unable to find file %s@\n" filename;
                            Array.make 10000 ""
                        | path :: paths -> (
                            try
                                let filename_with_path = Filename.concat path filename in
                                let inChan = open_in filename_with_path in
                                let lines = ref [ "(Line zero)" ] in
                                (try while true do lines := (input_line inChan) :: (!lines) done with End_of_file -> close_in inChan);
                                Array.of_list (List.rev (!lines)) 
                            with Sys_error _ -> get_file_inchan paths)
                    in
                    let lines = get_file_inchan (!gcov_paths) in
                    (* Output gcov-like output *)
                    let open_out_with_mkdir filename =
                        let dirname = Filename.dirname filename in
                        UnixPlus.mkdir_p dirname 0o755;
                        open_out filename
                    in
                    let output_filename = Filename.concat !gcov_out (Filename.concat profile.name filename) in (*TODO: use filename_with_path above *)
                    let outChan = open_out_with_mkdir output_filename in
                    let f = Format.formatter_of_out_channel outChan in
                    for i = 1 to Array.length lines - 1 do
                        let line = lines.(i) in
                        let cov, time = 
                            if not (CilLines.is_line filename i) then "-", ""
                            else try
                                let record = IntMap.find i linemap in
                                let cov_size = IntSet.cardinal record.Record.nodes in
                                let cov = Printf.sprintf "%d" cov_size in
                                let time = Printf.sprintf "%.1f" record.Record.time in
                                cov, time
                            with Not_found -> "#####", ""
                        in
                        Format.fprintf f "%7s:%7s:%7d:%s\n" cov time i line
                    done;
                    close_out outChan

            end string_map
        end
end

module Make (Key : KeyType) = struct

    module Hashtbl = Hashtbl.Make (Key)
    let hashtbl : Profile.t Hashtbl.t = Hashtbl.create 8
    let last_profile_update = ref None

    let interceptor job interceptor = 
        if !arg_run_gcov then begin
            CilLines.set_file job#file; (* TODO: avoid this *)
            Profiler.global#call "JobProfiler.interceptor" begin fun () ->
                let time = Unix.gettimeofday () in
                begin match !last_profile_update with
                | None -> ()
                | Some (key, profile, time') ->
                    let profile = profile (time -. time') in
                    Hashtbl.replace hashtbl key profile
                end;
                (* This job will be processed by next call to this interceptor *)
                let key = Key.of_job job in
                let profile = try Hashtbl.find hashtbl key with Not_found -> Profile.create (Key.to_string key) in
                let profile = Profile.update profile job in
                last_profile_update := Some (key, profile, time)
            end
        end;
        interceptor job

    let flush () = Hashtbl.iter (fun _ -> Profile.flush) hashtbl
end

let options = [
    "--gcov",
        Arg.Set arg_run_gcov,
        " Output gcov-like statistics";
    "--gcov-out",
        Arg.Set_string gcov_out,
        Printf.sprintf "<dir> Set the directory storing gcov-like statistics (default: %s)" (!gcov_out);
    "--gcov-path",
        Arg.String begin fun str ->
            let args = Str.split (Str.regexp ":") str in
            gcov_paths := "" :: args
        end,
        "<path[:path]> The path where files are found when processing gcov-like statistics.";
]
