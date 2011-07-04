open OcamlUtilities
open CilUtilities
open OtterCore
open Cil

module LineSet = Job.LineSet

let gcov_out = ref "./gcov.out/"
let gcov_paths = ref []
let arg_run_gcov = ref false

let get_lines =
    let module Memo = Memo.Make (CilData.CilFile) in
    Memo.memo "GCov.get_lines"
        begin fun file ->
            let vis = new Coverage.getStatsVisitor file in
            iterGlobals file (function GFun(fundec,_) -> ignore (visitCilFunction (vis:>cilVisitor) fundec) | _ -> ());
            vis#lines
        end

type line_status = Visit_count of int | Not_executable
let get_status_count = function Visit_count 0 -> "#####" | Visit_count n -> string_of_int n | Not_executable -> "-"
let update_status = function Visit_count(n) -> Visit_count(n+1) | Not_executable -> invalid_arg "non-executable line cannot be visited"

class gcovfile file filename = object (self)

    val mutable lines = Array.make 0 ("",Not_executable)
    val mutable filename_with_path = filename

    (* TODO: make use of path_id *)
    method update (path_id:int) linenum =
        let (line, status) = lines.(linenum) in
        let status = update_status status in
        lines.(linenum) <- (line, status)

    method flush =
        let open_out_with_mkdir filename =
            let dirname = Filename.dirname filename in
            UnixPlus.mkdir_p dirname 0o755;
            open_out filename
        in
        let outChan = open_out_with_mkdir (Filename.concat (!gcov_out) filename_with_path) in
        let f = Format.formatter_of_out_channel outChan in
        for i = 1 to Array.length lines - 1 do
            let (line, status) = lines.(i) in
            Format.fprintf f "%6s:%5d:%s\n" (get_status_count status) i line
        done;
        close_out outChan

    method set_lines lines' = lines <- lines'
    method set_filename_with_path f = filename_with_path <- f

    initializer
        let lineset = get_lines file in
        let rec get_file_inchan = function
            | [] ->
                Output.must_printf "Unable to find file %s" filename;
                Array.make 10000 ""
            | path :: paths -> (
                try
                    let filename_with_path = Filename.concat path filename in
                    let inChan = open_in filename_with_path in
                    self#set_filename_with_path filename_with_path;
                    let lines = ref [ "(Line zero)" ] in
                    (try while true do lines := (input_line inChan) :: (!lines) done with End_of_file -> close_in inChan);
                    Array.of_list (List.rev (!lines)) 

                with Sys_error _ -> get_file_inchan paths)
        in
        let lines = get_file_inchan (!gcov_paths) in
        let lines = Array.mapi (fun i line -> (line, if LineSet.mem (filename, i) lineset then Visit_count 0 else Not_executable)) lines in
        self#set_lines lines

end

module GcovHashtbl = Hashtbl.Make(struct
        type t = Cil.file * string
        let hash = Hashtbl.hash
        let equal (f1,s1) (f2,s2) = match String.compare s1 s2 with 0 -> CilData.CilFile.equal f1 f2 | _ -> false
    end)

let gcovs = GcovHashtbl.create 8

let get_gcovfile file filename =
    if not (GcovHashtbl.mem gcovs (file, filename)) then GcovHashtbl.add gcovs (file, filename) (new gcovfile file filename);
    GcovHashtbl.find gcovs (file, filename)


let flush_gcovfiles () =
    GcovHashtbl.iter (fun _ gcovfile -> gcovfile#flush) gcovs


let interceptor job interceptor =
    let loc = Job.get_loc job in
    if not (!arg_run_gcov) || loc = Cil.locUnknown then interceptor job
    else
        let file = job#file in
        let path_id = job#path_id in
        let filename = loc.file in
        let linenum = loc.line in
        let gcovfile = get_gcovfile file filename in
        gcovfile#update path_id linenum;
        interceptor job


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
