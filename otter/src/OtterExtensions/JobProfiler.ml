open OcamlUtilities
open OtterCore

let gcov_out = ref "./gcov.out/"
let gcov_paths = ref []
let arg_run_gcov = ref false

module type KeyType = sig
    type t
    val hash : t -> int
    val equal : t -> t -> bool
    val of_job : ((_,_) #Job.t -> t)
    val to_string : t -> string
end

module LocationTree = struct
    module type RecordType = sig 
        type t 
        val zero : t 
        val merge : t -> t -> t 
        val merge_caller : t -> t -> t 
    end

    module Make (Record : RecordType) = struct
        module Location = CilUtilities.CilData.CilLocation
        module LocationMap = Map.Make(Location)

        type t = {
            record : Record.t;
            children : t LocationMap.t;
        }

        let empty = {
            record = Record.zero;
            children = LocationMap.empty;
        }

        let update callstack loc record tree = Profiler.global#call "LocationTree.update" begin fun () ->
            let rec update path tree =
                match path with 
                | [] -> assert(false)
                | loc :: [] -> 
                    { tree with
                        record = Record.merge record tree.record;
                    }
                | parent :: child :: path ->
                    let tree' = try LocationMap.find child tree.children with Not_found -> empty in
                    let tree' = update (child :: path) tree' in
                    {
                        record = Record.merge_caller record tree.record;
                        children = LocationMap.add child tree' tree.children;
                    }
            in
            update (List.rev (loc :: callstack)) tree
        end

        let rec preorder (ff : Cil.location -> Record.t -> 'a -> 'a) ?(node=Cil.locUnknown) tree acc = 
            let acc = ff node tree.record acc in
            LocationMap.fold (fun loc tree acc -> preorder ff ~node:loc tree acc) tree.children acc

        let rec postorder (ff : Cil.location -> Record.t -> 'a -> 'a) ?(node=Cil.locUnknown) tree acc = 
            let acc = LocationMap.fold (fun loc tree acc -> postorder ff ~node:loc tree acc) tree.children acc in
            ff node tree.record acc

        let fold = postorder
    end
end

module CilLines = struct
    module LineSet = Job.LineSet
    let get_lines =
        let module Memo = Memo.Make (CilUtilities.CilData.CilFile) in
        Memo.memo "JobProfiler.get_lines"
            begin fun file ->
                let vis = new Coverage.getStatsVisitor file in
                Cil.iterGlobals file (function Cil.GFun(fundec,_) -> ignore (Cil.visitCilFunction (vis:>Cil.cilVisitor) fundec) | _ -> ());
                vis#lines
            end
    let file = ref Cil.dummyFile
    let set_file file' = file := file'
    let is_line filename linenum = LineSet.mem (filename, linenum) (get_lines !file)
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
    module LocationTree = LocationTree.Make (Record)

    type t = {
        name : string;
        tree : LocationTree.t;
    }
    let create name = {
        name = name;
        tree = LocationTree.empty;
    }

    let update profile job time = 
        let record = {
            Record.time = time;
            Record.nodes = IntSet.singleton job#node_id;
        } in { profile with
            tree = LocationTree.update job#caller_list (Job.get_loc job) record profile.tree; 
        }

    let flush profile = 
        if !arg_run_gcov then begin
            (* Produce gcov-like output *)
            let module StringMap = Map.Make (String) in
            let module IntMap = Map.Make (Int) in
            let module Line = Module.CombineOrderedTypes (String) (Int) in
            let module LineMap = Map.Make(Line) in

            (* Collapse different locations of same (file, line) into one *)
            let line_map = LocationTree.fold begin
                fun loc record line_map -> 
                    (* Discard unknown locations *)
                    if loc == Cil.locUnknown then line_map 
                    else
                        let line = (loc.Cil.file, loc.Cil.line) in
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
