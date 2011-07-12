open OtterExtensions.Gcov
open Cil

module GcovHashtbl = Hashtbl.Make(struct
        type t = Cil.file * Cil.fundec * string
        let hash = Hashtbl.hash
        let equal (f1,c1,s1) (f2,c2,s2) = match String.compare s1 s2 with 0 -> (match CilUtilities.CilData.CilFile.compare f1 f2 with 0 -> CilUtilities.CilData.CilFundec.equal c1 c2 | _ -> false) | _ -> false
    end)

let gcovs = GcovHashtbl.create 8

let get_gcovfile file fundec filename =
    if not (GcovHashtbl.mem gcovs (file, fundec, filename)) then GcovHashtbl.add gcovs (file, fundec, filename) (new gcovfile file filename (Filename.concat (!gcov_out) fundec.svar.vname));
    GcovHashtbl.find gcovs (file, fundec, filename)


let flush_gcovfiles () =
    GcovHashtbl.iter (fun _ gcovfile -> gcovfile#flush) gcovs


let interceptor job interceptor =
    let loc = OtterCore.Job.get_loc job in
    if not (!arg_run_gcov) || loc = Cil.locUnknown then interceptor job
    else
        let file = job#file in
        let origin_fundec = BackOtterUtilities.get_origin_function job in
        let node_id = job#node_id in
        let filename = loc.file in
        let linenum = loc.line in
        let gcovfile = get_gcovfile file origin_fundec filename in
        gcovfile#update node_id linenum;
        interceptor job

