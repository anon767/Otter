open Cil

let doit (file: Cil.file) =
    Format.printf "Run Cil's Reaching Definitions@\n";
    let vidVarinfoHash = Hashtbl.create 0 in
    Cil.visitCilFileSameGlobals begin object
        inherit Cil.nopCilVisitor
        method vvdec varinfo =
            Hashtbl.add vidVarinfoHash varinfo.vid varinfo;
            Cil.SkipChildren
    end end file;
    List.iter 
        begin function 
            | GFun(fundec,_) ->
                Format.printf "Function %s@\n" fundec.svar.vname;
                Cil.prepareCFG fundec;
                Cil.computeCFGInfo fundec false;
                Reachingdefs.computeRDs fundec;
                List.iter
                    begin fun stmt ->
                        Format.printf "----------------------------------------------------------@\n";
                        Format.printf "Stmt %d: %a@\n" stmt.sid Printcil.stmt stmt;
                        Format.printf " * * * * *@\n";
                        match Reachingdefs.getRDs stmt.sid with
                        | Some (_,_,vidmap) -> 
                            Reachingdefs.IH.iter 
                                begin fun id set ->
                                    Format.printf ":varinfo: %s@\n" (Hashtbl.find vidVarinfoHash id).vname;
                                    Reachingdefs.IOS.iter (
                                        function
                                            | Some defId -> 
                                                let _ = Format.printf "::DefId: %d, " defId in
                                                let _ = match Reachingdefs.getSimpRhs defId with
                                                    | Some (Reachingdefs.RDExp exp) -> Format.printf "exp: %a@\n" Printcil.exp exp
                                                    | Some (Reachingdefs.RDCall instr) -> Format.printf "call: %a@\n" Printcil.instr instr
                                                    | None -> Format.printf "StmtID: None@\n"
                                                in
                                                ()
                                            | None -> Format.printf "::DefId: None@\n" 
                                    ) set
                                end vidmap
                        | None -> Format.printf ":(None)@\n"
                    end
                    fundec.sallstmts

            | _ -> ()
        end
        file.globals

let feature : featureDescr = {
    fd_name = "RunReachingDef";
    fd_enabled = ref false;
    fd_description = "Run Cil's Reaching Definitions";
    fd_extraopt = [];
    fd_post_check = true;
    fd_doit = doit;
}

