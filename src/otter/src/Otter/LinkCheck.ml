open Cil

module VarinfoSet = Set.Make(CilUtilities.CilData.CilVar)

let doit (file: Cil.file) =
    Rmtmps.removeUnusedTemps ~isRoot:Rmtmps.isCompleteProgramRoot file;

    let vardecl_set = ref VarinfoSet.empty in
    let fundec_set = ref VarinfoSet.empty in

    Cil.visitCilFileSameGlobals begin object
        inherit Cil.nopCilVisitor
        method vglob global =
            begin match global with
            | GVarDecl (varinfo, _) -> 
                begin match varinfo.vtype with
                | TFun _ -> vardecl_set := VarinfoSet.add varinfo (!vardecl_set)
                | _ -> ()
                end
            | GFun (fundec, _) -> fundec_set := VarinfoSet.add fundec.svar (!fundec_set)
            | _ -> ()
            end;
            Cil.SkipChildren
    end end file;
    VarinfoSet.iter (fun varinfo -> 
        if not (VarinfoSet.mem varinfo (!fundec_set) || 
               (OtterCore.BuiltinFunctions.is_builtin varinfo.vname)) then 
            (Format.printf "Error: function %s does not exist@\n" varinfo.vname; exit 1)
        else ()
        ) (!vardecl_set) 

let feature : featureDescr = {
    fd_name = "LinkCheck";
    fd_enabled = ref false;
    fd_description = "Exit with error if undefined functions exist (except builtins)";
    fd_extraopt = [];
    fd_post_check = false;
    fd_doit = doit;
}
