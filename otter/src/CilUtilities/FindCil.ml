(** Functions for searching {!Cil.file}. *)


(**/**)
module VarinfoSet = Set.Make (CilData.CilVar)
(**/**)


(** Get the memoization tables for searching in a file, initializing them if necessary.
        @param file the {!Cil.file} to get the memoization tables for
        @return [(varinfo_to_fundec, varinfo_to_varinit, name_to_fundec)] a tuple containing the memoization
                tables for searching the file
*)
let memotables =
    let file_memotables = Hashtbl.create 0 in
    fun file ->
        try
            Hashtbl.find file_memotables file
        with Not_found ->
            let varinfo_to_fundec = Hashtbl.create 100 in
            let varinfo_to_varinit = Hashtbl.create 100 in
            let name_to_fundec = Hashtbl.create 100 in
            let name_to_global_varinfo = Hashtbl.create 100 in
            let all_varinfos = ref VarinfoSet.empty in

            Cil.iterGlobals file begin function
                | Cil.GFun (fundec ,_) ->
                    Hashtbl.replace varinfo_to_fundec fundec.Cil.svar fundec;
                    Hashtbl.replace name_to_fundec fundec.Cil.svar.Cil.vname fundec;
                    List.iter (fun varinfo -> all_varinfos := VarinfoSet.add varinfo !all_varinfos) fundec.Cil.sformals;
                    List.iter (fun varinfo -> all_varinfos := VarinfoSet.add varinfo !all_varinfos) fundec.Cil.slocals;
                | Cil.GVar (varinfo, initinfo, _) ->
                    Hashtbl.replace varinfo_to_varinit varinfo initinfo;
                    Hashtbl.replace name_to_global_varinfo varinfo.Cil.vname varinfo;
                    all_varinfos := VarinfoSet.add varinfo !all_varinfos;
                | Cil.GVarDecl (varinfo, _) ->
                    Hashtbl.replace varinfo_to_varinit varinfo { Cil.init=None };
                    Hashtbl.replace name_to_global_varinfo varinfo.Cil.vname varinfo;
                    all_varinfos := VarinfoSet.add varinfo !all_varinfos;
                | _ -> ()
            end;

            let all_varinfos = VarinfoSet.elements !all_varinfos in

            let memotables = object
                method varinfo_to_fundec = Hashtbl.find varinfo_to_fundec
                method varinfo_to_varinit = Hashtbl.find varinfo_to_varinit
                method name_to_fundec = Hashtbl.find name_to_fundec
                method name_to_global_varinfo = Hashtbl.find name_to_global_varinfo
                method all_varinfos = all_varinfos
            end in
            Hashtbl.replace file_memotables file memotables;
            memotables


(** Find a {!Cil.fundec} by {!Cil.varinfo} from a {!Cil.file}.
        @param file the {!Cil.file} to find the {!Cil.fundec} in
        @param varinfo the {!Cil.varinfo} of the {!Cil.fundec} to find
        @return the {!Cil.fundec}
        @raise Not_found if a {!Cil.fundec} for [varinfo] does not exist in [file]
*)
let fundec_by_varinfo file varinfo =
    (memotables file)#varinfo_to_fundec varinfo


(** Find the {!Cil.initinfo} for a {!Cil.varinfo} from a {!Cil.file}.
        @param file the {!Cil.file} to find the {!Cil.initinfo} in
        @param varinfo the {!Cil.varinfo} of the {!Cil.initinfo} to find
        @return the {!Cil.initinfo}
        @raise Not_found if a global [varinfo] does not exist in [file]
*)
let global_varinit_by_varinfo file varinfo =
    (memotables file)#varinfo_to_varinit varinfo


(** Find a {!Cil.fundec} by name from a {!Cil.file}.
        @param file the {!Cil.file} to find the {!Cil.fundec} in
        @param name the name of the {!Cil.fundec} to find
        @return the {!Cil.fundec}
        @raise Not_found if a {!Cil.fundec} named [name] does not exist in [file]
*)
let fundec_by_name file name =
    (memotables file)#name_to_fundec name


(** Find a global {!Cil.varinfo} by name from a {!Cil.file}.
        @param file the {!Cil.file} to find the {!Cil.fundec} in
        @param name the name of the {!Cil.fundec} to find
        @return the {!Cil.varinfo}
        @raise Not_found if a global {!Cil.varinfo} named [name] does not exist in [file]
*)
let global_varinfo_by_name file name =
    (memotables file)#name_to_global_varinfo name


(** Return a list of all {!Cil.varinfo} in a {!Cil.file}.
        @param file the {!Cil.file} to find the {!Cil.fundec} in
        @return the list of {!Cil.varinfo}
*)
let all_varinfos file =
    (memotables file)#all_varinfos

