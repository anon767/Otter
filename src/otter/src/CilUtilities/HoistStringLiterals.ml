(** Hoist all string literals in a {!Cil.file} to [const char \[\]]. *)

open DataStructures

let apply file =
    let module StringMap = Map.Make (String) in
    let string_map = ref StringMap.empty in
    let string_counter = Counter.make () in
    let hoist_string string =
        try
            snd (StringMap.find string !string_map)
        with Not_found ->
            (* set up the const char[] definition and replacement expression *)
            let const_char_type = Cil.TInt (Cil.IChar, [ Cil.Attr ("const", []) ]) in
            let const_char_init i c =
                let index = Cil.Index (Cil.integer i, Cil.NoOffset) in
                let const = Cil.SingleInit (Cil.CastE (const_char_type, Cil.Const (Cil.CChr c))) in
                (index, const)
            in
            let length = String.length string + 1 in (* plus null *)
            let name = "__cil_hoisted_string" ^ string_of_int (Counter.next string_counter) in
            let typ = Cil.TArray (const_char_type, Some (Cil.integer length), []) in
            let varinfo = { (Cil.makeGlobalVar name typ) with Cil.vdescr = Pretty.dprintf "\"%s\"" string; Cil.vdescrpure = true } in
            let string_init =
                let rec string_init i init_list =
                    if i >= 0 then
                        string_init (i - 1) ((const_char_init i string.[i])::init_list)
                    else
                        init_list
                in
                Cil.CompoundInit (typ, string_init (length - 2) [ const_char_init (length - 1) '\000' ])
            in
            let global = Cil.GVar (varinfo, { Cil.init = Some string_init }, Cil.locUnknown) in
            let expr = Cil.mkAddrOrStartOf (Cil.var varinfo) in
            string_map := StringMap.add string (global, expr) !string_map;
            expr
    in

    (* first replace all string literals with address of const char[] *)
    Cil.visitCilFileSameGlobals begin object
        inherit Cil.nopCilVisitor
        method vexpr = function
            | Cil.Const (Cil.CStr s) ->
                Cil.ChangeTo (hoist_string s)
            | _ ->
                Cil.DoChildren
    end end file;

    (* then add the hoisted string variables *)
    StringMap.iter (fun _ (global, _) -> file.Cil.globals <- global::file.Cil.globals) !string_map

