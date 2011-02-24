(** Widen all bit-fields in a {!Cil.file} into standard fields *)

let apply file =
    Cil.iterGlobals
        file
        (function
             | Cil.GCompTag (compinfo, _) ->
                   List.iter (fun fieldinfo -> fieldinfo.Cil.fbitfield <- None) compinfo.Cil.cfields
             | _ -> ())
