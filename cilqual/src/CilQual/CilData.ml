open Cil


module CilType = struct
    type t = typ

    let is_or_points_to_function t =
        let rec is_or_points_to_function = function
            | Cil.TSFun (_, _, _, _) -> true
            | Cil.TSPtr (t, _) -> is_or_points_to_function t
            | _ -> false
        in
        is_or_points_to_function (Cil.typeSig t)

    let ref t = TPtr (t, [])
    let deref = function
        | TPtr (t, _) -> t
        | _ -> failwith "TODO: report deref of non-pointer Cil.typ"

    let is_cil_inserted_type typ = Cil.hasAttribute Config.cil_inserted_attribute_string (Cil.typeAttrs typ)
end

module CilVar = struct
    type t = varinfo
    let compare x y = if x == y then 0 else compare x.vid y.vid
    let hash x = x.vid
    let equal x y = compare x y = 0
    let printer ff x = Format.fprintf ff "%s" x.vname
end

module CilField = struct
    type t = fieldinfo
    let compare x y = if x == y then 0 else compare (x.fcomp.ckey, x.fname) (y.fcomp.ckey, y.fname)
    let hash x = x.fcomp.ckey
    let equal x y = compare x y = 0
    let printer ff x = Format.fprintf ff "%s.%s" x.fcomp.cname x.fname
end

module CilFundec = struct
    type t = fundec
    let compare x y = if x == y then 0 else CilVar.compare x.svar y.svar
    let hash x = x.svar.vid
    let equal x y = compare x y = 0
    let printer ff x = Format.fprintf ff "%s" x.svar.vname
end

