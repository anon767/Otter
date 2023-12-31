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

module CilCanonicalType = struct
    type t = typ
    let compare x y =
        let canonicalize t = typeSigWithAttrs (fun _ -> []) t in
        Pervasives.compare (canonicalize x) (canonicalize y)
    let equal x y = compare x y = 0
end

module CilLocation = struct
    type t = location
    let compare = Cil.compareLoc
    let default = Cil.locUnknown
    let file f = { Cil.file = f.Cil.fileName; Cil.line = -1; Cil.byte = -1 }
    let printer ff loc =
        if loc == default then
            Format.fprintf ff ""
        else if loc.Cil.line <= 0 then
            Format.fprintf ff "%s" loc.Cil.file
        else
            Format.fprintf ff "%s:%d" loc.Cil.file loc.Cil.line
end

module CilVar = struct
    type t = varinfo
    let compare x y = if x == y then 0 else compare x.vid y.vid
    let hash x = x.vid
    let equal x y = compare x y = 0
    let printer ff x =
        if x.vdecl == CilLocation.default then
            Format.fprintf ff "%s" x.vname
        else
            Format.fprintf ff "%s:%a" x.vname CilLocation.printer x.vdecl
end

module CilField = struct
    type t = fieldinfo
    let compare x y = if x == y then 0 else compare (x.fcomp.ckey, x.fname) (y.fcomp.ckey, y.fname)
    let hash x = x.fcomp.ckey
    let equal x y = compare x y = 0
    let printer ff x =
        if x.floc == CilLocation.default then
            Format.fprintf ff "%s.%s" x.fcomp.cname x.fname
        else
            Format.fprintf ff "%s.%s:%a" x.fcomp.cname x.fname CilLocation.printer x.floc
end

module CilFundec = struct
    type t = fundec
    let compare x y = if x == y then 0 else compare x.svar.vid y.svar.vid
    let hash x = x.svar.vid
    let equal x y = compare x y = 0
    let printer ff x = CilVar.printer ff x.svar
end

module CilExp = struct
    type t = exp * CilLocation.t
    let compare (xexp, xloc as x) (yexp, yloc as y) = if x == y then 0 else
        match CilLocation.compare xloc yloc with
            | 0 -> Pervasives.compare xexp yexp
            | i -> i
    let hash (_, loc) = Hashtbl.hash loc
    let equal x y = compare x y = 0
    let printer ff (exp, loc) =
        Format.fprintf ff "%s:%a" (Pretty.sprint 0 (d_exp () exp)) CilLocation.printer loc
end
