(** Module wrappers and helper functions for various CIL types; useful for creating Set/Map *)

open Cil

module CilLocation = struct
    type t = location
    let compare = Cil.compareLoc
    let hash = Hashtbl.hash
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

module CilType = struct
    type t = typ
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal x y = compare x y = 0
end

module CilCanonicalType = struct
    type t = typ
    let canonicalize t = typeSigWithAttrs (fun _ -> []) t
    let compare x y = if x == y then 0 else Pervasives.compare (canonicalize x) (canonicalize y)
    let hash x = Hashtbl.hash (canonicalize x)
    let equal x y = compare x y = 0
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
    let is_const varinfo =
        if Cil.hasAttribute "const" varinfo.Cil.vattr || Cil.hasAttribute "const" (Cil.typeAttrs varinfo.Cil.vtype) then
            true
        else match Cil.unrollType varinfo.Cil.vtype with
            | Cil.TArray (typ, _, _) -> Cil.hasAttribute "const" (Cil.typeAttrs typ)
            | _ -> false
end

module CilField = struct
    type t = fieldinfo
    let compare x y = if x == y then 0 else Pervasives.compare (x.fcomp.ckey, x.fname) (y.fcomp.ckey, y.fname)
    let hash x = x.fcomp.ckey
    let equal x y = compare x y = 0
    let printer ff x =
        if x.floc == CilLocation.default then
            Format.fprintf ff "%s.%s" x.fcomp.cname x.fname
        else
            Format.fprintf ff "%s.%s:%a" x.fcomp.cname x.fname CilLocation.printer x.floc
end

module CilOffset = struct
    type t = Cil.offset
    let compare = Pervasives.compare
    let equal x y = compare x y = 0
end

module CilFundec = struct
    type t = fundec
    let compare x y = if x == y then 0 else compare x.svar.vid y.svar.vid
    let hash x = x.svar.vid
    let equal x y = compare x y = 0
    let printer ff x = CilVar.printer ff x.svar
end

module CilLhost = struct
    type t = Cil.lhost
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal x y = compare x y = 0
end

module CilExp = struct
    type t = Cil.exp
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal x y = compare x y = 0
    let printer = Printcil.exp
end

module CilExpLoc = struct
    type t = CilExp.t * CilLocation.t
    let compare (xexp, xloc as x) (yexp, yloc as y) = if x == y then 0 else
        match CilLocation.compare xloc yloc with
            | 0 -> CilExp.compare xexp yexp
            | i -> i
    let hash (exp, loc) = CilExp.hash exp lxor CilLocation.hash loc
    let equal x y = compare x y = 0
    let printer ff (exp, loc) =
        Format.fprintf ff "%a:%a" CilExp.printer exp CilLocation.printer loc
end

module Malloc = struct
    type t = CilVar.t * string * CilCanonicalType.t
    let compare (xv, xs, xt as x) (yv, ys, yt as y) = if x == y then 0 else
        match CilVar.compare xv yv with
            | 0 ->
                begin match String.compare xs ys with
                    | 0 -> CilCanonicalType.compare xt yt
                    | i -> i
                end
            | i ->
                i
end
