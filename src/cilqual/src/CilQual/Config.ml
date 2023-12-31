open Control.Data
open Control.Monad
open TypeQual.QualType


(* Cil setup *)
let annot_attribute_string = "cilqual"
let cil_inserted_attribute_string = "cilqual_cil_inserted"

let init_cil () =
    (* don't annotate inserted variables *)
    let prev_typeForInsertedVar = !Cabs2cil.typeForInsertedVar in
    Cabs2cil.typeForInsertedVar := begin fun t -> Cil.visitCilType begin object
        inherit Cil.nopCilVisitor
        method vattr attr = Cil.ChangeTo (Cil.dropAttribute annot_attribute_string [ attr ])
    end end (prev_typeForInsertedVar t) end;
    (* mark inserted casts *)
    let prev_typeForInsertedCast = !Cabs2cil.typeForInsertedCast in
    Cabs2cil.typeForInsertedCast := begin fun t ->
        Cil.typeAddAttributes [ Cil.Attr (cil_inserted_attribute_string, []) ] (prev_typeForInsertedCast t)
    end;
    let prev_constFoldCastFilter = !Cil.constFoldCastFilter in
    Cil.constFoldCastFilter := begin fun t ->
        Cil.typeRemoveAttributes [ cil_inserted_attribute_string ] (prev_constFoldCastFilter t)
    end


(* parse type qualifiers annotations from attributes *)
let parse_annot attrlist =
    let rec parse_annot attrlist quallist = match attrlist with
        (* attribute in the form __attribute__((cilqual(qual))) *)
        | Cil.Attr (s, [ Cil.ACons (qual, _) ])::tail when s = annot_attribute_string ->
            parse_annot tail (qual::quallist)
        | Cil.Attr (s, _)::_ when s = annot_attribute_string ->
            failwith "TODO: report invalid qualifier"
        | _::tail ->
            parse_annot tail quallist
        | [] ->
            quallist
    in
    parse_annot attrlist []

