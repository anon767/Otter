

let annot_attribute_string = "mix"
let model_attribute_param = "model"
let typed_attribute_param = "typed"
let symbolic_attribute_param = "symbolic"

module type BlockConfig = sig
    val should_enter_block : Cil.attributes -> bool
    val is_model_block : Cil.attributes -> bool
end

module BlockConfig (C : sig val attribute_param : string end) = struct
    let parse_annot attrlist =
        let rec parse_annot attrlist analysis = match attrlist with
            (* attribute in the form __attribute__((mix(analysis))) *)
            | Cil.Attr (s, [ Cil.ACons (a, _) ])::tail when s = annot_attribute_string ->
                begin match analysis with
                    | None -> parse_annot tail (Some a)
                    | Some _ -> failwith "TODO: report more than one mix attribute specified"
                end
            | Cil.Attr (s, _)::_ when s = annot_attribute_string ->
                failwith "TODO: report invalid mix attribute"
            | _::tail ->
                parse_annot tail analysis
            | [] ->
                analysis
        in
        parse_annot attrlist None

    let should_enter_block attr = match parse_annot attr with
        (* annotated with the given param *)
        | Some a when a = C.attribute_param -> true
        (* annotated as model *)
        | Some a when a = model_attribute_param -> true
        (* unannotated *)
        | None -> true
        (* annotated, but of a different param *)
        | _ -> false

    let is_model_block attr = match parse_annot attr with
        | Some a when a = model_attribute_param -> true
        | _ -> false
end

module TypedBlockConfig = BlockConfig (struct let attribute_param = typed_attribute_param end)
module SymbolicBlockConfig = BlockConfig (struct let attribute_param = symbolic_attribute_param end)

