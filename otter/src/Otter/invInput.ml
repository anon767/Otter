open YamlParser
open Types
open MemOp
open Cilutility
open Cil

module ObjectMap = Map.Make (Int64);;
module StringMap = Map.Make (String);;

type objectmap = YamlNode.t ObjectMap.t;;

let null = "0";;

(*
 * Extract {SCALAR,SEQUENCE,MAPPING} from the YamlNode constructors
 *)
let getYamlScalar (node:YamlNode.t) : string =
  match node with
    | YamlNode.SCALAR (_,s) -> s
    | _ -> failwith "Error: input node is not SCALAR"
;;
let getYamlSequence (node:YamlNode.t) : YamlNode.t list =
  match node with
    | YamlNode.SEQUENCE (_,t) -> t
    | _ -> failwith "Error: input node is not SEQUENCE"
;;
let getYamlMapping (node:YamlNode.t) : (YamlNode.t*YamlNode.t) list =
  match node with
    | YamlNode.MAPPING (_,t) -> t
    | _ -> failwith "Error: input node is not MAPPING"
;;

(* 
 * (@CLASS,@TYPE,@CONTENT)
 * @CLASS is java class name
 * @TYPE is {@SCALAR,@SEQUENCE,@MAPPING}
 *)
let getInfo (objectMap:objectmap) (value:string) : (string*string*YamlNode.t) =
  let getObject (hashcode:int64) (map:objectmap) : YamlNode.t =
    try
    ObjectMap.find hashcode map 
    with Not_found -> 
      failwith (Printf.sprintf "Error in getObject: %s not found" (Int64.to_string(hashcode)))
  in
  let hashcode = Int64.of_string value in
  let obj = getObject hashcode objectMap in
    match obj with
      | YamlNode.MAPPING (_,lst) ->
          let lookup key =
            let predicate key (yk,v) =
              let k = getYamlScalar yk in k = key
            in
            let (_,v) = List.find (predicate key) lst in v
          in
            ( 
              (getYamlScalar (lookup "@CLASS")),
              (getYamlScalar (lookup "@TYPE")),
              (lookup "@CONTENT")
            )
      | _ -> failwith "Error in getInfo"
;;

(*
 *  Functions that traverse the java data structure
 *  the string type acts as untyped
 *)
let getAttribute (objectMap:objectmap) (value:string) (attrName:string) : string =
  let (_,typ,content) = getInfo objectMap value in
    match typ with
      | "@SCALAR" -> 
          let (k,v) = 
            try
              List.find (fun (k,v) -> (getYamlScalar k)=attrName) (getYamlMapping content)
            with Not_found -> failwith (Printf.sprintf "Error in getAttribute: %s not found" attrName)
          in (getYamlScalar v) 
      | _ -> failwith "Error: non-SCALAR has no attributes"
;;

let getSequence (objectMap:objectmap) (value:string) : string list =
  let (_,typ,content) = getInfo objectMap value in
    match typ with
      | "@SEQUENCE" -> List.map getYamlScalar (getYamlSequence content)
      | _ -> failwith "Error: getSequence invoked with non-SEQUENCE"
;;

let getMapping (objectMap:objectmap) (value:string) : string StringMap.t =
  let (_,typ,content) = getInfo objectMap value in
    match typ with
      | "@MAPPING" -> List.fold_left 
                        (fun map (k,v) -> StringMap.add (getYamlScalar k) (getYamlScalar v) map) 
                        StringMap.empty (getYamlMapping content)
      | _ -> failwith "Error: getMapping invoked with non-MAPPING"
;;

let findInMapping (objectMap:objectmap) (key:string) (value:string) : string =
  let map = getMapping objectMap value in
    try StringMap.find key map with Not_found -> null
;;

(* 
 * Parse *.yml files into an objectmap
 *)
let parse yaml_str : objectmap =
  let mapHashcodesToObjects yamlnode =
    match yamlnode with
      | YamlNode.MAPPING(_,hashCode2ObjectList) ->
          List.fold_left 
            begin fun map (h,o) -> ObjectMap.add (Int64.of_string (getYamlScalar h)) o map end
            ObjectMap.empty hashCode2ObjectList
      | _ -> failwith "Error in mapping hashcodes to objects"
  in
    if yaml_str = "" then ObjectMap.empty
    else
      let yamlnode = YamlParser.parse_string (YamlParser.make ()) yaml_str in
        mapHashcodesToObjects yamlnode
;;

(* 
 * Locate the only PptMap instance and return its address (as untyped) 
 *)
let findPptMap objectMap =
  let pptmaps = ObjectMap.fold 
    begin
      fun k v acc ->
        let k_str = Int64.to_string k in
        let (c,t,content) = getInfo objectMap k_str in
          if c="daikon.PptMap" then k_str::acc else acc
    end
    objectMap []
  in
    assert (List.length pptmaps = 1);
    List.hd pptmaps
;;

(* 
 * Test function
 *)
let test_function objectMap =
  let pptmap = findPptMap objectMap in
  let serialVersionUID = getAttribute objectMap pptmap "serialVersionUID" in
  let nameToPpt = getAttribute objectMap pptmap "nameToPpt" in
  let pptTopLevel = findInMapping objectMap "..addstr():::ENTER" nameToPpt in
    Printf.printf "serialVersionUID=%s\n" serialVersionUID;
    Printf.printf "pptTopLevel=%s\n" pptTopLevel;
    ()
;;

let isSubstring a b (* a is substring of b *) =
  try ignore (Str.search_forward (Str.regexp_string a) b 0); true with Not_found -> false 
;;


(*
 *  Structure of Daikon's VarInfo
 *
 *  VarInfo 
 *    .vardef.name: original name of the variable, or null if it's derived
 *    .derived: e.g., SizeOf. null if not derived
 *
 *)

let findCilFormal state (str_formal:string) : Cil.varinfo =
  try
    let fundec = List.hd state.callstack in
    let formal = List.find (fun formal -> formal.vname=str_formal) fundec.sformals in
      formal
  with Not_found ->
    failwith (Printf.sprintf "Error in findCilFormal: %s not found" str_formal)
;;

let constrain_invariant state (fundec:Cil.fundec) (inv:string) objectMap =
  let (c,t,content) = getInfo objectMap inv in
    match c with
      | "daikon.inv.unary.scalar.OneOfScalar" ->
          let ppt = getAttribute objectMap inv "ppt" in
          let var_infos = getAttribute objectMap ppt "var_infos" in
          let var_infos_list = getSequence objectMap var_infos in
          let var_info = List.hd var_infos_list in

          let derived = getAttribute objectMap var_info "derived" in
          let num_elts = int_of_string (getAttribute objectMap inv "num_elts") in
          let elts = getAttribute objectMap inv "elts" in
          let elts_list = getSequence objectMap elts in

          let vardef = getAttribute objectMap var_info "vardef" in
            if vardef <> null then
              begin (* Non-derived variable *)
                let var_name = getAttribute objectMap vardef "name" in
                let formal = findCilFormal state var_name in
                let values,_ = List.fold_left 
                                 (fun (lst,n) s ->
                                    (if n<=0 then lst else (int_of_string s)::lst) (* TODO: overflow? *) , n-1
                                 ) ([],num_elts) elts_list in
                let state,formal_bytes = Eval.rval state (Lval (Var(formal),NoOffset)) in
                let typ = formal.vtype in
                let eqExps = List.map (fun v -> (Operation.eq [(formal_bytes,typ);(Bytes.lazy_int_to_bytes v,typ)],typ) ) values in
                let pc,_ = List.fold_left 
                           (fun pc (exp,typ) -> 
                              let bs = (Bytes.make_Bytes_Op (Bytes.OP_LOR, [pc;(exp,typ)])) in
                                (bs,typ)
                           ) (Bytes.bytes__zero,Cil.intType) eqExps in
                let state = {state with path_condition=pc::state.path_condition; } in
                  state
              end
            else (* Derived variable *)
              begin
                Printf.printf "derived=%s\n" derived; 
                state
              end

      | _ -> state
;;

let constrain_pptslice state (fundec:Cil.fundec) (pptslice:string) objectMap =
  let invs = getAttribute objectMap pptslice "invs" in
  let invs_list = getSequence objectMap invs in
    List.fold_left (
      fun state inv ->
        constrain_invariant state fundec inv objectMap
    ) state invs_list
;;

(*
 * Put constraints to fundec into state
 *)
(* TODO: omit fundec, since it's already in List.hd state.callstack *)
let constrain state (fundec:Cil.fundec) objectMap =
  let pptmap = findPptMap objectMap in
  let nameToPpt = getAttribute objectMap pptmap "nameToPpt" in
  let nameToPpt_mapping = getMapping objectMap nameToPpt in 
  let pptTopLevel_opt = 
    StringMap.fold (
      fun k v target ->
        match target with
          | Some _ -> target
          | None -> if isSubstring fundec.svar.vname k && isSubstring ":::ENTER" k 
            then Some v else None
    ) nameToPpt_mapping None
  in
    begin match pptTopLevel_opt with
      | None -> state
      | Some pptTopLevel ->
          let views = getAttribute objectMap pptTopLevel "views" in
          let views_mapping = getMapping objectMap views in
            StringMap.fold (
              fun _ pptslice state ->
                constrain_pptslice state fundec pptslice objectMap
            ) views_mapping state
    end 
;;


