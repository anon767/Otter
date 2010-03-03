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
    ObjectMap.find hashcode map 
  in
  let hashcode = Int64.of_string value in
  let obj = getObject hashcode objectMap in
    match obj with
      | YamlNode.MAPPING ("daikon.JavaObject",lst) ->
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
          let (k,v) = List.find 
                        (fun (k,v) -> (getYamlScalar k)=attrName) 
                        (getYamlMapping content)
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

let constrain_invariant state (fundec:Cil.fundec) (inv:string) objectMap =
  (* TODO *)
  (* "daikon.inv.unary.scalar.OneOfScalar" *)
  let (c,t,content) = getInfo objectMap inv in
    Printf.printf "%s\n" c;
  state
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


