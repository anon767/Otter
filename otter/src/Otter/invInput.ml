open YamlParser
open Types
open MemOp
open Cilutility

module ObjectMap = Map.Make (Int64);;
module StringMap = Map.Make (String);;

type objectmap = YamlNode.t ObjectMap.t;;
type untyped   = string*objectmap;;

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

(* Functions that extract values from untyped *)
let getString (value:untyped) : string =
  fst value
;;
let getObjectMap (value:untyped) : objectmap =
  snd value
;;
(* getInteger, getBoolean, ... *)

(* 
 * (@CLASS,@TYPE,@CONTENT)
 * @CLASS is java class name
 * @TYPE is {@SCALAR,@SEQUENCE,@MAPPING}
 *)
let getInfo (value:untyped) : (string*string*YamlNode.t) =
  let getObject (hashcode:int64) (map:objectmap) : YamlNode.t =
    ObjectMap.find hashcode map 
  in
  let hashcode = Int64.of_string (getString value) in
  let obj = getObject hashcode (getObjectMap value) in
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
let getAttribute (value:untyped) (attrName:string) : untyped =
  let (_,typ,content) = getInfo value in
    match typ with
      | "@SCALAR" -> 
          let (k,v) = List.find 
                        (fun (k,v) -> (getYamlScalar k)=attrName) 
                        (getYamlMapping content)
          in (getYamlScalar v) , (getObjectMap value)
      | _ -> failwith "Error: non-SCALAR has no attributes"
;;

let getSequence (value:untyped) : string list =
  let (_,typ,content) = getInfo value in
    match typ with
      | "@SEQUENCE" -> List.map getYamlScalar (getYamlSequence content)
      | _ -> failwith "Error: getSequence invoked with non-SEQUENCE"
;;

let getMapping (value:untyped) : string StringMap.t =
  let (_,typ,content) = getInfo value in
    match typ with
      | "@MAPPING" -> List.fold_left 
                        (fun map (k,v) -> StringMap.add (getYamlScalar k) (getYamlScalar v) map) 
                        StringMap.empty (getYamlMapping content)
      | _ -> failwith "Error: getMapping invoked with non-MAPPING"
;;

let findInMapping (key:string) (value:untyped) : untyped =
  let map = getMapping value in
    ( (try StringMap.find key map with Not_found -> null) , getObjectMap value)
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
 * Test function
 *)
let findPptMap objectMap =
  "446196",objectMap (* Think of this as a pointer *) 
;;

let test_function objectMap =
  let pptmap = findPptMap objectMap in
  let serialVersionUID = getAttribute pptmap "serialVersionUID" in
  let nameToPpt = getAttribute pptmap "nameToPpt" in
  let pptTopLevel = findInMapping "..addstr():::ENTER" nameToPpt in
    Printf.printf "serialVersionUID=%s\n" (getString serialVersionUID);
    Printf.printf "pptTopLevel=%s\n" (getString pptTopLevel);
    ()
;;

(*
 * Put constraints to fundec into state
 *)
let constrain state fundec objectMap =
  test_function objectMap;
  (* "daikon.inv.unary.scalar.OneOfScalar" *)
  state
;;


