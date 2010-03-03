open YamlParser
open Types
open MemOp
open Cilutility

module ObjectMap = Map.Make (Int64);;
module StringMap = Map.Make (String);;

let null = "0";;

(* Globals *)
let (objectMap : YamlNode.t ObjectMap.t ref) = ref ObjectMap.empty;;
let file = ref Cil.dummyFile;;


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
 * value is the untyped (either primitive or pointer)
 * typ is {@SCALAR,@SEQUENCE,@MAPPING}
 *)
let get (value:string) : (string*YamlNode.t) =
  let getObject (hashcode:int64) : YamlNode.t =
    ObjectMap.find hashcode (!objectMap) 
  in
  let getContent (obj:YamlNode.t) : (string*YamlNode.t) =
    match obj with
      | YamlNode.MAPPING ("daikon.JavaObject",lst) ->
          let (k,v) = List.find 
                        (fun (tk,v) -> 
                           let k=getYamlScalar tk in 
                             k="@SCALAR"||k="@MAPPING"||k="@SEQUENCE") lst 
          in (getYamlScalar k,v)
      | _ -> failwith "Error in getContent"
  in
  let hashcode = Int64.of_string value in
  let obj = getObject hashcode in
  let (typ,content) = getContent obj in
    (typ,content)
;;

let getType (value:string) : string =
  let (typ,content) = get value in
    typ
;;

(*
 *  Functions that traverse the java data structure
 *  the string type acts as untyped
 *)
let getAttribute (value:string) (attrName:string) : string =
  let (typ,content) = get value in
    match typ with
      | "@SCALAR" -> 
          let (k,v) = List.find 
                        (fun (k,v) -> (getYamlScalar k)=attrName) 
                        (getYamlMapping content)
          in (getYamlScalar v) 
      | _ -> failwith "Error: non-SCALAR has no attributes"
;;

let getSequence (value:string) : string list =
  let (typ,content) = get value in
    match typ with
      | "@SEQUENCE" -> List.map getYamlScalar (getYamlSequence content)
      | _ -> failwith "Error: getSequence invoked with non-SEQUENCE"
;;

let getMapping (value:string) : string StringMap.t =
  let (typ,content) = get value in
    match typ with
      | "@MAPPING" -> List.fold_left 
                        (fun map (k,v) -> StringMap.add (getYamlScalar k) (getYamlScalar v) map) 
                        StringMap.empty (getYamlMapping content)
      | _ -> failwith "Error: getMapping invoked with non-MAPPING"
;;

let findInMapping (key:string) (value:string) : string =
  let map = getMapping value in
  try StringMap.find key map
  with Not_found -> "0"
;;

(* 
 * Parse *.yml files
 *)
let parse yaml_str (file':Cil.file) : unit =
  let mapHashcodesToObjects yamlnode =
    match yamlnode with
      | YamlNode.MAPPING(_,hashCode2ObjectList) ->
          List.fold_left 
            begin fun map (h,o) -> ObjectMap.add (Int64.of_string (getYamlScalar h)) o map end
            ObjectMap.empty hashCode2ObjectList
      | _ -> failwith "Error in mapping hashcodes to objects"
  in
  file := file';
  if yaml_str = "" then () else
    let yamlnode = YamlParser.parse_string (YamlParser.make ()) yaml_str in
      objectMap := mapHashcodesToObjects yamlnode
;;

(* 
 * Test function
 *)
let test_function () =
  let pptmap = "14454885" (* Think of this as a pointer *) in
  let serialVersionUID = getAttribute pptmap "serialVersionUID" in
  let nameToPpt = getAttribute pptmap "nameToPpt" in
  let pptTopLevel = findInMapping "..addstr():::ENTER" nameToPpt in
    Printf.printf "serialVersionUID=%s\n" serialVersionUID;
    Printf.printf "pptTopLevel=%s\n" pptTopLevel;
    ()
;;

(*
 * Put constraints to fundec into state
 *)
let constrain state fundec =
  test_function () ;
  (* "daikon.inv.unary.scalar.OneOfScalar" *)
  state
;;


