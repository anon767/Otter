open YamlParser
open Types
open Bytes
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

let getAttributes (objectMap:objectmap) (value:string) : string StringMap.t =
  let (_,typ,content) = getInfo objectMap value in
    match typ with
      | "@SCALAR" -> List.fold_left 
                        (fun map (k,v) -> StringMap.add (getYamlScalar k) (getYamlScalar v) map) 
                        StringMap.empty (getYamlMapping content)
      | _ -> failwith "Error in getAttributes"
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

(* TODO: split the file into two *)

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
 *  Structure of Daikon's VarInfo
 *
 *  VarInfo 
 *    .vardef.name: original name of the variable, or null if it's derived
 *    .derived: e.g., SizeOf. null if not derived
 *
 *)

type condition =
  | OneOfScalar of Cil.varinfo * int list
  | Negation of condition
;;
type creation =
  | TypedMalloc of Cil.varinfo * int list (* size *)
;;
type task =
  | Condition of condition
  | Creation of creation
  | Nothing
;;

let findCilFormal state (str_formal:string) : Cil.varinfo =
  try
    let fundec = List.hd state.callstack in
    let formal = List.find (fun formal -> formal.vname=str_formal) fundec.sformals in
      formal
  with Not_found ->
    failwith (Printf.sprintf "Error in findCilFormal: %s not found" str_formal)
;;

let constrain_invariant state (fundec:Cil.fundec) (inv:string) objectMap : task list =
  let (c,t,content) = getInfo objectMap inv in
    match c with
      | "daikon.inv.unary.scalar.NonZero" ->
          let ppt = getAttribute objectMap inv "ppt" in
          let var_infos = getAttribute objectMap ppt "var_infos" in
          let var_infos_list = getSequence objectMap var_infos in
          let var_info = List.hd var_infos_list in
          let vardef = getAttribute objectMap var_info "vardef" in
            if vardef <> null then
              begin (* Non-derived variable *)
                let var_name = getAttribute objectMap vardef "name" in
                let formal = findCilFormal state var_name in
                  [Condition (Negation (OneOfScalar (formal,[0])))]
              end
            else (* Derived variable *)
              []

      | "daikon.inv.unary.scalar.OneOfScalar"  ->
          let ppt = getAttribute objectMap inv "ppt" in
          let var_infos = getAttribute objectMap ppt "var_infos" in
          let var_infos_list = getSequence objectMap var_infos in
          let var_info = List.hd var_infos_list in

          let derived = getAttribute objectMap var_info "derived" in
          let num_elts = int_of_string (getAttribute objectMap inv "num_elts") in
          let elts = getAttribute objectMap inv "elts" in
          let elts_list = getSequence objectMap elts in
          let values,_ = List.fold_left 
                           (fun (lst,n) s ->
                              (if n<=0 then lst else (int_of_string s)::lst) (* TODO: overflow? *) , n-1
                           ) ([],num_elts) elts_list in

          let vardef = getAttribute objectMap var_info "vardef" in
            if vardef <> null then
              begin (* Non-derived variable *)
                let var_name = getAttribute objectMap vardef "name" in
                let formal = findCilFormal state var_name in
                  [Condition (OneOfScalar (formal,values))]
              end
            else (* Derived variable *)
              begin
                let (c,_,_) = getInfo objectMap derived in
                match c with
                  | "daikon.derive.unary.SequenceLength" -> 
                      let base = getAttribute objectMap derived "base" in
                      let vardef = getAttribute objectMap base "vardef" in
                        if vardef <> null then 
                          begin
                            let shift = int_of_string (getAttribute objectMap derived "shift") in
                            let enclosing_var = getAttribute objectMap vardef "enclosing_var" in
                            let formal = findCilFormal state enclosing_var in 
                              if shift = 0 then (
                                [Creation ( TypedMalloc (formal,values) )]
                              )
                              else []
                          end
                        else
                          failwith "Error in constrain_invariant: derived of derived variable?"
                  | _ -> []
              end

      | _ -> []
;;

(* Transform a task into
 * 1. a constraint
 * 2. an updated state (excluding the constraint above)
 *)
let constrain_task state task : bytes (* constraints *) * state=
  let rec constrain_task_condition state c =
    match c with
      | OneOfScalar (formal,values) ->
          let _,formal_bytes = Eval.rval state (Lval (Var(formal),NoOffset)) in (* TODO: assert that state is unchanged? *)
          let typ = formal.vtype in
          let eqExps = List.map (fun v -> (Operation.eq [(formal_bytes,typ);(Bytes.lazy_int_to_bytes v,typ)],typ) ) values in
          let pc = List.fold_left 
                       (fun pc (exp,_) -> 
                          let bs = bytes_or pc exp in bs
                       ) fls eqExps in
            pc
      | Negation (c) -> let pc = constrain_task_condition state c in
          bytes_not pc

  in
  let constrain_task_creation state c =
    match c with
      | TypedMalloc (formal,values) ->
			    let size = (Cil.bitsSizeOf formal.Cil.vtype)/8 in
          let lstsize = List.fold_left max 0 values in (* preliminary *)
          (* Two parts: *)
          (* the constraint *) 
          let _,formal_bytes = Eval.rval state (Lval (Var(formal),NoOffset)) in (* TODO: assert that state is unchanged? *)
            (* check for non-null. TODO: check for length *)
          let ct = Bytes.make_Bytes_Op (Bytes.OP_NE, [(formal_bytes,Cil.intType);(fls(*zero*),Cil.intType)]) in

          (* the updated state *)
            (* TODO: eventually, we don't want assignment. We want constraint
             * instead 
             *)
          let (state,bytes_addr) = 
            Builtin_function.libc___builtin_alloca_size state (lstsize*size) 
              (Bytes.bytes__symbolic (lstsize*size))
          in
			    let state, lval_block = MemOp.state__varinfo_to_lval_block state formal in
			    let state = MemOp.state__assign state (lval_block, size) bytes_addr in
            (ct, state)
  in
  match task with
    | Condition (c) ->
        let pc = constrain_task_condition state c in
          (pc,state)
    | Creation (c) ->
        constrain_task_creation state c
    | Nothing -> (tru,state)
;;

let task_rank task =
  match task with
    | Creation (c) -> 0
    | Condition (c) -> 1
    | Nothing -> 2
;;

let constrain_pptslice state (fundec:Cil.fundec) (pptslice:string) objectMap : bytes list (* list of constraints *) * state =
  let invs = getAttribute objectMap pptslice "invs" in
  let invs_list = getSequence objectMap invs in
  let task_list = 
    List.fold_left (
      fun tasks inv ->
        List.rev_append (constrain_invariant state fundec inv objectMap)  tasks
    ) [] invs_list 
  in
  let task_list = List.stable_sort (fun a b -> (task_rank a) - (task_rank b)) task_list in
    List.fold_left (fun (lst,s) t -> let (pc,s') = constrain_task s t in (pc::lst,s')) ([],state) task_list
;;

let constrain state (fundec:Cil.fundec) objectMap : bytes * state =
  (* TODO: omit fundec, since it's already in List.hd state.callstack *)
  let constrain_helper state (fundec:Cil.fundec) objectMap : bytes list * state =
    let isSubstring a b (* a is substring of b *) =
      try ignore (Str.search_forward (Str.regexp_string a) b 0); true with Not_found -> false 
    in
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
        | None -> ([],state)
        | Some pptTopLevel ->
            let views = getAttribute objectMap pptTopLevel "views" in
            let views_mapping = getMapping objectMap views in
              StringMap.fold (
                fun _ pptslice (lst,state) ->
                  let (lst',state') = constrain_pptslice state fundec pptslice objectMap
                  in (List.rev_append lst lst'),state'
              ) views_mapping ([],state)
      end 
  in

  let (lst,state') = constrain_helper state fundec objectMap in
  let pc = List.fold_left bytes_and tru lst in
  let state'' = MemOp.state__add_path_condition state' pc false in
    (pc,state'')
;;


(* ******************* *)


let global_objectMap = ref ObjectMap.empty;;

type record = { numTrue:int; numFalse:int; numUnknown:int };;
let incr_record r t = match t with
  | Ternary.True -> r:={!r with numTrue=(!r).numTrue+1}
  | Ternary.False -> r:={!r with numFalse=(!r).numFalse+1}
  | Ternary.Unknown -> r:={!r with numUnknown=(!r).numUnknown+1}
;;

let pc2ct = ref {numTrue=0;numFalse=0;numUnknown=0};;
let ct2pc = ref {numTrue=0;numFalse=0;numUnknown=0};;

let examine state fundec = 
  (
  if global_objectMap = ref ObjectMap.empty then
    global_objectMap := parse Executeargs.run_args.Executeargs.arg_yaml
  else
    ()
  );
  let ct,_ = constrain state fundec (!global_objectMap) in
  let pc = state.path_condition in
    begin
      Printf.printf "state |- pc -> ct: ";
      let _,truth = MemOp.state__eval state pc ct in
        incr_record pc2ct truth;
        match truth with
          | Ternary.True -> Printf.printf "True\n"
          | Ternary.False -> Printf.printf "False\n"
          | _ -> Printf.printf "Unknown\n"
    end;
    begin
      Printf.printf "state |- ct -> pc: ";
      let _,truth = MemOp.state__eval state [ct] (List.fold_left bytes_and tru pc) in
        incr_record ct2pc truth;
        match truth with
          | Ternary.True -> Printf.printf "True\n"
          | Ternary.False -> Printf.printf "False\n"
          | _ -> Printf.printf "Unknown\n"
    end;
    ()
;;

