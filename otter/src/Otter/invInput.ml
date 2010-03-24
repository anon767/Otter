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


(* helpers *)
let tru = Bytes.lazy_int_to_bytes 1;;
let fls = Bytes.lazy_int_to_bytes 0;;
let bytes_or b1 b2 = 
  if b1=fls then b2 else if b2=fls then b1 else
  Bytes.make_Bytes_Op (Bytes.OP_LOR, [(b1,Cil.intType);(b2,intType)]);;
let bytes_and b1 b2 = 
  if b1=tru then b2 else if b2=tru then b1 else
  Bytes.make_Bytes_Op (Bytes.OP_LAND, [(b1,Cil.intType);(b2,intType)]);;
let bytes_not b = 
  if b=tru then fls else if b=fls then tru else 
  Bytes.make_Bytes_Op (Bytes.OP_LNOT, [b,Cil.intType])

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

let constrain_task state task =
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
          let (state,bytes_addr) = 
            Builtin_function.libc___builtin_alloca_size state (lstsize*size) 
              (Bytes.bytes__symbolic (lstsize*size))
          in
			    let state, lval_block = state__varinfo_to_lval_block state formal in
			    let state = state__assign state (lval_block, size) bytes_addr in
            state
  in
  match task with
    | Condition (c) ->
        let pc = constrain_task_condition state c in
        let state = {state with path_condition=pc::state.path_condition; } in
          state
    | Creation (c) ->
        constrain_task_creation state c
    | Nothing -> state
;;

let task_rank task =
  match task with
    | Creation (c) -> 0
    | Condition (c) -> 1
    | Nothing -> 2
;;

let constrain_pptslice state (fundec:Cil.fundec) (pptslice:string) objectMap =
  let invs = getAttribute objectMap pptslice "invs" in
  let invs_list = getSequence objectMap invs in
  let task_list = 
    List.fold_left (
      fun tasks inv ->
        List.rev_append (constrain_invariant state fundec inv objectMap)  tasks
    ) [] invs_list 
  in
  let task_list = List.stable_sort (fun a b -> (task_rank a) - (task_rank b)) task_list in
    List.fold_left constrain_task state task_list
;;

(*
 * Put constraints to fundec into state
 *)
(* TODO: omit fundec, since it's already in List.hd state.callstack *)
let constrain state (fundec:Cil.fundec) objectMap =
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


(* COPIES for now *)
(* Manipulate this function to add invariants *)
let constrain_task' state task =
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
          (* Instead of allocating memory and assign to formal,
           * check if formal points to something of the right size.
           * For now, just check if formal is non-null if such task exists
           * (such task exists only if formal is non-null)
           *)
			    let size = (Cil.bitsSizeOf formal.Cil.vtype)/8 in
          let lstsize = List.fold_left max 0 values in (* preliminary *)
          let overallsize = size* lstsize in
            ignore overallsize;
            let _,formal_bytes = Eval.rval state (Lval (Var(formal),NoOffset)) in (* TODO: assert that state is unchanged? *)
              (* TODO: currently there're at least two ways of generating bytes.
               * Not Good
               *)
              Bytes.make_Bytes_Op (Bytes.OP_NE, [(formal_bytes,Cil.intType);(fls(*zero*),intType)])
  in
  match task with
    | Condition (c) ->
        constrain_task_condition state c 
    | Creation (c) ->
        constrain_task_creation state c
    | Nothing -> tru
;;

let constrain_pptslice' state (fundec:Cil.fundec) (pptslice:string) objectMap : Bytes.bytes =
  let invs = getAttribute objectMap pptslice "invs" in
  let invs_list = getSequence objectMap invs in
  let task_list = 
    List.fold_left (
      fun tasks inv ->
        List.rev_append (constrain_invariant state fundec inv objectMap)  tasks
    ) [] invs_list 
  in
  let task_list = List.stable_sort (fun a b -> (task_rank a) - (task_rank b)) task_list in
    List.fold_left (fun ct task -> let ct' = constrain_task' state task in bytes_and ct ct') tru task_list
;;

(*
 * Put constraints to fundec into state
 *)
(* TODO: omit fundec, since it's already in List.hd state.callstack *)
let constrain' state (fundec:Cil.fundec) objectMap =
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
      | None -> tru
      | Some pptTopLevel ->
          let views = getAttribute objectMap pptTopLevel "views" in
          let views_mapping = getMapping objectMap views in
            StringMap.fold (
              fun _ pptslice ct ->
                let ct' = constrain_pptslice' state fundec pptslice objectMap in
                  bytes_and ct ct'
            ) views_mapping tru
    end 
;;
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
  let ct = constrain' state fundec (!global_objectMap) in
  let pc = state.path_condition in
    begin
      Printf.printf "pc |- ct: ";
      let truth = Stp.eval pc ct in
        incr_record pc2ct truth;
        match truth with
          | Ternary.True -> Printf.printf "True\n"
          | Ternary.False -> Printf.printf "False\n"
          | _ -> Printf.printf "Unknown\n"
    end;
    begin
      Printf.printf "ct |- pc: ";
      let truth = Stp.eval [ct] (List.fold_left bytes_and tru pc) in
        incr_record ct2pc truth;
        match truth with
          | Ternary.True -> Printf.printf "True\n"
          | Ternary.False -> Printf.printf "False\n"
          | _ -> Printf.printf "Unknown\n"
    end;
    ()
;;

