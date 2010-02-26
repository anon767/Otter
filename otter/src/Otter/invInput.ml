open YamlParser
   
open Cilutility

module StringMap = Map.Make (String);;

type pptattr = string list StringMap.t;;
type pptinv = string * pptattr;;
type pptmap = pptinv list FundecMap.t;;

let (__map : pptinv list FundecMap.t ref) = ref FundecMap.empty;;

let parse yaml_str file  = 
  let rec read_string a =
    match a with
      | YamlNode.SCALAR (_,s) -> s
      | _ -> failwith "YamlParser: wrong format (read_string)"
  in 
  let retrieve_fundec fundec_str =
    let s = read_string fundec_str in 
      Function.from_name_in_file s file
  in
  let read_pptpair a b =
    (read_string a,
    match b with
      | YamlNode.SEQUENCE(_,lst) -> List.map (fun s -> read_string s) lst
      | YamlNode.SCALAR(_,s) -> [s]
      | _ -> failwith "YamlParser: wrong format (read_pptpair)"
    )
  in
  let read_pptinv doc =
    match doc with
      | YamlNode.MAPPING(_,lst)-> (
          match snd(List.nth lst 0) with YamlNode.SCALAR(_,name) -> (
          match snd(List.nth lst 1) with YamlNode.MAPPING(_,pairlst) -> (
            (name,
             List.fold_left 
               (fun map (a,b) -> 
                  let sa,sb = read_pptpair a b in
                    StringMap.add sa sb map
               )
               StringMap.empty pairlst)
          ) | _ -> failwith "YamlParser: wrong format (read_pptinv MAPPING)"
          ) | _ -> failwith "YamlParser: wrong format (read_pptinv SCALAR)"
          ) | _ -> failwith "YamlParser: wrong format (read_pptinv)"
  in
  let read_pptinvs doc =
    match doc with
      | YamlNode.SEQUENCE(_,lst) -> List.map read_pptinv lst
      | _ -> failwith "YamlParser: wrong format (read_pptinvs)"
  in
  let rec read_pptmap doc=
    match doc with
      | YamlNode.MAPPING(_,lst) -> 
          List.fold_left 
            (fun map (fundec_str,subdoc) ->
               let f = (retrieve_fundec fundec_str) in
               FundecMap.add f (read_pptinvs subdoc) map)
            FundecMap.empty lst 
      | _ -> failwith "YamlParser: wrong format (read_pptmap)"
  in
  let yaml = YamlParser.parse_string (YamlParser.make ()) yaml_str in
    __map := read_pptmap yaml;
    ()
;;


let constrain bytes varinfo fundec_opt state =
  (* TODO *)
  (match fundec_opt with
    | None -> () (* Global *)
    | Some fundec -> () (* Local (formal) *)
  );
  bytes,state
;;





