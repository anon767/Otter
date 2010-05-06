open Types
open Cil
open Hashtbl

(** 
  * Simple prioritizer
  *)
let time = ref 0.0;;
let timer () =
    (time:=(!time+.1.0);!time)
;;

let dfs job = timer () ;;
let bfs job = -. (timer ()) ;;



(**
  * Prioritization using CFG
  *)

type instr_stmt =
  | Instr of Cil.instr list * Cil.stmt (* stmt: stmt that contains instr *)
  | Stmt of Cil.stmt
  | Target of int
;;

let instr_stmt_length instr_stmt =
  match instr_stmt with
    | Stmt (stmt) -> List.length stmt.succs
    | _ -> 1
;;

let get_instr_stmt job =
  match job.instrList with
    | instr::instrs -> 
        (* TODO: check if instr corresponds to a target *)
        Instr(job.instrList,List.hd job.stmt.preds)
    | [] -> Stmt(job.stmt)
;;

type graph_node = 
    { 
        mutable obj: instr_stmt;
        mutable prevs: graph_node list;
        mutable nexts: graph_node list;
        mutable color: int;
    }
;;

type graph = (instr_stmt,graph_node) Hashtbl.t ;;

let print_instr_stmt instr_stmt =
  match instr_stmt with
    | Instr (instr::_,_) -> To_string.instr instr
    | Instr ([],stmt) -> "(End of instrs)"
    | Stmt (stmt) -> To_string.stmt stmt
    | Target (n) -> Printf.sprintf "(@Target%d)" n
;;

let print_node node =
  let s = Printf.sprintf "Node: %s\n" (print_instr_stmt node.obj) in
    List.fold_left 
      begin
        fun str next -> 
          let s = Printf.sprintf "\tNext: %s\n" (print_instr_stmt next.obj) in
            str^s
      end
      s node.nexts
;;

let print_graph graph =
  To_string.force_print := true;
  Hashtbl.iter (fun k v -> Printf.printf "%s" (print_node v)) graph;
  To_string.force_print := true;
  ()
;;


let make_graph_node graph instr_stmt =
  (* memoization *)
  if Hashtbl.mem graph instr_stmt then
    Hashtbl.find graph instr_stmt
  else
    let node =
      {
        obj = instr_stmt;
        prevs = [];
        nexts = [];
        color = 0;
      }
    in
      Hashtbl.replace graph instr_stmt node;
      node
;;

let make_graph fundec =
  let rec make_graph_forward graph node =
    (* check if node has been processed *)
    if node.color = 1 then () else
      begin
        node.color <- 1;
        match node.obj with
          | Stmt(stmt) -> 
              begin match stmt.skind with
                | Cil.Instr (instrs) -> 
                    let instr_stmt = Instr(instrs,stmt) in
                      node.obj <- instr_stmt;
                      node.color <- 0;
                      make_graph_forward graph node
                | _ -> explore_succs graph node stmt.succs
              end
          | Instr(instr::instrs,stmt) ->
              let node' = make_graph_node graph (Instr(instrs,stmt)) in
                node.nexts <- node'::node.nexts;
                make_graph_forward graph node'
          | Instr([],stmt) ->
              explore_succs graph node stmt.succs
          | Target(n) -> ()
      end
  and explore_succs graph node succs =
    List.iter 
      begin
        fun stmt' ->
          let node' = make_graph_node graph (Stmt stmt') in
            node.nexts <- node'::node.nexts;
            make_graph_forward graph node'
      end
      succs
  in
  let make_graph_backward graph =
    Hashtbl.iter
      begin
        fun _ node ->
          List.iter 
            begin
              fun node' ->
                node'.prevs <- node::node'.prevs
            end node.nexts
      end graph
  in
  let graph = Hashtbl.create 100 in
  let stmt = List.hd fundec.sallstmts in
  let root = make_graph_node graph (Stmt stmt) in
    make_graph_forward graph root;
    make_graph_backward graph ;
    graph,root
;;


let graph__get_node graph instr_stmt =
  Hashtbl.find graph instr_stmt 
;;

let reachable graph src tar =
  true
;;

let prioritize job = 
  try
    let _ = if (Executeargs.run_args.Executeargs.arg_cfg_pruning) then
      begin
        let graph,root = make_graph (List.hd job.state.callstack) in
          ignore root;
          print_graph graph;
          ()
      end
    else ()
    in

      dfs job

(*
    let target = graph__get_node graph (Target 0) in
    let source = graph__get_node graph (get_instr_stmt job) in
      if not (reachable graph source target) then
        neg_infinity
      else 
        dfs job
 *)
  with _ -> dfs job
;;

