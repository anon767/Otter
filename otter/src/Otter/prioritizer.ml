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

let lifo job = timer () ;;
let fifo job = -. (timer ()) ;;



(**
  * Prioritization using CFG
  *)

type instr_stmt =
  | Instr of Cil.instr list * Cil.stmt (* stmt: stmt that contains instr *)
  | Stmt of Cil.stmt
;;

let instr_stmt_length instr_stmt =
  match instr_stmt with
    | Stmt (stmt) -> List.length stmt.succs
    | _ -> 1
;;

let hash_consing_instr_stmt_tbl : (instr_stmt,instr_stmt) Hashtbl.t = Hashtbl.create 10;;
let hash_consing_instr_stmt_create bs = 
  try Hashtbl.find hash_consing_instr_stmt_tbl bs
  with Not_found -> Hashtbl.add hash_consing_instr_stmt_tbl bs bs; bs;;

let make_instr_stmt_Stmt stmt =
  hash_consing_instr_stmt_create (Stmt (stmt))
;;

let make_instr_stmt_Instr (instrs,stmt) =
  hash_consing_instr_stmt_create (Instr (instrs,stmt))
;;

let get_instr_stmt job =
  match job.instrList with
    | instr::instrs -> 
        make_instr_stmt_Instr (job.instrList,List.hd job.stmt.preds)
    | [] -> make_instr_stmt_Stmt (job.stmt)
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
    | Instr (instrs,stmt) -> "(Instr:"^(List.fold_right (fun instr str -> (To_string.instr instr)^"::"^str) instrs "[]" )^","^(To_string.stmt stmt)^")"
    | Stmt (stmt) -> "(Stmt:"^(To_string.stmt stmt)^")"
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

let graph__set_color graph color =
  Hashtbl.iter (fun _ node -> node.color <-color) graph
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
                    let succ = make_instr_stmt_Instr (instrs,stmt) in
                      explore_succ graph node succ
                | _ -> 
                    List.iter (explore_succ graph node) (List.map make_instr_stmt_Stmt stmt.succs)
              end
          | Instr(instr::instrs,stmt) ->
              let succ = make_instr_stmt_Instr (instrs,stmt) in
                explore_succ graph node succ
          | Instr([],stmt) ->
              List.iter (explore_succ graph node) (List.map make_instr_stmt_Stmt stmt.succs)
      end
  and explore_succ graph node succ =
    let node' = make_graph_node graph succ in
      node.nexts <- node'::node.nexts;
      make_graph_forward graph node'
  in

  let make_graph_backward graph =
    Hashtbl.iter
      begin
        fun _ node ->
          node.color <- 0;
          List.iter 
            begin
              fun node' ->
                node'.prevs <- node::node'.prevs
            end node.nexts
      end graph
  in
  let graph = Hashtbl.create 100 in
  let stmt = List.hd fundec.sallstmts in
  let root = make_graph_node graph (make_instr_stmt_Stmt stmt) in
    make_graph_forward graph root;
    make_graph_backward graph ;
    graph,root
;;

let graph__get_nodes_satisfying graph predicate =
  Hashtbl.fold 
    begin
      fun _ node lst ->
        if predicate node then node::lst else lst
    end
    graph []
;;

(*
let graph__get_node graph instr_stmt =
  Printf.printf "-----------------------------------------------\n";
  Printf.printf "%s\n" (print_instr_stmt instr_stmt);
  print_graph graph;
  let result = Hashtbl.find graph instr_stmt in
    Printf.printf "found.\n";
    result
;;
 *)


let reachable graph src tar =
  graph__set_color graph 0;
  let rec dfs graph tar src =
    if tar == src then true
    else if tar.color = 1 then false 
    else
      begin
        tar.color <- 1;
        List.fold_left 
          begin
            fun t prev -> if t then true else dfs graph prev src
          end false tar.prevs
      end
  in
    dfs graph tar src 
;;

let prioritize job = 
  if (Executeargs.run_args.Executeargs.arg_cfg_pruning) then
    begin
      let graph,root = make_graph (List.hd job.state.callstack) in
      let targets = graph__get_nodes_satisfying graph 
                      begin
                        fun node -> match node.obj with
                          | Instr((Call(_,Lval(Var(varinfo),_),exps,_))::_,_) 
                              when varinfo.vname = "__ASSERT" -> true
                          | _ -> false
                      end
      in
      (*let source = graph__get_node graph (get_instr_stmt job) in*)
      let get_predicate job node = 
        match job.instrList,job.stmt,node.obj with
          | [],stmt,Stmt(stmt') when stmt==stmt' -> true
          | [],_,_ -> false
          | instrs,_,Instr(instrs',_) when instrs==instrs' -> true
          | _ -> false
      in
      let sources = graph__get_nodes_satisfying graph (get_predicate job) in
      assert(List.length sources = 1);
      let source = List.hd sources in
      let reachable_from_targets = List.fold_left
                                        (fun t tar -> if t then true else
                                           reachable graph source tar)
                                     false targets
      in
        if reachable_from_targets then
          lifo job
        else 
          neg_infinity
    end
  else 
    lifo job
;;

