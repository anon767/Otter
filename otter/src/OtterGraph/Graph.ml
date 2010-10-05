open Cil
open InstrStmt

type node = 
    { 
        mutable obj: InstrStmt.t;
        mutable prevs: node list;
        mutable nexts: node list;
        mutable color: int;
    }

type t = (InstrStmt.t,node) Hashtbl.t 

let print_node node = 
  let s = Printf.sprintf "Node: \x1b[34m%s\x1b[m Color: \x1b[31m%s\x1b[m\n" (InstrStmt.print ~obj:(Some node) node.obj) (if node.color = max_int then "inf" else string_of_int node.color)
  in
  let s = List.fold_left 
      begin
        fun str next -> 
          let s = Printf.sprintf "\tNext: %s\n" (InstrStmt.print ~obj:(Some next) next.obj) in
            str^s
      end
      s node.nexts
  in
  let s = List.fold_left 
      begin
        fun str next -> 
          let s = Printf.sprintf "\tPrev: %s\n" (InstrStmt.print ~obj:(Some next) next.obj) in
            str^s
      end
      s node.prevs
  in
    s

let print_graph graph =
  let lst = Hashtbl.fold (fun k v lst -> (print_node v)::lst) graph [] in
  let lst = List.sort String.compare lst in
    List.iter (fun s -> Format.printf "%s" s) lst;
  ()

let set_color graph color =
  Hashtbl.iter (fun _ node -> node.color <-color) graph

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
                    let succ = InstrStmt.make_instr (instrs,stmt) in
                      explore_succ graph node succ
                | Cil.Switch _ 
                | Cil.Break _ 
                | Cil.Continue _ -> failwith "Cil programs after prepareCFG should not contain switches, breaks or continues"
                | Cil.Goto (stmtref,_) ->
                    assert( !stmtref == List.hd stmt.succs);
                    List.iter (explore_succ graph node) (List.map InstrStmt.make_stmt stmt.succs)
                | Cil.Block (block)
                | Cil.Loop (block,_,_,_) ->
                    (* From driver.ml, it seems that Loop's succs is bogus.
                     * We have to look at block.bstmts
                     *)
				    let nextStmt = List.hd block.bstmts in
                    explore_succ graph node (InstrStmt.make_stmt nextStmt)
                | _ -> 
                    List.iter (explore_succ graph node) (List.map InstrStmt.make_stmt stmt.succs)
              end
          | Instr(instr::instrs,stmt) ->
              let succ = InstrStmt.make_instr (instrs,stmt) in
                explore_succ graph node succ
          | Instr([],stmt) ->
              List.iter (explore_succ graph node) (List.map InstrStmt.make_stmt stmt.succs)
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
          node.color <- max_int;
          List.iter 
            begin
              fun node' ->
                node'.prevs <- node::node'.prevs
            end node.nexts
      end graph
  in
  let graph = Hashtbl.create 100 in
  let stmt = List.hd fundec.sallstmts in
  let root = make_graph_node graph (InstrStmt.make_stmt stmt) in
    make_graph_forward graph root;
    make_graph_backward graph ;
    graph,root


let filter_nodes graph predicate : node list =
  Hashtbl.fold 
    begin
      fun _ node lst ->
        if predicate node then node::lst else lst
    end
    graph []

let backward_distance graph src tar =
  let rec bfs tars =
    let tars' = 
      List.fold_left 
      (
        fun (lst) tar ->
          List.fold_left
            (
              (* prev is one of the previous nodes of tar
               * see if we want to explore prev
               *)
              fun (lst) prev ->
                let rec impl lst prev = 
                  if prev.color < max_int (* explored *) then (lst) else
                  let prev_outdeg = List.length prev.nexts in
                  if prev_outdeg = 1 then
                    ( 
                      prev.color <- tar.color;
                      (* if prev==src then ([],src.color) else impl (List.hd prev.nexts) *)
                      if List.length prev.prevs = 0 then (lst)
                      else List.fold_left impl lst prev.prevs
                    )
                  else (* if prev_outdeg > 1 then *)
                    (
                      prev.color <- tar.color + 1;
                      prev::lst
                    )
                in
                  impl lst prev
            )
            (lst) tar.prevs
      )
      ([]) tars
    in
    let srcdist = src.color in
      if srcdist < max_int then srcdist 
      else if tars' = [] then max_int 
      else bfs tars'
  in
    set_color graph max_int;
    tar.color <- 0;
    bfs [tar] 
