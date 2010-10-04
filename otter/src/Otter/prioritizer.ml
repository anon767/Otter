open OtterBytes
open OtterCore
open OtterGraph
open Types
open Job
open Cil
open Graph

(* TODO (martin) refactor this module *)

(* TODO (martin): move failing_predicate and target to another module *)
type failing_predicate =
    (* TODO (martin): make the condition a list of bytes *)
    | FailingCondition of state * Bytes.bytes 
    | FailingPaths of fork_decision list list

(* target to be used in prioritizer *)
type target = {
  target_func: Cil.fundec;
  target_predicate: failing_predicate;
}


(** 
  * Simple prioritizer
  *)
let time = ref 0.0
let timer () =
    (time:=(!time+.1.0);!time)


let prioritize assertfn targets job = 
  if (!Executeargs.arg_cfg_pruning) then
    begin
      let graph,root = make_graph (List.hd job.state.callstack) in
      let target_nodes = 
        graph__get_nodes_satisfying graph 
          begin
            fun node -> match node.obj with
              | InstrStmt.Instr((Call(_,Lval(Var(varinfo),_),exps,_))::_,_)  ->
                  if varinfo = assertfn.svar then true else
                    List.fold_left (fun b t -> if t.target_func.svar == varinfo then true else b) false targets
              | _ -> false
          end
      in
      (*let source = graph__get_node graph (get_instr_stmt job) in*)
      let get_predicate job node = 
        match job.instrList,job.stmt,node.obj with
          | [],stmt,InstrStmt.Stmt(stmt') when stmt==stmt' -> true
          | [],_,_ -> false
          | instrs,_,InstrStmt.Instr(instrs',_) when instrs==instrs' -> true
          | _ -> false
      in
      let sources = graph__get_nodes_satisfying graph (get_predicate job) in
      assert(List.length sources = 1);
      let source = List.hd sources in
    (*
      let backward_reachable_from_targets = List.fold_left
                                        (fun t tar -> if t then true else
                                           backward_reachable graph source tar)
                                     false target_nodes
      in
        if backward_reachable_from_targets then
          lifo job
        else 
          neg_infinity
     *)
      graph__set_color graph max_int;
      let backward_distance_from_targets = List.fold_left
                                             (fun d tar -> 
                                                let d' = backward_distance graph source tar in
                                                  min d d'
                                             )
                                             max_int target_nodes
      in (float_of_int backward_distance_from_targets)
    end
  else 
    timer ()

let max_priority = 1073741822.0

