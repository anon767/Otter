open OtterCFG

(** Find the nearest "critical edge" of the given Instruction.t, as in the ESD paper.
 *  Here, a critical edge is (instr: Instruction.t, branch: bool), where the instr must be an If-stmt, 
 *  and the branch indicates which branch it takes to reach the input instruction.
 *)
(* TODO: make this inter-procedural *)
let rec find instruction = 
    match Instruction.predecessors instruction with
    | [ pred ] -> if List.length (Instruction.successors pred) > 1 then Some (pred, instruction) else find pred
    | _ -> None

