
(**/**) (* various helpers *)
module InstructionSet = Set.Make (Instruction)
(**/**)

(** Find the shortest distance from an {!Instruction.t} to a function return. *)
let find =
    let memotable = Hashtbl.create 0 in
    fun instr ->
        try
            Hashtbl.find memotable instr

        with Not_found ->
            let lookup instr worklist = try (Hashtbl.find memotable instr, worklist) with Not_found -> (max_int, InstructionSet.add instr worklist) in
            let calc_dist instrs worklist = match instrs with
                | [] -> (0, worklist)
                | instrs -> List.fold_left (fun (dist, worklist) instr -> let dist', worklist = lookup instr worklist in (min dist dist', worklist)) (max_int, worklist) instrs
            in
            let rec update worklist =
                let instr = InstructionSet.choose worklist in

                (* compute the distance by taking 1 + the minumum of its successors, and the minimum distances of its call targets,
                   adding uncomputed successors and call targets to a new worklist *)
                let worklist' = worklist in
                let succ_dist, worklist' = calc_dist (Instruction.successors instr) worklist' in
                let target_dist, worklist' = calc_dist (Instruction.call_targets instr) worklist' in
                let dist =
                    let dist = 1 + succ_dist in
                    if dist < 0 then
                        max_int (* overflow *)
                    else
                        let dist = dist + target_dist in
                        if dist < 0 then
                            max_int (* overflow *)
                        else
                            dist
                in
                Hashtbl.add memotable instr dist;

                (* if worklist is not updated, add the predecessors and call sites to worklist *)
                let worklist =
                    if not (InstructionSet.equal worklist' worklist) then worklist' else
                    List.fold_left (fun instrs instr -> InstructionSet.add instr instrs) worklist
                        (List.rev_append (Instruction.predecessors instr) (Instruction.call_sites instr))
                in

                (* recurse on the remainder of the worklist *)
                let worklist = InstructionSet.remove instr worklist in
                if not (InstructionSet.is_empty worklist) then update worklist
            in
            update (InstructionSet.singleton instr);
            Hashtbl.find memotable instr
