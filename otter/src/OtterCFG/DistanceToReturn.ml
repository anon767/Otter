(** Find distances from instructions to function returns. *)


(**/**) (* various helpers *)
module InstructionPriority = DataStructures.PrioritySearchQueue.Make (Instruction) (struct type t = int let compare = Pervasives.compare end)
module InstructionHash = Hashtbl.Make (Instruction)
(**/**)


(** Find the shortest distance from an {!Instruction.t} to a function return.

    Distances are calculated by counting instructions along a path to a function return within the function of
    the source instruction. If a function call occurs along a path, the shortest distance through the call targets is
    added.

    @return the shortest distance to a function return, or {!max_int} if no function returns are reachable.
*)
let find =
    let memotable = InstructionHash.create 0 in
    fun instr ->
        try
            InstructionHash.find memotable instr

        with Not_found ->
            let rec update worklist =
                (* pick the highest priority instruction from the worklist *)
                let instr, priority, () = InstructionPriority.find_min worklist in
                let worklist = InstructionPriority.delete_min worklist in

                (* compute the new distance by taking the minimum of:
                        - 0 if the instruction is a return (has no successors);
                        - or, 1 + the minimum distance of its successors + the minimum distance through its call targets;
                   adding uncomputed successors and call targets to the worklist *)
                let dist, worklist =
                    if Instruction.successors instr = [] then
                        (0, worklist)
                    else
                        let calc_dist instrs worklist =
                            List.fold_left begin fun (dist, worklist) instr ->
                                try (min dist (InstructionHash.find memotable instr), worklist)
                                with Not_found -> (dist, InstructionPriority.insert instr priority () worklist)
                            end (max_int, worklist) instrs
                        in

                        let dist, worklist = calc_dist (Instruction.successors instr) worklist in
                        let dist, worklist = match Instruction.call_targets instr with
                            | [] ->
                                (* no call targets, just successors *)
                                (dist, worklist)
                            | call_targets ->
                                (* compute the distance through call targets and successors *)
                                let through_dist, worklist = calc_dist call_targets worklist in
                                let dist =
                                    let dist = dist + through_dist in
                                    if dist < 0 then max_int (* overflow *) else dist
                                in
                                (dist, worklist)
                        in
                        let dist =
                            let dist = 1 + dist in
                            if dist < 0 then max_int (* overflow *) else dist
                        in
                        (dist, worklist)
                in

                (* update the distance if changed *)
                let updated =
                    try
                        let dist' = InstructionHash.find memotable instr in
                        if dist <> dist' then InstructionHash.replace memotable instr dist;
                        dist <> dist'
                    with Not_found ->
                        InstructionHash.add memotable instr dist;
                        true
                in

                (* if updated, add this instruction's predecessors and call sites to the worklist. *)
                let worklist =
                    if updated then List.fold_left (fun worklist instr -> InstructionPriority.insert instr priority () worklist) worklist
                            (List.rev_append (Instruction.predecessors instr) (Instruction.call_sites instr))
                    else
                        worklist
                in

                (* recurse on the remainder of the worklist *)
                if not (InstructionPriority.is_empty worklist) then update worklist
            in
            update (InstructionPriority.singleton instr max_int ());
            InstructionHash.find memotable instr

