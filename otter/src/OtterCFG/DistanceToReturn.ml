
(**/**) (* various helpers *)
module InstructionSet = Set.Make (Instruction)
module InstructionHash = Hashtbl.Make (Instruction)
(**/**)

(** Find the shortest distance from an {!Instruction.t} to a function return. *)
let find =
    let memotable = InstructionHash.create 0 in
    fun instr ->
        try
            InstructionHash.find memotable instr

        with Not_found ->
            let calc_dist instrs worklist = match instrs with
                | [] ->
                    (0, worklist)
                | instrs ->
                    List.fold_left begin fun (dist, worklist) instr ->
                        try (min dist (InstructionHash.find memotable instr), worklist)
                        with Not_found -> (dist, InstructionSet.add instr worklist)
                    end (max_int, worklist) instrs
            in
            let rec update worklist =
                let instr = InstructionSet.choose worklist in
                let dist =
                    try
                        InstructionHash.find memotable instr
                    with Not_found ->
                        InstructionHash.add memotable instr max_int;
                        max_int
                in

                (* compute the new distance by taking 1 + the minimum distance of its successors + the minimum distance
                   of its call targets, adding uncomputed successors and call targets to the worklist *)
                let worklist' = worklist in
                let succ_dist, worklist' = calc_dist (Instruction.successors instr) worklist' in
                let target_dist, worklist' = calc_dist (Instruction.call_targets instr) worklist' in
                let dist' =
                    let dist' = 1 + succ_dist in
                    if dist' < 0 then
                        dist (* overflow *)
                    else
                        let dist' = dist' + target_dist in
                        if dist' < 0 then
                            dist (* overflow *)
                        else
                            dist'
                in
                (* update the distance if changed *)
                if dist' <> dist then InstructionHash.replace memotable instr dist';

                let worklist =
                    (* if worklist is updated, use it because this instruction will have to be updated again later. *)
                    if not (InstructionSet.equal worklist' worklist) then worklist' else

                    (* if the worklist is not updated and the distance has changed, then add the predecessors and
                       call sites to the worklist. *)
                    if dist' <> dist then
                        List.fold_left (fun worklist instr -> InstructionSet.add instr worklist) worklist
                            (List.rev_append (Instruction.predecessors instr) (Instruction.call_sites instr))
                    else
                        worklist
                in

                (* recurse on the remainder of the worklist *)
                let worklist = InstructionSet.remove instr worklist in
                if not (InstructionSet.is_empty worklist) then update worklist
            in
            update (InstructionSet.singleton instr);
            InstructionHash.find memotable instr
