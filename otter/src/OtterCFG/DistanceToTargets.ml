
(**/**) (* various helpers *)
module InstructionSet = Set.Make (Instruction)
module InstructionTargetsHash = Hashtbl.Make (struct
    type t = Instruction.t * Instruction.t list
    let equal (x, xt) (y, yt) = List.for_all2 Instruction.equal (x::xt) (y::yt)
    let hash (x, xt) = List.fold_left (fun h x -> 33 * h + Instruction.hash x) 0 (x::xt)
end)
(**/**)

(** Find the shortest distance from an {!Instruction.t} to a list of target {!Instruction.t}s. *)
let find =
    let memotable = InstructionTargetsHash.create 0 in
    fun instr targets ->
        try
            InstructionTargetsHash.find memotable (instr, targets)

        with Not_found ->
            let calc_dist instrs worklist =
                if List.exists (fun instr -> List.exists (Instruction.equal instr) targets) instrs then
                    (0, worklist)
                else
                    List.fold_left begin fun (dist, worklist) instr ->
                        try (min dist (InstructionTargetsHash.find memotable (instr, targets)), worklist)
                        with Not_found -> (min dist max_int, InstructionSet.add instr worklist)
                    end (max_int, worklist) instrs
            in
            let rec update worklist =
                let instr = InstructionSet.choose worklist in
                let dist = try InstructionTargetsHash.find memotable (instr, targets) with Not_found -> max_int in

                (* compute the new distance by taking 1 + the minimum distance of its successors + the minimum distance
                   of its call targets, or the minimum distance of its return targets if it's a return instruction,
                   adding uncomputed successors and call targets or return targets to the worklist *)
                let worklist' = worklist in
                let dist', worklist' = match Instruction.return_targets instr with
                    | [] ->
                        (* not a return instruction *)
                        let succ_dist, worklist' = calc_dist (Instruction.successors instr) worklist' in
                        let target_dist, worklist' = calc_dist (Instruction.call_targets instr) worklist' in
                        let dist' = succ_dist + target_dist in
                        let dist' = if dist' > 0 then dist' else max_int in (* overflow *)
                        (dist', worklist')
                    | instrs ->
                        calc_dist instrs worklist'
                in
                let dist' =
                    let dist' = 1 + dist' in
                    if dist' > 0 then dist' else max_int (* overflow *)
                in
                (* update the distance if changed *)
                if dist' <> dist then InstructionTargetsHash.replace memotable (instr, targets) dist';

                let worklist =
                    (* if worklist is updated, use it because this instruction will have to be updated again later. *)
                    if not (InstructionSet.equal worklist' worklist) then worklist' else

                    (* if the worklist is not updated and the distance has changed, then add the predecessors and
                       call sites, or the return targets if it is a call instruction, to the worklist. *)
                    if dist' <> dist then
                        let instrs = match Instruction.return_sites instr with
                            | [] ->
                                (* not a call instruction *)
                                List.rev_append (Instruction.predecessors instr) (Instruction.call_sites instr)
                            | instrs ->
                                instrs
                        in
                        List.fold_left (fun worklist instr -> InstructionSet.add instr worklist) worklist instrs
                    else
                        worklist
                in

                (* recurse on the remainder of the worklist *)
                let worklist = InstructionSet.remove instr worklist in
                if not (InstructionSet.is_empty worklist) then update worklist
            in
            update (InstructionSet.singleton instr);
            InstructionTargetsHash.find memotable (instr, targets)
