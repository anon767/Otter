(** Find distances between instructions. *)


(**/**) (* various helpers *)
module InstructionStack = DataStructures.StackSet.Make (Instruction)
module InstructionSet = Set.Make (Instruction)
module InstructionTargetsHash = Hashtbl.Make (struct
    type t = Instruction.t * InstructionSet.t
    let equal (x, xt) (y, yt) = Instruction.equal x y && InstructionSet.equal xt yt
    let hash (x, xt) = Hashtbl.hash (Instruction.hash x, Hashtbl.hash xt)
end)
(**/**)


(** Find the shortest distance from an {!Instruction.t} to a list of target {!Instruction.t}s.

    Distances are calculated by counting instructions along a path up to a function return within the function of
    the source instruction. If a function call occurs along a path, the minimum of the distance from the first
    instruction to the targets in the call targets, or the shortest distance through the call targets, is added.

    @return the shortest distance to one of the targets, or {!max_int} if none of the targets are reachable.
*)
let find =
    let distance_hash = InstructionTargetsHash.create 0 in
    fun instr targets ->
        if targets = [] then invalid_arg "find: targets must be a non-empty list";
        let targets = List.fold_left (fun targets target -> InstructionSet.add target targets) InstructionSet.empty targets in
        try
            InstructionTargetsHash.find distance_hash (instr, targets)

        with Not_found -> OcamlUtilities.Profiler.global#call "DistanceToTargets.find (uncached)" begin fun () ->
            let rec update worklist =
                let worklist = OcamlUtilities.Profiler.global#call "update" begin fun () ->
                    (* pick the an instruction from the worklist *)
                    let instr, worklist = InstructionStack.pop worklist in

                    (* compute the new distance by taking the minimum of:
                            - 0 if the instruction is a target;
                            - or, 1 + the minimum distance of its successors + the minimum distance through its call targets;
                            - or, 1 + the minimum distance of its call targets;
                       adding uncomputed successors and call targets to the worklist *)
                    let dist, worklist =
                        if InstructionSet.mem instr targets then
                            (0, worklist)
                        else
                            let calc_dist instrs worklist =
                                (* if any dependencies are uncomputed, add them to the worklist *)
                                List.fold_left begin fun (dist, worklist) instr ->
                                    try (min dist (InstructionTargetsHash.find distance_hash (instr, targets)), worklist)
                                    with Not_found -> (dist, InstructionStack.push instr worklist)
                                end (max_int, worklist) instrs
                            in

                            let dist, worklist = calc_dist (Instruction.successors instr) worklist in
                            let dist, worklist = match Instruction.call_targets instr with
                                | [] ->
                                    (* no call targets, just successors *)
                                    (dist, worklist)
                                | call_targets ->
                                    (* compute the distance through call targets and successors *)
                                    let through_dist =
                                        let through_dist = dist + List.fold_left (fun d call_target -> min d (DistanceToReturn.find call_target)) max_int call_targets in
                                        if through_dist < 0 then max_int (* overflow *) else through_dist
                                    in
                                    (* compute the distances of call targets *)
                                    let call_target_dist, worklist = calc_dist call_targets worklist in
                                    (* take the minimum of the above *)
                                    (min through_dist call_target_dist, worklist)
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
                            let dist' = InstructionTargetsHash.find distance_hash (instr, targets) in
                            if dist <> dist' then InstructionTargetsHash.replace distance_hash (instr, targets) dist;
                            dist <> dist'
                        with Not_found ->
                            InstructionTargetsHash.add distance_hash (instr, targets) dist;
                            true
                    in

                    (* if updated, add this instruction's predecessors and call sites to the worklist. *)
                    if updated then
                        List.fold_left (fun worklist instr -> InstructionStack.push instr worklist) worklist
                            (List.rev_append (Instruction.predecessors instr) (Instruction.call_sites instr))
                    else
                        worklist
                end in

                (* recurse on the remainder of the worklist *)
                if not (InstructionStack.is_empty worklist) then update worklist
            in
            update (InstructionStack.singleton instr);
            InstructionTargetsHash.find distance_hash (instr, targets)
        end


(** Find the shortest distance from an {!Instruction.t} to a list of target {!Instruction.t}s through function returns
    in a calling context given as a list of {!Instruction.t} successors of function calls.

    Distances through function returns are computed by recursively unwinding the calling context, taking the
    shortest distance from the source instruction through function returns down the calling context to targets in the
    calling context.

    @return the shortest distance to one of the targets, or {!max_int} if none of the targets are reachable.
*)
let find_in_context instr context targets = OcamlUtilities.Profiler.global#call "DistanceToTargets.find_in_context" begin fun () ->
    (* compute the distance from the instr through function returns to targets in the call context *)
    let rec unwind dist return_dist = function
        | call_return::context ->
            let dist =
                (* "+1" since call_return is the NEXT instruction after the call *)
                let dist' = return_dist + 1 + find call_return targets in
                if dist' < 0 then dist (* overflow *) else min dist dist'
            in
            (* "+1" since call_return is the NEXT instruction after the call *)
            let return_dist = return_dist + 1 + DistanceToReturn.find call_return in
            if return_dist < 0 then
                dist (* overflow; terminate since further unwindings will also overflow *)
            else
                unwind dist return_dist context
        | [] ->
            dist
    in
    (* compute the initial distance to targets and distance to function returns *)
    let dist = find instr targets in
    let return_dist = DistanceToReturn.find instr in
    unwind dist return_dist context
end

