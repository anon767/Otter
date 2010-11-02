(** Find distances between instructions. *)


(**/**) (* various helpers *)
module InstructionSet = Set.Make (Instruction)
module InstructionTargetsHash = Hashtbl.Make (struct
    type t = Instruction.t * Instruction.t list
    let equal (x, xt) (y, yt) = try List.for_all2 Instruction.equal (x::xt) (y::yt) with Invalid_argument "List.for_all2" -> false
    let hash (x, xt) = List.fold_left (fun h x -> 33 * h + Instruction.hash x) 0 (x::xt)
end)
(**/**)


(** Find the shortest distance from an {!Instruction.t} to a list of target {!Instruction.t}s.

    Distances are calculated by counting instructions along a path up to a function return within the function of
    the source instruction. If a function call occurs along a path, the minimum of the distance from the first
    instruction to the targets in the call targets, or the shortest distance through the call targets, is added.

    @return the shortest distance to one of the targets, or {!max_int} if none of the targets are reachable.
*)
let find =
    let memotable = InstructionTargetsHash.create 0 in
    fun instr targets ->
        if targets = [] then invalid_arg "find: targets must be a non-empty list";
        try
            InstructionTargetsHash.find memotable (instr, targets)

        with Not_found ->
            let calc_dist instrs worklist =
                if List.exists (fun instr -> List.exists (Instruction.equal instr) targets) instrs then
                    (0, worklist)
                else
                    List.fold_left begin fun (dist, worklist) instr ->
                        try (min dist (InstructionTargetsHash.find memotable (instr, targets)), worklist)
                        with Not_found -> (dist, InstructionSet.add instr worklist)
                    end (max_int, worklist) instrs
            in
            let rec update worklist =
                let instr = InstructionSet.choose worklist in
                let dist =
                    try
                        InstructionTargetsHash.find memotable (instr, targets)
                    with Not_found ->
                        InstructionTargetsHash.add memotable (instr, targets) max_int;
                        max_int
                in

                (* compute the new distance by taking the minimum of:
                        - 1 + the minimum distance of its successors + the minimum distance through its call targets,
                        - or, 1 + the minimum distance of its call targets;
                   adding uncomputed successors and call targets to the worklist *)
                let worklist' = worklist in
                let call_targets = Instruction.call_targets instr in
                let succ_dist, worklist' =
                    let succ_dist, worklist' = calc_dist (Instruction.successors instr) worklist' in
                    let succ_dist = match call_targets with
                        | [] -> succ_dist
                        | call_targets -> succ_dist + List.fold_left (fun d call_target -> min d (DistanceToReturn.find call_target)) max_int call_targets
                    in
                    let succ_dist = if succ_dist < 0 then dist (* overflow *) else succ_dist in
                    (succ_dist, worklist')
                in
                let target_dist, worklist' = calc_dist call_targets worklist' in
                let dist' =
                    let dist' = 1 + min succ_dist target_dist in
                    if dist' < 0 then dist (* overflow *) else dist'
                in
                (* update the distance if changed *)
                if dist' <> dist then InstructionTargetsHash.replace memotable (instr, targets) dist';

                let worklist =
                    (* if this is not a target and the worklist is updated, process the worklist first because this
                       instruction will have to be updated again later. *)
                    if dist' > 1 && not (InstructionSet.equal worklist' worklist) then worklist' else

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
            InstructionTargetsHash.find memotable (instr, targets)


(** Find the shortest distance from an {!Instruction.t} to a list of target {!Instruction.t}s through function returns
    in a calling context given as a list of {!Instruction.t} successors of function calls.

    Distances through function returns are computed by recursively unwinding the calling context, taking the
    shortest distance from the source instruction through function returns down the calling context to targets in the
    calling context.

    @return the shortest distance to one of the targets, or {!max_int} if none of the targets are reachable.
*)
let find_in_context instr targets context =
    (* compute the distance from the instr through function returns to targets in the call context *)
    let rec unwind dist return_dist = function
        | call_return::context ->
            let dist =
                let dist' = return_dist + find call_return targets in
                if dist' < 0 then dist (* overflow *) else min dist dist'
            in
            let return_dist = return_dist + DistanceToReturn.find call_return in
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
