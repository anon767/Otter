(** Closest-to-uncovered-instruction Otter job strategy:jobs are weighted by 1/n where n is the shortest distance to
    an uncovered instruction.

    Based on a similar strategy from KLEE.
*)

open DataStructures
open OtterCFG
open OtterCore

(**/**) (* various helpers *)
module InstructionStack = StackSet.Make (Instruction)
module InstructionMap = struct
    include Map.Make (Instruction)
    let find instr coverage = try find instr coverage with Not_found -> 0
end
(**/**)


class ['self] t = object (self : 'self)
    (* both coverage and distances are initially zero for every instruction *)
    val coverage = InstructionMap.empty
    val distances = lazy InstructionMap.empty

    method private update_distances instr coverage =
        let rec update worklist distances =
            (* pick the an instruction from the worklist *)
            let instr, worklist = InstructionStack.pop worklist in

            (* compute the new distance by taking the minimum of:
                    - 0 if the instruction is uncovered;
                    - or, 1 + the minimum distance of its successors + the minimum distance through its call targets. *)
            let dist =
                if InstructionMap.find instr coverage = 0 then
                    0
                else
                    let calc_dist instrs = List.fold_left (fun dist instr ->
                    min dist (InstructionMap.find instr distances)) max_int instrs in

                    (* only follow forward successors, to avoid backward loops *)
                    let successors = List.filter (fun succ -> succ.Instruction.stmt.Cil.sid > instr.Instruction.stmt.Cil.sid) (Instruction.successors instr) in
                    let dist = calc_dist successors in
                    let dist = match Instruction.call_targets instr with
                        | [] ->
                            (* no call targets, just successors *)
                            dist
                        | call_targets ->
                            (* compute the distance through call targets and successors *)
                            let through_dist =
                                let through_dist =
                                    dist + List.fold_left (fun d call_target -> min d (DistanceToReturn.find call_target)) max_int call_targets
                                in
                                if through_dist < 0 then max_int (* overflow *) else through_dist
                            in
                            (* compute the distances of call targets *)
                            let call_target_dist = calc_dist call_targets in
                            (* take the minimum of the above *)
                            min through_dist call_target_dist
                    in
                    let dist = 1 + dist in
                    if dist < 0 then max_int (* overflow *) else dist
            in

            (* update the distance if changed *)
            let updated, distances =
                let dist' = InstructionMap.find instr distances in
                if dist <> dist' then
                    (true, InstructionMap.add instr dist distances)
                else
                    (false, distances)
            in

            (* if updated, add this instruction's predecessors and call sites to the worklist. *)
            let worklist =
                if updated then
                    List.fold_left (fun worklist instr -> InstructionStack.push instr worklist) worklist
                        (List.rev_append (Instruction.predecessors instr) (Instruction.call_sites instr))
                else
                    worklist
            in

            (* recurse on the remainder of the worklist *)
            if not (InstructionStack.is_empty worklist) then
                update worklist distances
            else
                distances
        in
        update (InstructionStack.singleton instr) (Lazy.force distances)

    method private calculate_distance job =
        (* compute the distance from the instr through function returns to uncovered in the call context *)
        let distances = Lazy.force distances in
        let rec unwind dist return_dist = function
            | call_return::context ->
                let dist =
                    let dist' = return_dist + InstructionMap.find call_return distances in
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
        let instr = Job.get_instruction job in
        let context = Job.get_instruction_context job in
        let dist = InstructionMap.find instr distances in
        let return_dist = DistanceToReturn.find instr in
        unwind dist return_dist context

    method add job = self

    method remove job =
        (* update coverage and distances *)
        let instr = Job.get_instruction job in
        let count = InstructionMap.find instr coverage + 1 in
        let coverage = InstructionMap.add instr count coverage in
        let distances = lazy (self#update_distances instr coverage) in
        {< coverage = coverage; distances = distances >}

    method weight job =
        1. /. float_of_int (self#calculate_distance job)
end

