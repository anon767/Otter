(** Closest-to-uncovered-instruction first Otter job queue, based on KLEE. *)

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
    val distances = InstructionMap.empty
    val queue = RandomAccessList.empty

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
                    let calc_dist instrs = List.fold_left (fun dist instr -> min dist (InstructionMap.find instr distances)) max_int instrs in

                    let dist = calc_dist (Instruction.successors instr) in
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
        update (InstructionStack.singleton instr) distances

    method private calculate_distance job =
        (* compute the distance from the instr through function returns to uncovered in the call context *)
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

    method put job =
        {< queue = RandomAccessList.cons job queue >}

    method get =
        match RandomAccessList.list_view queue with
            | `Nil ->
                None
            | `Cons (first, rest) ->
                (* dequeue the job with the shortest distance to an uncovered instruction *)
                let rec get job dist index count work = match RandomAccessList.list_view work with
                    | `Cons (job', rest') ->
                        let count = count + 1 in
                        let dist' = self#calculate_distance job' in
                        if dist' < dist then
                            get job' dist' count count rest'
                        else
                            get job dist index count rest'
                    | `Nil when index >= 0 ->
                        (RandomAccessList.update index first rest, job)
                    | `Nil ->
                        (rest, job)
                in
                let queue, job = get first (self#calculate_distance first) ~-1 ~-1 rest in

                (* update coverage and distances, and return *)
                let instr = Job.get_instruction job in
                let coverage = InstructionMap.add instr (InstructionMap.find instr coverage + 1) coverage in
                let distances = self#update_distances instr coverage in
                Some ({< coverage = coverage; distances = distances; queue = queue >}, job)
end

