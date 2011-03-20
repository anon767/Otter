(** Closest-to-uncovered-instruction Otter job strategy:jobs are weighted by 1/n where n is the shortest distance to
    an uncovered instruction.

    Based on a similar strategy from KLEE.
*)

open DataStructures
open OtterCFG
open OtterCore

(**/**) (* various helpers *)
module InstructionSet = Set.Make (Instruction)
module InstructionMap = Map.Make (Instruction)
module CoverageMap = struct
    include Map.Make (Instruction)
    let find instr coverage = try find instr coverage with Not_found -> 0
end
(**/**)

let arg_tracked_only = ref false

class ['self] t = object (self : 'self)
    (* both coverage and distances are initially zero for every instruction *)
    val coverage = CoverageMap.empty
    val distances = lazy CoverageMap.empty

    (* Determine if the instr is tracked in this strategy.
     * An untracked instr is always covered *)
    method private tracked instr =
        if (!arg_tracked_only) then
            let fname = instr.Instruction.fundec.Cil.svar.Cil.vname in
            let file = instr.Instruction.file in
            TrackingFunctions.isTracked fname (TrackingFunctions.trackedFns file)
        else
            true

    method private update_distances instr coverage =
        let rec update worklist distances =
            (* pick the an instruction from the worklist *)
            let instr, visited = InstructionMap.max_binding worklist in
            let worklist = InstructionMap.remove instr worklist in

            (* compute the new distance by taking the minimum of:
                    - 0 if the instruction is uncovered;
                    - or, 1 + the minimum distance of its successors + the minimum distance through its call targets. *)
            let dist =
                (* An untracked instr is always covered *)
                if self#tracked instr && CoverageMap.find instr coverage = 0 then
                    0
                else
                    let calc_dist instrs = List.fold_left (fun dist instr -> min dist (CoverageMap.find instr distances)) max_int instrs in

                    (* avoid backward loops, as they may lead to an (almost) infinite loop *)
                    let successors = List.filter (fun succ -> succ.Instruction.stmt.Cil.sid > instr.Instruction.stmt.Cil.sid) (Instruction.successors instr) in
                    let dist = calc_dist successors in

                    (* avoid recursive calls, as they may lead to an (almost) infinite loop *)
                    let call_targets = List.filter (fun call_target -> not (InstructionSet.mem call_target visited)) (Instruction.call_targets instr) in
                    let dist = match call_targets with
                        | [] ->
                            (* no call targets, just successors *)
                            dist
                        | call_targets ->
                            (* compute the distance through call targets and successors *)
                            let through_dist =
                                let through_dist = dist + List.fold_left (fun d call_target -> min d (DistanceToReturn.find call_target)) max_int call_targets in
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
                let dist' = CoverageMap.find instr distances in
                if dist <> dist' then
                    (true, CoverageMap.add instr dist distances)
                else
                    (false, distances)
            in

            (* if updated, add this instruction's predecessors and call sites to the worklist. *)
            let worklist =
                if updated then
                    let visited = if Instruction.equal instr (Instruction.fundec_of instr) then InstructionSet.add instr visited else visited in
                    List.fold_left begin fun worklist instr ->
                        let visited' = try InstructionMap.find instr worklist with Not_found -> InstructionSet.empty in
                        InstructionMap.add instr (InstructionSet.union visited visited') worklist
                    end worklist (List.rev_append (Instruction.predecessors instr) (Instruction.call_sites instr))
                else
                    worklist
            in

            (* recurse on the remainder of the worklist *)
            if not (InstructionMap.is_empty worklist) then
                update worklist distances
            else
                distances
        in
        update (InstructionMap.singleton instr InstructionSet.empty) (Lazy.force distances)

    method private calculate_distance job =
        (* compute the distance from the instr through function returns to uncovered in the call context *)
        let distances = Lazy.force distances in
        let rec unwind dist return_dist = function
            | call_return::context ->
                let dist =
                    let dist' = return_dist + CoverageMap.find call_return distances in
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
        let dist = CoverageMap.find instr distances in
        let return_dist = DistanceToReturn.find instr in
        unwind dist return_dist context

    method add job = self

    method remove job =
        (* update coverage and distances *)
        let instr = Job.get_instruction job in
        let count = CoverageMap.find instr coverage + 1 in
        let coverage = CoverageMap.add instr count coverage in
        let distances = lazy (self#update_distances instr coverage) in
        {< coverage = coverage; distances = distances >}

    method weights jobs =
        List.map (fun job -> 1. /. float_of_int (self#calculate_distance job)) jobs
end

let options = [
	("--distance-to-uncovered-tracked-only",
		Arg.Set arg_tracked_only,
        " Instructions in untracked functions (specified by --(un)tracked-functions) are considered covered in the beginning.\n"
	);
]

