(** Least-covered-instruction Otter strategy: jobs are weighted by 1/n where n is the coverage of the job instruction. *)

open DataStructures
open OtterCore

module InstructionMap = struct
    include Map.Make (OtterCFG.Instruction)
    let find instr coverage = try find instr coverage with Not_found -> 1
end

class ['self] t = object (self : 'self)
    (* coverage is initially zero for every instruction *)
    val coverage = InstructionMap.empty

    method add job = self

    method remove job =
        (* update coverage *)
        let instr = Job.get_instruction job in
        let count = InstructionMap.find instr coverage + 1 in
        let coverage = InstructionMap.add instr count coverage in
        {< coverage = coverage >}

    method weights jobs =
        List.map (fun job -> 1. /. float_of_int (InstructionMap.find (Job.get_instruction job) coverage)) jobs
end

