(** Least-covered-instruction first Otter job queue. *)

open DataStructures
open OtterCore

module InstructionMap = struct
    include Map.Make (OtterCFG.Instruction)
    let find instr coverage = try find instr coverage with Not_found -> 0
end
module InstructionPriority = PrioritySearchQueue.Make
    (OtterCFG.Instruction)
    (struct type t = int let compare = Pervasives.compare end)

class ['job] t = object
    (* coverage is initially zero for every instruction *)
    val coverage = InstructionMap.empty
    val queue = InstructionPriority.empty

    method put (job : 'job) =
        (* score and enqueue *)
        let instr = Job.get_instruction job in
        let count, jobs =
            try
                InstructionPriority.lookup instr queue
            with InstructionPriority.Key ->
                let count = InstructionMap.find instr coverage in
                (count, RandomBag.empty)
        in
        let jobs = RandomBag.put job jobs in
        let queue = InstructionPriority.insert instr count jobs queue in
        {< queue = queue >}

    method get =
        try
            (* update coverage and dequeue *)
            let instr, count, jobs = InstructionPriority.find_min queue in
            match RandomBag.get jobs with
                | Some (jobs, job) ->
                    let count = count + 1 in
                    let coverage = InstructionMap.add instr count coverage in
                    let queue =
                        if RandomBag.is_empty jobs then
                            InstructionPriority.delete_min queue
                        else
                            InstructionPriority.insert instr count jobs queue
                    in
                    Some ({< coverage = coverage; queue = queue >}, job)
                | None ->
                    failwith "Impossible!"
        with InstructionPriority.Empty ->
            None
end

