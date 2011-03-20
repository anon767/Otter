(** Closest-to-targets first strategy: jobs are weighted by 1/n where n is the shortest distance to a call to __FAILURE() or a target function. *)

open OcamlUtilities
open CilUtilities
open OtterCFG
open OtterCore


let get_distances =
    (* hierarchically memoize to reduce the cost of hashing *)
    let module FundecsHash = Hashtbl.Make (ListPlus.MakeHashedList (CilData.CilFundec)) in
    let module InstructionListHash = Hashtbl.Make (ListPlus.MakeHashedList (Instruction)) in
    let module InstructionHash = Hashtbl.Make (Instruction) in

    let source_hash = InstructionHash.create 0 in
    let get_distances source =
        let context_hash =
            try
                InstructionHash.find source_hash source
            with Not_found ->
                let context_hash = InstructionListHash.create 0 in
                InstructionHash.add source_hash source context_hash;
                context_hash
        in
        fun context ->
            let target_fundecs_hash =
                try
                    InstructionListHash.find context_hash context
                with Not_found ->
                    let target_fundecs_hash = FundecsHash.create 0 in
                    InstructionListHash.add context_hash context target_fundecs_hash;
                    target_fundecs_hash
            in
            fun target_fundecs ->
                let backotter_line_targets_hash =
                    try
                        FundecsHash.find target_fundecs_hash target_fundecs
                    with Not_found ->
                        let backotter_line_targets_hash = InstructionListHash.create 0 in
                        FundecsHash.add target_fundecs_hash target_fundecs backotter_line_targets_hash;
                        backotter_line_targets_hash
                in
                fun backotter_line_targets ->
                    let line_targets_hash =
                        try
                            InstructionListHash.find backotter_line_targets_hash backotter_line_targets
                        with Not_found ->
                            let line_targets_hash = InstructionListHash.create 0 in
                            InstructionListHash.add backotter_line_targets_hash backotter_line_targets line_targets_hash;
                            line_targets_hash
                    in
                    fun line_targets ->
                        try
                            InstructionListHash.find line_targets_hash line_targets
                        with Not_found ->
                            Profiler.global#call "ClosestToTargetsStrategy.get_distances (uncached)" begin fun () ->
                                let call_sites = Instruction.call_sites (Instruction.fundec_of source) in
                                let target_instrs = List.filter begin fun call_site ->
                                    List.exists (CilData.CilFundec.equal call_site.Instruction.fundec) target_fundecs
                                end call_sites in
                                let all_targets = target_instrs @ line_targets @ backotter_line_targets in
                                let distance =
                                    if all_targets = [] then
                                        max_int
                                    else
                                        DistanceToTargets.find_in_context source context all_targets
                                in
                                InstructionListHash.add line_targets_hash line_targets distance;
                                distance
                            end
    in
    fun ({ Instruction.file = file; _ } as source) context ->
        get_distances source context (BackOtterTargets.get_target_fundecs ()) (BackOtterTargetTracker.get_line_targets file) (LineTargets.get_line_targets file)


class ['self] t weight_fn = object (self : 'self)
    method add job = self

    method remove job = self

    method weights jobs = Profiler.global#call "ClosestToTargetsStrategy.weights" begin fun () ->
        List.map begin fun job ->
            let source = Job.get_instruction job in
            let context = Job.get_instruction_context job in
            let distance = get_distances source context in
            Output.debug_printf "Job %d has distance to target = %d@\n" job#node_id distance;
            weight_fn distance
        end jobs
    end
end

