(** Closest-to-targets first strategy: jobs are weighted by 1/n where n is the shortest distance to a call to __FAILURE() or a target function. *)

open OcamlUtilities
open CilUtilities
open OtterCFG
open OtterCore


module JobMap = struct
    module M = Map.Make (struct type t = int let compare = Pervasives.compare end)
    let empty = M.empty
    let remove job jobs = M.remove job#node_id jobs
    let add job x jobs = M.add job#node_id x jobs
    let find job jobs = M.find job#node_id jobs
end

class ['self] t = object (self : 'self)
    val distances = JobMap.empty

    method add job =
        (* if the targets change, the distance calculated and stored here may be off, but it saves from
         * having to recalculate at every query *)
        Profiler.global#call "ClosestToTargetsStrategy.add" begin fun () ->
            let source = Job.get_instruction job in
            let call_sites = Instruction.call_sites (Instruction.fundec_of source) in

            (* TODO: maybe memoize the target calculation, if it turns out to be expensive *)
            let target_fundecs = BackOtterTargets.get_target_fundecs () in
            let target_instrs = List.filter begin fun call_site ->
                List.exists (CilData.CilFundec.equal call_site.Instruction.fundec) target_fundecs
            end call_sites in
            let line_targets = LineTargets.get_line_targets job#file @ BackOtterTargetTracker.get_line_targets job#file in
            let all_targets = target_instrs @ line_targets in

            let distance =
                if all_targets = [] then
                    max_int
                else
                    let context = Job.get_instruction_context job in
                    Distance.find_in_context (source, context, all_targets)
            in
            Output.debug_printf "Job %d has distance to target = %d@\n" job#node_id distance;
            {< distances = JobMap.add job distance distances >}
        end

    method remove job =
        {< distances = JobMap.remove job distances >}

    method weight job =
        1. /. float_of_int (JobMap.find job distances)
end

