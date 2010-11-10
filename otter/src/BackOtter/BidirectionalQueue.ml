open BackOtterUtilities
open OcamlUtilities
open OtterCore
open Types
open Job
open Decision
open Cil

let default_bidirectional_search_ratio = ref 0.5


(* Given the failing paths for each target, construct bounding paths for the job.
 * Very inefficient, but working
 *
 * Returns (job, job with bounding paths, smallest depth of the first feasible attachment)
 * *)
let update_bounding_paths targets job =
    let update_bounding_paths () =
        let is_suffix eq suf lst =
            let rec is_prefix eq pre lst =
                match pre, lst with
                | d1::pre', d2::lst' -> if eq d1 d2 then is_prefix eq pre' lst' else -1
                | [], [] -> 0
                | [], _ -> 1
                | _, _ -> -1
            in
            is_prefix eq (List.rev suf) (List.rev lst)
        in
        let rec get_bounding_paths decision_path = match decision_path with
            | DecisionFuncall (_, fundec) :: decision_tail ->
                let failing_paths = BackOtterTargets.get fundec targets in
                let stitched_failing_paths = List.map (fun p -> List.append p decision_path) failing_paths in
                (* Take out paths from bounding_paths where job.decisionPath is not a suffix of them *)
                let stitched_failing_paths = List.filter (fun p -> is_suffix Decision.equals job.decisionPath p >= 0) stitched_failing_paths in
                let upper_bounding_paths, depth = get_bounding_paths decision_tail in
                let depth = if upper_bounding_paths = [] then (List.length decision_path) else depth in
                List.fold_left (fun paths path ->
                    if List.exists (fun p -> is_suffix Decision.equals p path = 0) paths then paths else path :: paths
                ) upper_bounding_paths stitched_failing_paths, depth
            | _ :: decision_tail -> get_bounding_paths decision_tail
            | [] -> [], max_int
        in
        let bounding_paths, depth = get_bounding_paths job.decisionPath in
        job, {job with boundingPaths = Some bounding_paths;}, depth (* TODO: make boundingPaths not option *)
    in
    Stats.time "BidirectionalQueue.update_bounding_paths" update_bounding_paths ()


let has_bounding_paths (_,job,_) = match job.boundingPaths with
    | Some (_ :: _) -> true
    | _ -> false


type job_type = EntryfnJob of job | OtherfnJob of job


(* TODO: package the long list of arguments into BackOtterProfile.t *)
class ['job] t ?(ratio=(!default_bidirectional_search_ratio))
               file
               targets_ref
               timer_ref
               entry_fn
               failure_fn
               entry_job
               f_queue
               b_queue = object (self)

    (* ratio < 0.0 denotes purely backward (beware of precision!) *)
    val is_bidirectional = ratio >= 0.0
    val otherfn_jobqueue = new RemovableQueue.t (b_queue)
    val entryfn_jobqueue =
        let queue = new RemovableQueue.t (f_queue) in
        if (ratio >= 0.0) then queue#put entry_job else queue
    val origin_fundecs = [] (* fundecs whose initialized jobs have been created *)


    method put (job : 'job) =
        if is_bidirectional && get_origin_function job == entry_fn then
            {< entryfn_jobqueue = entryfn_jobqueue#put job >}
        else
            {< otherfn_jobqueue = otherfn_jobqueue#put job >}

    method get =
        let get () = try
            (* Clear the label, as anything printed here has no specific job context *)
            Output.set_formatter (new Output.plain);

            (* If there's no more entry jobs, the forward search has ended. So we terminate. *)
            if is_bidirectional && entryfn_jobqueue#get_contents = [] then None else

            (* Set up targets, which maps target functions to their failing paths, and
             * target_fundecs, which is basically the key set of targets *)
            let targets = !targets_ref in
            let target_fundecs = BackOtterTargets.get_fundecs targets in

            (* Create new jobs for callers of new targets/failure_fn *)
            let origin_fundecs', otherfn_jobqueue =
                let impl () =
                List.fold_left (
                    fun (origin_fundecs, otherfn_jobqueue) target_fundec ->
                        let callers = CilUtilities.CilCallgraph.find_callers file target_fundec in
                        List.fold_left (
                            fun (origin_fundecs, otherfn_jobqueue) caller ->
                                if (is_bidirectional && caller == entry_fn) || List.memq caller origin_fundecs then
                                    origin_fundecs, otherfn_jobqueue
                                else
                                    let job =
                                        if caller == entry_fn then entry_job
                                        else (
                                            Output.debug_printf "Create new job for function %s@\n" caller.svar.vname;
                                            OtterJob.FunctionJob.make file caller)
                                    in
                                    caller :: origin_fundecs, otherfn_jobqueue#put job
                        ) (origin_fundecs, otherfn_jobqueue) callers
                ) (origin_fundecs, otherfn_jobqueue) (failure_fn :: target_fundecs)
                in Stats.time "BidirectionalQueue.t#get/create_new_jobs" impl ()
            in

            (* debug, Debug, DEBUG *)
            Output.debug_printf "Number of entry function jobs: %d@\n" (List.length (entryfn_jobqueue#get_contents));
            Output.debug_printf "Number of other function jobs: %d@\n" (List.length (otherfn_jobqueue#get_contents));
            List.iter (fun f -> Output.debug_printf "Target function: %s@\n" f.svar.vname) (BackOtterTargets.get_fundecs targets);
            List.iter (fun f -> Output.debug_printf "Origin function: %s@\n" f.svar.vname) (origin_fundecs');

            (* Return a pair, where the first element is the job in jobs that can have bounding paths,
             * and the second element is the job with bounding paths added.
             * The first element is used as the key to remove it from jobs.
             *)
            let find_job_with_bounding_paths jobs =
                let job_assocs = List.map (update_bounding_paths targets) jobs in
                let job_assocs = List.sort (fun (_,_,d1) (_,_,d2) -> d1 - d2) job_assocs in (* shallower depth first *)
                let (job, job_with_paths, depth) = List.find has_bounding_paths job_assocs in
                Output.debug_printf "Job %d is run under influence of bounding path(s) from function %s, with depth %d:@\n"
                    job_with_paths.jid
                    (get_origin_function job_with_paths).svar.vname
                    depth;
                begin match job_with_paths.boundingPaths with
                    | None -> ()
                    | Some bounding_paths -> List.iter (fun path -> Output.debug_printf "Path: @[%a@]@\n" Decision.print_decisions path) bounding_paths
                end;
                job, job_with_paths
            in
            (* Determine whether to run entry function jobs or other function jobs *)
            let want_process_entryfn =
                let entryfn_time_elapsed, otherfn_time_elapsed = !timer_ref in
                let total_elapsed = entryfn_time_elapsed +. otherfn_time_elapsed in
                if total_elapsed <= 0.0001 (* epsilon *) then ratio > 0.0
                else entryfn_time_elapsed /. total_elapsed <= ratio
            in
            if entryfn_jobqueue#get_contents <> [] && (otherfn_jobqueue#get_contents = [] || want_process_entryfn)  then
                (* Do forward search *)
                try
                    let job, job_with_paths = find_job_with_bounding_paths entryfn_jobqueue#get_contents in
                    let entryfn_jobqueue = entryfn_jobqueue#remove job in
                    Some ({< entryfn_jobqueue = entryfn_jobqueue;
                             otherfn_jobqueue = otherfn_jobqueue;  (* Jobs might have been added into ths queue *)
                             origin_fundecs = origin_fundecs'; >}, job_with_paths)
                with Not_found ->
                    match entryfn_jobqueue#get with
                    | Some (entryfn_jobqueue, job) ->
                        Some ({< entryfn_jobqueue = entryfn_jobqueue;
                                 otherfn_jobqueue = otherfn_jobqueue;  (* Same as above *)
                                 origin_fundecs = origin_fundecs'; >}, job)
                    | None -> failwith "This is unreachable"
            else if otherfn_jobqueue#get_contents <> [] then
                (* Do "backward" search *)
                let get_distance_from_entryfn = get_distance_from file entry_fn in
                try
                    let otherfn_jobs = List.sort (fun j1 j2 ->
                        (get_distance_from_entryfn (get_origin_function j1)) -
                        (get_distance_from_entryfn (get_origin_function j2))) otherfn_jobqueue#get_contents
                    in
                    let job, job_with_paths = find_job_with_bounding_paths otherfn_jobs in
                    let otherfn_jobqueue = otherfn_jobqueue#remove job in
                    Some ({< otherfn_jobqueue = otherfn_jobqueue;
                             origin_fundecs = origin_fundecs'; >}, job_with_paths)
                with Not_found ->
                    match otherfn_jobqueue#get with
                    | Some (otherfn_jobqueue, job) ->
                        Some ({< otherfn_jobqueue = otherfn_jobqueue;
                                 origin_fundecs = origin_fundecs'; >}, job)
                    | None -> failwith "This is unreachable"

                else
                    None
        with Types.SignalException s ->
            (* if we got a signal, return None to indicate "no more job" *)
            Output.set_mode Output.MSG_MUSTPRINT;
            Output.printf "BidirectionalQueue.get: %s@\n" s;
            None
    in
    Stats.time "BidirectionalQueue.t#get" get ()

end

(** {1 Command-line options} *)

let options = [
    "--bidirectional-search-ratio",
        Arg.Set_float default_bidirectional_search_ratio,
        "<ratio> The fraction of computation dedicated to forward search (default: 0.5)";
]

