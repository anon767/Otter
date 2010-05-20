(**
  * Job manager
  * Uses a priority queue.
  * Prioritization from module Prioritizer
  *)
open Cil
open Types

(**
  * Change this function to use a different prioritizer 
  *)
let prioritize_wrt_targets = Prioritizer.prioritize ;;

type 'a prioritized = {
  obj: 'a;
  mutable priority: float;
}
type t =
    {
      mutable current_job: job option;
      mutable job_queue: job prioritized PriorityQueue.t;
      mutable merge_set: JobSet.t;
      prioritize: job -> float;
      targets: target list;
    }
;;

let create targets = 
  {
    current_job = None;
    job_queue = PriorityQueue.make (fun j1 j2 -> j1.priority >= j2.priority);
    merge_set = JobSet.empty;
    prioritize = prioritize_wrt_targets targets;
    targets = targets;
  }
;;

let has_next_runnable jobs =
   PriorityQueue.length jobs.job_queue > 0
;;

let has_next_mergable jobs =
  not (JobSet.is_empty jobs.merge_set)
;;

let add_runnable jobs job =
  let priority = jobs.prioritize job in
  let _ = if priority < -.(float_of_int (max_int - 1)) then
    Output.printf "Warning: job %d not continued\n" job.jid
  else ()
  in
    (* Output.printf "Add Job %d with priority %0.1f\n%!" job.jid priority; *)
    PriorityQueue.add jobs.job_queue { obj=job; priority=priority }
;;

let add_runnables jobs joblist =
  List.iter (fun job -> add_runnable jobs job) joblist
;;

let add_mergable jobs job =
   jobs.merge_set <- JobSet.add job jobs.merge_set
;;

let take_next_runnable jobs =
  try 
    let first = PriorityQueue.first jobs.job_queue in
      PriorityQueue.remove_first jobs.job_queue;
      (* Output.printf "Take Job %d with priority %0.1f\n%!" first.obj.jid first.priority; *)
      first.obj
  with _ -> failwith "Jobs: no more runnable jobs"
;;

let take_next_mergable jobs =
  try
    let j = JobSet.choose jobs.merge_set in
    let js = JobSet.remove j jobs.merge_set in
      jobs.merge_set <- js;
      j
  with e -> failwith "Jobs: no more mergable jobs"
;;

let merge jobs job =
  let result = PathMerging.merge_job job jobs.merge_set in
    match result with
      | Some (merge_set,truncated) ->
          jobs.merge_set <- merge_set;
          (Some truncated)
      | None ->
          add_mergable jobs job;
          None
;;

let running jobs job =
  jobs.current_job <- Some job
;;


