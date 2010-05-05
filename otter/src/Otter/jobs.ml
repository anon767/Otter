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
let the_prioritize = Prioritizer.prioritize ;;

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
    }
;;

let create file = 
  {
    current_job = None;
    job_queue = PriorityQueue.make (fun j1 j2 -> j1.priority >= j2.priority);
    merge_set = JobSet.empty;
    prioritize = the_prioritize;
  }
;;

let has_next_runnable jobs =
   PriorityQueue.length jobs.job_queue > 0
;;

let has_next_mergable jobs =
  not (JobSet.is_empty jobs.merge_set)
;;

let add_runnable jobs job =
  PriorityQueue.add jobs.job_queue { obj=job; priority=jobs.prioritize job }
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


(*
let bias guide j1 j2 =
  let position j = 
    if j.instrPriorityQueue = [] then 
      To_string.stmt j.stmt
    else
      To_string.instr (PriorityQueue.hd j.instrPriorityQueue)
  in
  let rec get_choice () =
    Output.printf "Enter choice (1/2):\n%!";
    try
      Scanf.scanf "%d\n" (fun i->i)
    with e -> get_choice ()
  in
    Output.printf "Position of j1: %s\n%!" (position j1);
    Output.printf "Position of j2: %s\n%!" (position j2);
  let choice = get_choice () in
    if choice = 1 then 
      j1,j2
    else
      j2,j1
;;
 *)
