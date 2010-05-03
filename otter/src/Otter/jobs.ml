open Cil
open Types

type t =
    {
      job_queue: job list;
      merge_set: JobSet.t;
    }
;;

let empty = 
  {
    job_queue = [];
    merge_set = JobSet.empty;
  }
;;

let has_next_runnable jobs =
   List.length jobs.job_queue > 0
;;

let has_next_mergable jobs =
  not (JobSet.is_empty jobs.merge_set)
;;

let add_runnable jobs job =
  {jobs with
       job_queue = job::(jobs.job_queue);
  }
;;

let add_runnables jobs joblist =
  List.fold_left add_runnable jobs joblist
;;

let add_mergable jobs job =
   {jobs with merge_set=JobSet.add job jobs.merge_set;}
;;

let take_next_runnable jobs =
  match jobs.job_queue with
    | [] -> failwith "Jobs: no more runnable jobs"
    | j::js -> j,{jobs with job_queue=js;}
;;

let take_next_mergable jobs =
  try
    let j = JobSet.choose jobs.merge_set in
    let js = JobSet.remove j jobs.merge_set in
      j,{jobs with merge_set=js;}
  with e -> failwith "Jobs: no more mergable jobs"
;;

let merge jobs job =
  let result = PathMerging.merge_job job jobs.merge_set in
    match result with
      | Some (merge_set,truncated) ->
          (Some truncated),{jobs with merge_set=merge_set;}
      | None ->
          None, add_mergable jobs job
;;


(*
let bias guide j1 j2 =
  let position j = 
    if j.instrList = [] then 
      To_string.stmt j.stmt
    else
      To_string.instr (List.hd j.instrList)
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
