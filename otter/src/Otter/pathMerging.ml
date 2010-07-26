open Executeargs
open Bytes
open Types

(** Find the common suffix of 2 lists l1 and l2, and return a triple
		(a,b,c) such that l1 = a @ c and l2 = b @ c. *)
let split_out_common_suffix l1 l2 =
	let rec helper a b acc = match a,b with
		| [], _ ->
			[], List.rev b, acc
		| _, [] ->
			List.rev a, [], acc
		| h1::t1, h2::t2 ->
			if h1 == h2
			then helper t1 t2 (h1 :: acc)
			else List.rev a, List.rev b, acc
	in helper (List.rev l1) (List.rev l2) []


let pc_to_bytes = function
	| [] -> failwith "pcToAND"
	| [h] -> asBoolean h
	| pc -> make_Bytes_Op(OP_LAND, List.map (fun b -> (b,Cil.intType)) pc)


let at_same_program_point job1 job2 =
	let state1 = job1.state
	and state2 = job2.state in
	(* I'm trying to be conservative here by using physical equality
	 * because I haven't quite figured out what 'being at the same
	 * program point' means. But I'm pretty sure that if these 3 things
	 * are physically equal, then they really are at the same program
	 * point, whatever that's supposed to mean. *)
	(* Actually, if the instrLists are equal, then the stmts have to be.
	 * Right? So the stmt equality can be moved into the assertion. *)
	if job1.stmt == job2.stmt
			&& job1.instrList == job2.instrList
			&& state1.callContexts == state2.callContexts then begin
		assert (
			job1.exHist.bytesToVars == job2.exHist.bytesToVars &&
			state1.global           == state2.global &&
			state1.formals          == state2.formals &&
			state1.locals           == state2.locals &&
			state1.callstack        == state2.callstack &&
			state1.va_arg           == state2.va_arg &&
			state1.va_arg_map       == state2.va_arg_map
		);
		true
	end else
		false


exception TooDifferent


(* Attempt to merge two jobs, returning the truncated job result and the newly merged job if successful. *)
let try_merge job other =
	if at_same_program_point job other then
		try
			let job_pc, other_pc, common_pc =
				split_out_common_suffix job.state.path_condition other.state.path_condition
			in
			let job_pc_bytes = pc_to_bytes job_pc in
			let other_pc_bytes = pc_to_bytes other_pc in

			let merged_pc = match job_pc, other_pc with
				| [byts],[Bytes_Op(OP_LNOT,[(byts',_)])]
				| [Bytes_Op(OP_LNOT,[(byts',_)])],[byts] when compare byts byts' = 0 ->
					(* Optimize the common case of P \/ ~P. Use compare x y = 0 (instead
							of x = y) to get short-circuiting in case of physical equality.
							(See
http://caml.inria.fr/pub/ml-archives/caml-list/2009/08/323bd4f55773e4a230d481aecce58770.en.html ) *)
					common_pc
				| _ ->
					(* General case: either one condition or the other is
					 * true. We might sometimes be able to simplify the
					 * pc, but right now we don't try to do so. *)
					let combined_pc = make_Bytes_Op(OP_LOR, [ (job_pc_bytes, Cil.intType); (other_pc_bytes, Cil.intType) ]) in
					combined_pc::common_pc
			in

			(* TODO: do we want to have a limit on number of blocks merged? Should benchmark the burden
			 *       on STP using the Morris encoding. *)
			let merged_count = ref 0 in
			let merged_memory = MemoryBlockMap.fold2 begin fun block deferreds merged_memory -> match deferreds with
				| MemoryBlockMap.Both (deferred1, deferred2) ->
					begin match deferred1, deferred2 with
						| deferred1, deferred2 when deferred1 == deferred2 ->
							merged_memory
						| Immediate bytes1, Immediate bytes2 when bytes__equal bytes1 bytes2 ->
							merged_memory
						| deferred1, deferred2 ->
							merged_count := !merged_count + 1;
							if !merged_count > 100 then raise TooDifferent;
							let deferred = Deferred begin fun state ->
								let state, bytes1 = MemOp.state__force state deferred1 in
								let state, bytes2 = MemOp.state__force state deferred2 in
								(* TODO: can bytes1 and bytes2 ever have different length? Aren't they supposed to
								 *       have the size declared in the block? *)
								assert (bytes__length bytes1 == bytes__length bytes2);
								(* only job_pc_bytes, since NOT job_pc_bytes implies other_pc_bytes due to the added
								 * path condition above *)
								let c = IfThenElse (
									guard__bytes job_pc_bytes, conditional__bytes bytes1, conditional__bytes bytes2
								) in
								(state, make_Bytes_Conditional c)
							end in
							MemoryBlockMap.add block deferred merged_memory
					end
				| MemoryBlockMap.Left deferred ->
					merged_memory
				| MemoryBlockMap.Right deferred ->
					MemoryBlockMap.add block deferred merged_memory
			end job.state.block_to_bytes other.state.block_to_bytes job.state.block_to_bytes in

			Output.printf "Merging jobs %d and %d (%s memory) \n"
				job.jid other.jid (if !merged_count = 0 then "identical" else "differing");

			(* This is a pretty stupid way of getting complete coverage information with
			 * merging, but it's something. It only credits a merged execution
			 * with the parts that *both* jobs covered. Things covered by only
			 * one side are attributed to truncated executions that end at the
			 * join point. *)
			let jobOnlyLines = LineSet.diff job.exHist.coveredLines other.exHist.coveredLines
			and otherOnlyLines = LineSet.diff other.exHist.coveredLines job.exHist.coveredLines
			and jobOnlyBlocks = StmtInfoSet.diff job.exHist.coveredBlocks other.exHist.coveredBlocks
			and otherOnlyBlocks = StmtInfoSet.diff other.exHist.coveredBlocks job.exHist.coveredBlocks
			and jobOnlyEdges = EdgeSet.diff job.exHist.coveredEdges other.exHist.coveredEdges
			and otherOnlyEdges = EdgeSet.diff other.exHist.coveredEdges job.exHist.coveredEdges
			and jobOnlyConds = CondSet.diff job.exHist.coveredConds other.exHist.coveredConds
			and otherOnlyConds = CondSet.diff other.exHist.coveredConds job.exHist.coveredConds
			in
			let merged = { job with
				state = { job.state with
					block_to_bytes = merged_memory;
					path_condition = merged_pc
				};
				exHist = { job.exHist with
					coveredLines = LineSet.inter job.exHist.coveredLines other.exHist.coveredLines;
					coveredBlocks = StmtInfoSet.inter job.exHist.coveredBlocks other.exHist.coveredBlocks;
					coveredEdges = EdgeSet.inter job.exHist.coveredEdges other.exHist.coveredEdges;
					coveredConds = CondSet.inter job.exHist.coveredConds other.exHist.coveredConds;
				};
				mergePoints = StmtInfoSet.union job.mergePoints other.mergePoints;
			} in
			let truncated = Complete (Truncated (
				{
					result_file = job.file;
					result_state = job.state;
					result_history = { job.exHist with
						coveredLines = jobOnlyLines;
						coveredBlocks = jobOnlyBlocks;
						coveredEdges = jobOnlyEdges;
						coveredConds = jobOnlyConds;
					}
				},
				{
					result_file = job.file;
					result_state = other.state;
					result_history = { other.exHist with
						coveredLines = otherOnlyLines;
						coveredBlocks = otherOnlyBlocks;
						coveredEdges = otherOnlyEdges;
						coveredConds = otherOnlyConds;
					}
				}
			)) in
			Some (truncated, merged)
		with TooDifferent ->
			Output.print_endline "Memory too different to merge";
			None
	else
		None


(* MergeDone contains the updated merge set and the truncated job result. *)
exception MergeDone of (JobSet.t * job_state)


(* Merge a job with another job in a merge set. If the merge is successful, return the updated merge set and the
 * truncated job result; otherwise, return None. *)
let merge_job job merge_set =
	try
		(* try to merge with some job in the merge_set *)
		JobSet.iter begin fun other ->
			match try_merge job other with
				| Some (truncated, merged) ->
					(* Remove the old job and add the merged job *)
					let merge_set = JobSet.add merged (JobSet.remove other merge_set) in
					raise (MergeDone (merge_set, truncated))
				| None ->
					()
		end merge_set;
		(* Reaching here means we've iterated through all jobs, never being able to merge *)
		None
	with MergeDone x ->
		Some x

let merge jobs job =
  let result = merge_job job jobs.Jobs.merge_set in
    match result with
      | Some (merge_set,truncated) ->
          jobs.Jobs.merge_set <- merge_set;
          (Some truncated)
      | None ->
          Jobs.add_mergable jobs job;
          None

let at_merge_point job =
	StmtInfoSet.mem
		{ siFuncName=(List.hd job.state.callstack).Cil.svar.Cil.vname;
		  siStmt=job.stmt; }
		job.mergePoints

let get_job_priority_queue_with_merge job_queue = 
	if Jobs.has_next_runnable job_queue then
		Some (((Jobs.take_next_runnable job_queue), true), job_queue)
	else if Jobs.has_next_mergable job_queue then
		begin
			(* job queue is empty: take a job out of the merge set and step it, since it cannot merge
			 * with any other jobs in the merge set (the merge set invariant) *)
			let job = Jobs.take_next_mergable job_queue in
			let _ = Jobs.running job_queue job in (* set current job *)
			Some ((job, false), job_queue)
		end
	else
		None

let merge_job_interceptor job job_queue interceptor = 
	let job, mergeable = job in
	if mergeable && at_merge_point job then
		(* job is at a merge point and merging is enabled: try to merge it *)
		begin match merge job_queue job with
			| Some (truncated) ->
				(* merge was successful: process the result and continue *)
				(truncated, job_queue)
			| None ->
				(* merge was unsuccessful: keep the job at the merge point in the merge set in case
				 * later jobs can merge; this leads to the invariant that no jobs in the merge set
				 * can merge with each other *)
				(Paused job, job_queue)
		end
	else
		(* job is not at a merge point: step the job *)
		let _ = Jobs.running job_queue job in (* set current job *)
		interceptor job job_queue

let init job =
	let jobs = Jobs.create [] in
	let _ = Jobs.add_runnable jobs job in
	let (@@) = Interceptors.(@@) in
	Driver.main_loop 
		get_job_priority_queue_with_merge
		(
			merge_job_interceptor @@
			Interceptors.set_output_formatter_interceptor @@
			Builtin_function.interceptor @@
			Core.step
		)
		Driver.process_result_priority_queue
		jobs

