open Types
open Executeargs

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
			state1.va_arg_map       == state2.va_arg_map &&
			state1.loc_map          == state2.loc_map
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
			 *       on STP using the MayBytes/Morris encoding. *)
			let merged_count = ref 0 in
			let merged_memory = MemoryBlockMap.map2 begin function
				| MemoryBlockMap.Both (deferred1, deferred2) ->
					begin match deferred1, deferred2 with
						| deferred1, deferred2 when deferred1 == deferred2 ->
							deferred1
						| Immediate bytes1, Immediate bytes2 when MemOp.same_bytes bytes1 bytes2 ->
							deferred1
						| deferred1, deferred2 ->
							merged_count := !merged_count + 1;
							if !merged_count > 100 then raise TooDifferent;
							Deferred begin fun state ->
								let state, bytes1 = MemOp.state__force state deferred1 in
								let state, bytes2 = MemOp.state__force state deferred2 in
								(* TODO: can bytes1 and bytes2 ever have different length? Aren't they supposed to
								 *       have the size declared in the block? *)
								assert (MemOp.bytes__length bytes1 == MemOp.bytes__length bytes2);
								(* only job_pc_bytes, since NOT job_pc_bytes implies other_pc_bytes due to the added
								 * path condition above *)
								(state, make_Bytes_IfThenElse (job_pc_bytes, bytes1, bytes2))
							end
					end
				| MemoryBlockMap.Left deferred
				| MemoryBlockMap.Right deferred ->
					deferred
			end job.state.block_to_bytes other.state.block_to_bytes in

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
					result_state = job.state;
					result_history = { job.exHist with
						coveredLines = jobOnlyLines;
						coveredBlocks = jobOnlyBlocks;
						coveredEdges = jobOnlyEdges;
						coveredConds = jobOnlyConds;
					}
				},
				{
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

