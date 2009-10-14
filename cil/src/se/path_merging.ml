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
	| [h] -> h
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


(* Transform an association list into a memory block map, preserving
 * only the *last* binding for each key.
 * Preserving the *first* instead (which is more natural for
 * association lists) could be accomplished by reversing [lst] in the
 * first call to [helper].
 * I currently don't reverse [lst] because I currently only call this
 * function on values of [lst] which have only one binding per key. *)
let assoc_list_to_memory_block_map lst =
	let rec helper acc = function
			[] -> acc
		| (a,b)::t ->
				helper (MemoryBlockMap.add a b acc) t
	in helper MemoryBlockMap.empty lst


let cmp_memory blkToByt1 blkToByt2 =
	(* Split the blocks in s1 into those which are in s2 but map to
	 * different bytes, those which map to the same bytes in s2, and
	 * those which are not in s2 at all. *)
	let cmpSharedBlocks b2b1 b2b2 =
		let f block bytes1 (diffShared,sameShared,in1ButNot2) =
			try
				let bytes2 = MemoryBlockMap.find block b2b2 in
				if MemOp.diff_bytes bytes1 bytes2 then
					((block,bytes1,bytes2) :: diffShared, sameShared, in1ButNot2)
				else
					(diffShared, (block,bytes1) :: sameShared, in1ButNot2)
			with Not_found -> (diffShared, sameShared, (block,bytes1) :: in1ButNot2)
		in
		MemoryBlockMap.fold f b2b1 ([],[],[])
	in
	let sharedBlockDiffs,sharedBlockSames,blocksIn1ButNot2 =
		cmpSharedBlocks blkToByt1 blkToByt2 in

	(* List blocks (memory allocations) that are in s2 but not in s1 *)
	let cmpUnsharedBlocks b2b1 b2b2 =
		let h b2b block bytes listOfDiffs =
			if MemoryBlockMap.mem block b2b then listOfDiffs else
			(block,bytes) :: listOfDiffs
		in
		MemoryBlockMap.fold (h b2b1) b2b2 []
	in
	let blocksIn2ButNot1 = cmpUnsharedBlocks blkToByt1 blkToByt2 in
	sharedBlockDiffs,sharedBlockSames,blocksIn1ButNot2,blocksIn2ButNot1


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
				| [Bytes_Op(OP_LNOT,[(byts',_)])],[byts] when byts == byts' ->
					(* Optimize the common case of P \/ ~P *)
					common_pc
				| _ ->
					(* General case: either one condition or the other is
					 * true. We might sometimes be able to simplify the
					 * pc, but right now we don't try to do so. *)
					let combined_pc = make_Bytes_Op(OP_LOR, [ (job_pc_bytes, Cil.intType); (other_pc_bytes, Cil.intType) ]) in
					combined_pc::common_pc
			in
			(* I'm assuming [j.state.locals == job.state.locals] for now
				(due to the implementation of atSameProgramPoint),
				so the varinfos always map to the same blocks. This means I
				only need to mess with the [block_to_bytes]---I can leave
				[locals] alone. *)
			let diffShared,sameShared,inJobOnly,inOtherOnly =
				cmp_memory job.state.block_to_bytes other.state.block_to_bytes in
			let merged_memory = if diffShared = [] then begin
				(* The memory is identical in [j] and [job] *)
				if not (inJobOnly = [] && inOtherOnly = []) then
					failwith "there might be a memory leak";
				Output.printf "Merging jobs %d and %d (identical memory)\n" job.jid other.jid;
				job.state.block_to_bytes
			end else begin
				(* We have to fiddle with memory *)
				(* TODO: do we still want to have a limit on number of blocks merged? Should benchmark the burden
				 *       on STP using the MayBytes/Morris encoding. *)
				let numSymbolsCreated = ref 0 in
				let newSharedBlocks =
					(* Make a new symbolic bytes for each differing block *)
					List.map
						(fun (block,jobBytes,otherBytes) ->
							let size = MemOp.bytes__length jobBytes in (* or should I just check block.memory_block_size? *)
							if size <> MemOp.bytes__length otherBytes then (
								Output.printf "Unimplemented: merging bytes with different lengths\n";
								raise TooDifferent
							) else (
								numSymbolsCreated := !numSymbolsCreated + size;
								if !numSymbolsCreated > 100 then raise TooDifferent;
								let symbBytes = make_Bytes_IfThenElse (job_pc_bytes, jobBytes, otherBytes) in
								(block, symbBytes)
							)
						)
						diffShared
				in
				let mergedMemory =
					(* I think it's safe (if not optimal) to keep both
						[inJOnly] and [inJobOnly] because [j]'s memory will be
						unreachable from [job]'s path and vice versa. *)
					assoc_list_to_memory_block_map
						(List.concat [newSharedBlocks; sameShared; inJobOnly; inOtherOnly])
				in
				Output.printf "Merging jobs %d and %d (differing memory)\n" job.jid other.jid;
				mergedMemory
			end in

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


(* MergeDone contains the truncated job result if merging was successful, and the updated merge set. *)
exception MergeDone of (job_state option * JobSet.t)


(* Merge a job with another job in a merge set. If the merge is successful, return the truncated job result and the
 * updated merge set; otherwise, just put the job into the merge set. *)
let merge_job job merge_set =
	try
		(* try to merge with some job in the merge_set *)
		JobSet.iter begin fun other ->
			match try_merge job other with
				| Some (truncated, merged) ->
					(* Remove the old job and add the merged job *)
					let merge_set = JobSet.add merged (JobSet.remove other merge_set) in
					raise (MergeDone (Some truncated, merge_set))
				| None ->
					()
		end merge_set;
		(* Reaching here means we've iterated through all jobs, never being able to merge; so we add it to the
         * merge_set. *)
		(None, JobSet.add job merge_set)
	with MergeDone x ->
		x

