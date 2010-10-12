open DataStructures
open OcamlUtilities
open OtterCore
open OtterBytes
open OtterJob
open MultiTypes
open Types
open Job
open Bytes
open MultiInterceptor

let libc_fork job multijob retopt exps =
	(* update instruction pointer, history, and such *)
	let job = BuiltinFunctions.end_function_call job in

	Output.set_mode Output.MSG_REG;
	Output.printf "fork(): parent: %d, child: %d@\n" multijob.current_pid multijob.next_pid;

	(* clone the job *)
	let job, child_job = match retopt with
		| None ->
			(job, job)
		| Some cil_lval ->

			let child_job = { job with
				(* TODO: make the pid symbolic *)
				state =
					let state, lval, _ = Expression.lval job.state cil_lval Types.Channel in
					MemOp.state__assign state lval (Bytes.int_to_bytes multijob.next_pid)
			} in
			let job = { job with
				state =
					let state, lval, _ = Expression.lval job.state cil_lval Types.Channel in
					MemOp.state__assign state lval (Bytes.bytes__zero)
			} in
			(job, child_job)
	in
	let multijob = (MultiJobUtilities.put_job child_job multijob multijob.next_pid) in
	let multijob = {multijob with next_pid = multijob.next_pid + 1 } in
	(Active job, multijob)

(* allocates on the global heap *)
let otter_gmalloc_size (state:Types.state) size bytes loc =
	let name = FormatPlus.sprintf "%s(%d)#%d/%a%s"
		(List.hd state.Types.callstack).Cil.svar.Cil.vname
		size
		(DataStructures.Counter.next BuiltinFunctions.libc___builtin_alloca__id)
		Printcil.loc loc
		(MemOp.state__trace state)
	in
	let block = Bytes.block__make name size Bytes.Block_type_Heap in
	let addrof_block = Bytes.make_Bytes_Address (block, Bytes.bytes__zero) in
	let state = MemOp.state__add_block state block bytes in
	(state, block, addrof_block)

let otter_gmalloc job multijob retopt exps =
	let state, b_size, _ = Expression.rval job.Job.state (List.hd exps) Types.Channel in
	let size =
		if Bytes.isConcrete_bytes b_size then
			Bytes.bytes_to_int_auto b_size (*safe to use bytes_to_int as arg should be small *)
		else
			1 (* currently bytearray have unbounded length *)
	in
	let bytes =
	  if !Executeargs.arg_init_malloc_zero
	  then Bytes.bytes__make size (* initially zero, as though malloc were calloc *)
	  else Bytes.bytes__make_default size Bytes.byte__undef (* initially the symbolic 'undef' byte *)
	in
	let state, block, bytes = otter_gmalloc_size state size bytes (Job.get_loc job) in
    let state', _ = BuiltinFunctions.set_return_value state retopt bytes Types.Channel in
	let job = BuiltinFunctions.end_function_call { job with state = state' } in

	let multijob =
		{multijob with
			shared =
				{multijob.shared with
					shared_block_to_bytes = MemoryBlockMap.add block (Deferred.Immediate bytes) multijob.shared.shared_block_to_bytes;
				};
			processes = List.map
				(fun (pc, ls) ->
					(pc, { ls with MultiTypes.block_to_bytes = MemoryBlockMap.add block (Deferred.Immediate bytes) ls.MultiTypes.block_to_bytes; })
				)
				multijob.processes;
		}
	in

	(Active job, multijob)

let otter_gfree job multijob retopt exps =
	let state, ptr, _ = Expression.rval job.state (List.hd exps) Types.Channel in
	match ptr with
		| Bytes.Bytes_Address (block, _) ->
			if block.Bytes.memory_block_type != Bytes.Block_type_Heap then
				FormatPlus.failwith "gfreeing a non-gmalloced pointer:@ @[%a@]@ = @[%a@]@\n" Printer.exp (List.hd exps) BytesPrinter.bytes ptr
			else if not (MemoryBlockMap.mem block multijob.shared.shared_block_to_bytes) then
				FormatPlus.failwith "gfreeing a non-gmalloced pointer or double-gfree:@ @[%a@]@ = @[%a@]@\n" Printer.exp (List.hd exps) BytesPrinter.bytes ptr
			else if not (MemoryBlockMap.mem block state.Types.block_to_bytes) then
				FormatPlus.failwith "gfreeing after free:@ @[%a@]@ = @[%a@]@\n" Printer.exp (List.hd exps) BytesPrinter.bytes ptr
			else
				let multijob =
					{multijob with
						shared =
							{multijob.shared with
								shared_block_to_bytes = MemoryBlockMap.remove block multijob.shared.shared_block_to_bytes;
							};
						processes = List.map
							(fun (pc, ls) ->
								(pc, { ls with MultiTypes.block_to_bytes = MemoryBlockMap.remove block ls.MultiTypes.block_to_bytes; })
							)
							multijob.processes;
					}
				in
				(Active job, multijob)

		| _ ->
			Output.set_mode Output.MSG_MUSTPRINT;
			FormatPlus.failwith "gfreeing something that is not a valid pointer:@ @[%a@]@ = @[%a@]@\n" Printer.exp (List.hd exps) BytesPrinter.bytes ptr

let interceptor job multijob job_queue interceptor =
	try
		(

		(intercept_multi_function_by_name_internal "fork"                       libc_fork) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_gmalloc"      otter_gmalloc) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_gfree"        otter_gfree) @@@

		(* pass on the job when none of those match *)
		interceptor

		) job multijob job_queue
	with Failure msg ->
		if !Executeargs.arg_failfast then failwith msg;
		let result = {
			result_file = job.file;
			result_state = job.state;
			result_history = job.exHist;
			result_decision_path = job.decisionPath; } in
		(Complete (Abandoned (`Failure msg, Job.get_loc job, result)), (multijob, job_queue))
