open DataStructures
open OcamlUtilities
open Cil
open OtterCore
open OtterBytes
open MultiTypes
open Types
open Job
open Bytes
open MultiInterceptor

let libc_fork job multijob retopt exps errors =
	(* update instruction pointer, history, and such *)
	let job = BuiltinFunctions.end_function_call job in

	Output.set_mode Output.MSG_REG;
	Output.printf "fork(): parent: %d, child: %d@\n" multijob.current_pid multijob.next_pid;

	(* clone the job *)
	let job, child_job, errors = match retopt with
		| None ->
			(job, job, errors)
		| Some cil_lval ->
			(* TODO: make the pid symbolic *)
			let child_state, child_lval, errors = Expression.lval job.state cil_lval errors in
			let child_job = { job with state = MemOp.state__assign child_state child_lval (Bytes.int_to_bytes multijob.next_pid); } in

			let state, lval, errors = Expression.lval job.state cil_lval errors in
			let job = { job with state = MemOp.state__assign state lval (Bytes.bytes__zero); } in
			(job, child_job, errors)
	in
	let multijob = (MultiJobUtilities.put_job child_job multijob multijob.next_pid) in
	let multijob = {multijob with next_pid = multijob.next_pid + 1 } in
	(Active job, multijob, errors)

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

let otter_gmalloc job multijob retopt exps errors =
	let state, b_size, errors = Expression.rval job.Job.state (List.hd exps) errors in
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
	let state, errors = BuiltinFunctions.set_return_value state retopt bytes errors in
	let job = BuiltinFunctions.end_function_call { job with state = state } in

	let multijob =
		{multijob with
			shared =
				{multijob.shared with
					shared_block_to_bytes = MemoryBlockMap.add block (Deferred.Immediate bytes) multijob.shared.shared_block_to_bytes;
				};
			processes = List.map
				(fun (pc, ls, pi) ->
					(pc, { ls with MultiTypes.block_to_bytes = MemoryBlockMap.add block (Deferred.Immediate bytes) ls.MultiTypes.block_to_bytes; }, pi)
				)
				multijob.processes;
		}
	in

	(Active job, multijob, errors)

let otter_gfree job multijob retopt exps errors =
	let state, ptr, errors = Expression.rval job.state (List.hd exps) errors in
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
							(fun (pc, ls, pi) ->
								(pc, { ls with MultiTypes.block_to_bytes = MemoryBlockMap.remove block ls.MultiTypes.block_to_bytes; }, pi)
							)
							multijob.processes;
					}
				in
				(Active job, multijob, errors)

		| _ ->
			Output.set_mode Output.MSG_MUSTPRINT;
			FormatPlus.failwith "gfreeing something that is not a valid pointer:@ @[%a@]@ = @[%a@]@\n" Printer.exp (List.hd exps) BytesPrinter.bytes ptr
			
let otter_get_pid job multijob retopt exps errors =
	let state, errors = BuiltinFunctions.set_return_value job.state retopt (int_to_bytes multijob.current_pid) errors in
	let job = BuiltinFunctions.end_function_call { job with state = state } in
	(Active job, multijob, errors)

(* takes a vardic list of pointers to blocks to watch *)
let otter_io_block job multijob retopt exps errors = 
	let rec find_blocks state exps errors =
		match exps with
			| [] -> ([], state, errors)
			| (Lval cil_lval)::t
			| (CastE (_, Lval cil_lval))::t
			| (AddrOf (_, NoOffset as cil_lval))::t
			| (CastE (_, AddrOf (_, NoOffset as cil_lval)))::t ->
				let state, bytes, errors = Expression.rval state (Lval cil_lval) errors in
				let state, lvals, errors = Expression.deref state bytes (Cil.typeOfLval cil_lval) errors in
				let blocks = conditional__fold
					~test:(Stp.query_stp state.path_condition)
					(fun acc guard (x, y) -> x::acc)
					[]
					lvals
				in
				let blocks2, state, errors = find_blocks state t errors in
				((List.rev_append blocks blocks2), state, errors)
			| _ -> failwith "io_block invalid arguments"
	in
	
	let blocks, state, errors = find_blocks job.state exps errors in
	
	let multijob =
		if blocks = [] then
			failwith "io_block with no underlying blocks"
		else
			let block_to_bytes = 
				try
					List.fold_left
						(fun acc block -> MemoryBlockMap.add block (MemoryBlockMap.find block state.block_to_bytes) acc)
						MemoryBlockMap.empty
						blocks
				with
					| Not_found -> failwith "io_block missing underlying block"
			in
			{ multijob with priority = IOBlock block_to_bytes; }
	in
	
	let state, errors = BuiltinFunctions.set_return_value state retopt (Bytes.bytes__zero) errors in
	let job = BuiltinFunctions.end_function_call { job with state = state } in
	(Active job, multijob, errors)

let otter_time_wait job multijob retopt exps errors = 
	match exps with
		| [CastE (_, h)] | [h] ->
			let state, bytes, errors = Expression.rval job.state h errors in
			let time = bytes_to_int_auto bytes in
			let job = BuiltinFunctions.end_function_call { job with state = state } in
			if time <= 0 then 
				(Active job, multijob, errors) 
			else
				(Active job, { multijob with priority = TimeWait time; }, errors)
		| _ -> failwith "timewait invalid arguments"

let otter_begin_atomic job multijob retopt exps errors = 
	let job = BuiltinFunctions.end_function_call job in
	(Active job, { multijob with priority = Atomic; }, errors)

let otter_end_atomic job multijob retopt exps errors = 
	let job = BuiltinFunctions.end_function_call job in
	(Active job, { multijob with priority = Running; }, errors)

let interceptor job multijob job_queue interceptor =
	try
		(

		(intercept_multi_function_by_name_internal "__otter_multi_fork"         libc_fork) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_gmalloc"      otter_gmalloc) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_gfree"        otter_gfree) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_get_pid"      otter_get_pid) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_io_block"     otter_io_block) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_time_wait"    otter_time_wait) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_begin_atomic" otter_begin_atomic) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_end_atomic"   otter_end_atomic) @@@

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
