open DataStructures
open OcamlUtilities
open CilUtilities
open Cil
open OtterCore
open OtterBytes
open MultiTypes
open State
open Job
open Bytes
open MultiInterceptor

let libc_fork job multijob retopt exps errors =
	(* update instruction pointer, history, and such *)
	let job = BuiltinFunctions.end_function_call job in

	Output.set_mode Output.MSG_REG;
	Output.printf "fork(): parent: %d, child: %d@\n" multijob.current_metadata.pid multijob.next_pid;

	(* clone the job *)
	let job, child_job, errors = match retopt with
		| None ->
			(job, job, errors)
		| Some cil_lval ->
			(* TODO: make the pid symbolic *)
			let child_job, child_lval, errors = Expression.lval job cil_lval errors in
			let child_job = MemOp.state__assign child_job child_lval (Bytes.int_to_bytes multijob.next_pid) in

			let job, lval, errors = Expression.lval job cil_lval errors in
			let job = MemOp.state__assign job lval (Bytes.bytes__zero) in
			(job, child_job, errors)
	in
	let multijob = MultiJobUtilities.put_job
		child_job
		multijob 
		{multijob.current_metadata with 
			pid = multijob.next_pid;
			parent_pid = multijob.current_metadata.pid;
		}
	in
	let multijob = { multijob with next_pid = multijob.next_pid + 1 } in
	(Active job, multijob, errors)

(* allocates on the global heap *)
let otter_gmalloc_size job size bytes loc =
	let name = FormatPlus.sprintf "%s(%d)#%d/%a%s"
		(List.hd job#state.State.callstack).Cil.svar.Cil.vname
		size
		(DataStructures.Counter.next BuiltinFunctions.libc___builtin_alloca__id)
		Printcil.loc loc
		(MemOp.state__trace job)
	in
	let block = Bytes.block__make name size Bytes.Block_type_Heap in
	let addrof_block = Bytes.make_Bytes_Address (block, Bytes.bytes__zero) in
	let job = MemOp.state__add_block job block bytes in
	(job, block, addrof_block)

let otter_gmalloc job multijob retopt exps errors =
	let job, b_size, errors = Expression.rval job (List.hd exps) errors in
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
	let job, block, bytes = otter_gmalloc_size job size bytes (Job.get_loc job) in
	let job, errors = BuiltinFunctions.set_return_value job retopt bytes errors in
	let job = BuiltinFunctions.end_function_call job in

	let multijob =
		{multijob with
			shared =
				{
					shared_block_to_bytes = MemoryBlockMap.add block (Deferred.Immediate bytes) multijob.shared.shared_block_to_bytes;
				};
			processes = List.map
				(fun (pc, ls, md) ->
					(pc, { ls with block_to_bytes = MemoryBlockMap.add block (Deferred.Immediate bytes) ls.block_to_bytes; }, md)
				)
				multijob.processes;
		}
	in

	(Active job, multijob, errors)

let otter_gfree job multijob retopt exps errors =
    let job, ptr, errors = Expression.rval job (List.hd exps) errors in
    match ptr with
      | Bytes.Bytes_Address (block, _) ->
            if block.Bytes.memory_block_type != Bytes.Block_type_Heap then
                FormatPlus.failwith "gfreeing a non-gmalloced pointer:@ @[%a@]@ = @[%a@]@\n" CilPrinter.exp (List.hd exps) BytesPrinter.bytes ptr
            else if not (MemoryBlockMap.mem block multijob.shared.shared_block_to_bytes) then
                FormatPlus.failwith "gfreeing a non-gmalloced pointer or double-gfree:@ @[%a@]@ = @[%a@]@\n" CilPrinter.exp (List.hd exps) BytesPrinter.bytes ptr
            else if not (MemoryBlockMap.mem block job#state.State.block_to_bytes) then
                FormatPlus.failwith "gfreeing after free:@ @[%a@]@ = @[%a@]@\n" CilPrinter.exp (List.hd exps) BytesPrinter.bytes ptr
            else
                let multijob =
                    {multijob with
                        shared =
                            {
                                shared_block_to_bytes = MemoryBlockMap.remove block multijob.shared.shared_block_to_bytes;
                            };
                        processes = List.map
                            (fun (pc, ls, md) ->
                                (pc, { ls with block_to_bytes = MemoryBlockMap.remove block ls.block_to_bytes; }, md)
                            )
                            multijob.processes;
                    }
                in
                let job = MemOp.state__remove_block job block in
                let job, errors = BuiltinFunctions.set_return_value job retopt bytes__zero errors in
                let job = BuiltinFunctions.end_function_call job in
                (Active job, multijob, errors)

      | _ ->
            if not (bytes__equal ptr bytes__zero) (* Freeing a null pointer is fine; it does nothing. *)
            then (
                Output.set_mode Output.MSG_MUSTPRINT;
                FormatPlus.failwith "gfreeing something that is not a valid pointer:@ @[%a@]@ = @[%a@]@\n" CilPrinter.exp (List.hd exps) BytesPrinter.bytes ptr
            );
            let job = BuiltinFunctions.end_function_call job in
            (Active job, multijob, errors)

let otter_get_pid job multijob retopt exps errors =
	let job, errors = BuiltinFunctions.set_return_value job retopt (int_to_bytes multijob.current_metadata.pid) errors in
	let job = BuiltinFunctions.end_function_call job in
	(Active job, multijob, errors)

let otter_get_parent_pid job multijob retopt exps errors =
	match exps with
		| [CastE (_, h)] | [h] ->
			let job, bytes, errors = Expression.rval job h errors in
			let pid = bytes_to_int_auto bytes in
			let ppid =
				if pid = multijob.current_metadata.pid then
					multijob.current_metadata.parent_pid
				else
					List.fold_left
						(fun acc (pc, ls, md) ->
							if md.pid = pid then
								md.parent_pid
							else
								acc
						)
						(-1)
						multijob.processes
			in
			let job, errors = BuiltinFunctions.set_return_value job retopt (int_to_bytes ppid) errors in
			let job = BuiltinFunctions.end_function_call job in
			(Active job, multijob, errors)
		| _ -> failwith "get_parent_id invalid arguments"	

let otter_set_parent_pid job multijob retopt exps errors =
	match exps with
		| [CastE (_, h)] | [h] ->
			let job, bytes, errors = Expression.rval job h errors in
			let pid = bytes_to_int_auto bytes in
			let job = BuiltinFunctions.end_function_call job in
			let multijob = 
				{multijob with
					current_metadata = { multijob.current_metadata with parent_pid = pid; };
				}
			in
			(Active job, multijob, errors)
		| _ -> failwith "set_parent_id invalid arguments"

(* TODO: make this take an array of pointers as an argument *)
(* takes a variadic list of pointers to blocks to watch *)
let otter_io_block job multijob retopt exps errors = 
    let find_blocks job exp errors =
        let job, bytes, errors = Expression.rval job exp errors in
        let job, lvals, errors = Expression.deref job bytes Cil.voidPtrType errors in
        let blocks = conditional__fold
            (fun acc guard (block, _) ->
                 (* TODO: Failing in this case might be overkill. Printing a warning might be good enough *)
                 if not (MemoryBlockMap.mem block multijob.shared.shared_block_to_bytes)
                 then FormatPlus.failwith "Trying to block on non-shared memory: %a" BytesPrinter.memory_block block
                 else block::acc)
            []
            lvals
        in
        (blocks, job, errors)
    in

    let blocks, job, errors =
        List.fold_left
            (fun (blocks, job, errors) ptr ->
                 let new_blocks, job, new_errors = find_blocks job ptr errors in
                 (List.rev_append new_blocks blocks, job, List.rev_append new_errors errors))
            ([], job, errors)
            exps
    in

	let multijob =
		if blocks = [] then
			failwith "io_block with no underlying blocks"
		else
			let block_to_bytes = 
				List.fold_left
					(fun acc block -> 
						try
							MemoryBlockMap.add block (MemoryBlockMap.find block multijob.shared.shared_block_to_bytes) acc
						with
							| Not_found -> 
								if (MemoryBlockMap.mem block job#state.block_to_bytes) then
									acc (* don't add non shared blocks as these will always trigger unblocking *)
								else
									failwith "io_block missing underlying block"
					)
					MemoryBlockMap.empty
					blocks
			in
			if MemoryBlockMap.is_empty block_to_bytes then
				failwith "Deadlock" (* not blocking on any shared bytes *)
			else
				{multijob with
					current_metadata = { multijob.current_metadata with priority = IOBlock block_to_bytes; };
				}
	in
	
	let job, errors = BuiltinFunctions.set_return_value job retopt (Bytes.bytes__zero) errors in
	let job = BuiltinFunctions.end_function_call job in
	(Active job, multijob, errors)

let otter_time_wait job multijob retopt exps errors = 
	match exps with
		| [CastE (_, h)] | [h] ->
			let job, bytes, errors = Expression.rval job h errors in
			let time = bytes_to_int_auto bytes in
			let job = BuiltinFunctions.end_function_call job in
			if time <= 0 then 
				(Active job, multijob, errors) 
			else
				(
					Active job, 
					{multijob with
						current_metadata = { multijob.current_metadata with priority = TimeWait time; };
					}, 
					errors
				)
		| _ -> failwith "timewait invalid arguments"

let otter_begin_atomic job multijob retopt exps errors = 
	let job = BuiltinFunctions.end_function_call job in
	(
		Active job, 
		{multijob with
			current_metadata = { multijob.current_metadata with priority  = Atomic; };
		},
		errors
	)

let otter_end_atomic job multijob retopt exps errors = 
	let job = BuiltinFunctions.end_function_call job in
	(
		Active job, 
		{multijob with
			current_metadata = { multijob.current_metadata with priority  = Running; };
		},
		errors
	)

let interceptor job multijob job_queue interceptor =
	try
		(

		(intercept_multi_function_by_name_internal "__otter_multi_fork"               libc_fork) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_gmalloc"            otter_gmalloc) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_gfree"              otter_gfree) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_get_pid"            otter_get_pid) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_get_parent_pid"     otter_get_parent_pid) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_set_parent_pid"     otter_set_parent_pid) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_io_block"           otter_io_block) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_time_wait"          otter_time_wait) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_begin_atomic"       otter_begin_atomic) @@@
		(intercept_multi_function_by_name_internal "__otter_multi_end_atomic"         otter_end_atomic) @@@

		(* pass on the job when none of those match *)
		interceptor

		) job multijob job_queue
	with Failure msg ->
		if !Executeargs.arg_failfast then failwith msg;
		(Complete (Abandoned (`Failure msg, job)), (multijob, job_queue))
