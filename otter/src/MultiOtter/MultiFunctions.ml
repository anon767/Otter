open DataStructures
open OcamlUtilities
open CilUtilities
open Cil
open OtterCore
open OtterBytes
open MultiJob
open State
open Job
open Bytes
open Interceptor
open MultiInterceptor

let libc_fork job retopt exps errors =
    (* update instruction pointer, history, and such *)
    let job = BuiltinFunctions.end_function_call job in

    (* While we probably could handle it, it's probably safer to disallow forking in atomic sections. *)
    begin match job#priority with
        Atomic _ -> failwith "Forking within an atomic section"
      | _ -> ()
    end;

    (* TODO: make the pid symbolic *)
    let child_job = job in
    let child_job = child_job#with_pid job#next_pid in
    let child_job = child_job#with_parent_pid job#pid in
    let job = job#with_next_pid (job#next_pid + 1) in

    Output.set_mode Output.MSG_REG;
    Output.printf "fork(): parent: %d, child: %d@." job#pid child_job#pid;

    let job, child_job, errors = match retopt with
        | None ->
            (job, child_job, errors)
        | Some cil_lval ->
            let child_job, lval, errors = Expression.lval child_job cil_lval errors in
            let child_job = MemOp.state__assign child_job lval (Bytes.bytes__zero) in
            let job, lval, errors = Expression.lval job cil_lval errors in
            let job = MemOp.state__assign job lval (Bytes.int_to_bytes child_job#pid) in
            (job, child_job, errors)
    in
    let job = job#with_other_processes ((new process_state child_job)::job#other_processes) in
    (Active job, errors)

(* allocates on the global heap *)
let otter_gmalloc_size job size bytes loc =
	let name = FormatPlus.sprintf "%s(%d)#%d/%a%s"
		(List.hd job#state.State.callstack).Cil.svar.Cil.vname
		size
		(DataStructures.Counter.next BuiltinFunctions.libc___builtin_alloca__id)
		Printcil.loc loc
		(MemOp.state__trace job)
	in
	let block = Bytes.block__make name (int_to_offset_bytes size) Bytes.Block_type_Heap in
	let addrof_block = Bytes.make_Bytes_Address (block, Bytes.bytes__zero) in
	let job = MemOp.state__add_block job block bytes in
	(job, block, addrof_block)

let otter_gmalloc job retopt exps errors =
	let job, b_size, errors = Expression.rval job (List.hd exps) errors in
	let size =
		if Bytes.isConcrete_bytes b_size then
			Bytes.bytes_to_int_auto b_size (*safe to use bytes_to_int as arg should be small *)
		else
			1 (* currently bytearray have unbounded length *)
	in
	let bytes = bytes__make_default size (!InitBytes.init_malloc ()) in
	let job, block, bytes = otter_gmalloc_size job size bytes (Job.get_loc job) in
	let job, errors = BuiltinFunctions.set_return_value job retopt bytes errors in
	let job = BuiltinFunctions.end_function_call job in

	let job = job#with_state { job#state with block_to_bytes = MemoryBlockMap.add block (Deferred.Immediate bytes) job#state.block_to_bytes; } in
	let job = job#with_shared_blocks (SharedBlocks.add block job#shared_blocks) in
	let job = job#with_other_processes begin List.map
		begin fun other ->
			other#with_state { other#state with block_to_bytes = MemoryBlockMap.add block (Deferred.Immediate bytes) other#state.block_to_bytes; }
		end
		job#other_processes
	end in
	(Active job, errors)

let otter_gfree job retopt exps errors =
    let job, ptr, errors = Expression.rval job (List.hd exps) errors in
    match ptr with
        | Bytes.Bytes_Address (block, _) ->
            if block.Bytes.memory_block_type != Bytes.Block_type_Heap then
                FormatPlus.failwith "gfreeing a non-gmalloced pointer:@ @[%a@]@ = @[%a@]@\n" CilPrinter.exp (List.hd exps) BytesPrinter.bytes ptr
            else if not (SharedBlocks.mem block job#shared_blocks) then
                FormatPlus.failwith "gfreeing a non-gmalloced pointer or double-gfree:@ @[%a@]@ = @[%a@]@\n" CilPrinter.exp (List.hd exps) BytesPrinter.bytes ptr
            else if not (MemoryBlockMap.mem block job#state.State.block_to_bytes) then
                FormatPlus.failwith "gfreeing after free:@ @[%a@]@ = @[%a@]@\n" CilPrinter.exp (List.hd exps) BytesPrinter.bytes ptr
            else
                let job = job#with_state { job#state with block_to_bytes = MemoryBlockMap.remove block job#state.block_to_bytes; } in
                let job = job#with_shared_blocks (SharedBlocks.remove block job#shared_blocks) in
                let job = job#with_other_processes begin List.map
                    begin fun other ->
                        other#with_state { other#state with block_to_bytes = MemoryBlockMap.remove block other#state.block_to_bytes; }
                    end
                    job#other_processes
                end in
                let job = MemOp.state__remove_block job block in
                let job, errors = BuiltinFunctions.set_return_value job retopt bytes__zero errors in
                let job = BuiltinFunctions.end_function_call job in
                (Active job, errors)

        | _ ->
            if not (bytes__equal ptr bytes__zero) (* Freeing a null pointer is fine; it does nothing. *)
            then (
                Output.set_mode Output.MSG_ERROR;
                FormatPlus.failwith "gfreeing something that is not a valid pointer:@ @[%a@]@ = @[%a@]@\n" CilPrinter.exp (List.hd exps) BytesPrinter.bytes ptr
            );
            let job = BuiltinFunctions.end_function_call job in
            (Active job, errors)

let otter_get_pid job retopt exps errors =
	let job, errors = BuiltinFunctions.set_return_value job retopt (int_to_bytes job#pid) errors in
	let job = BuiltinFunctions.end_function_call job in
	(Active job, errors)

let otter_get_parent_pid job retopt exps errors =
	match exps with
		| [CastE (_, h)] | [h] ->
			let job, bytes, errors = Expression.rval job h errors in
			let pid = bytes_to_int_auto bytes in
			let ppid =
				if pid = job#pid then
					job#parent_pid
				else
					try
						let other = List.find (fun other -> pid = other#pid) job#other_processes in
						other#parent_pid
					with Not_found ->
						-1
			in
			let job, errors = BuiltinFunctions.set_return_value job retopt (int_to_bytes ppid) errors in
			let job = BuiltinFunctions.end_function_call job in
			(Active job, errors)
		| _ -> failwith "get_parent_id invalid arguments"	

let otter_set_parent_pid job retopt exps errors =
	match exps with
		| [CastE (_, h)] | [h] ->
			let job, bytes, errors = Expression.rval job h errors in
			let pid = bytes_to_int_auto bytes in
			let job = BuiltinFunctions.end_function_call job in
			let job = job#with_parent_pid pid in
			(Active job, errors)
		| _ -> failwith "set_parent_id invalid arguments"

let otter_io_block_common job pointers errors =
    let find_blocks job ptr_bytes errors =
        let job, lvals, errors = Expression.deref job ptr_bytes Cil.voidPtrType errors in
        let blocks = conditional__fold
            (fun acc guard (block, _) ->
                 (* TODO: Failing in this case might be overkill. Printing a warning might be good enough *)
                 if not (SharedBlocks.mem block job#shared_blocks)
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
            pointers
    in
	let job =
		if blocks = [] then
			failwith "io_block with no underlying blocks"
		else
			let block_to_bytes = 
				List.fold_left
					(fun acc block -> 
						try
							MemoryBlockMap.add block (MemoryBlockMap.find block job#state.block_to_bytes) acc
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
				job#with_priority (IOBlock block_to_bytes)
	in

	let job = BuiltinFunctions.end_function_call job in
	(Active job, errors)

(* takes a variadic list of pointers to blocks to watch *)
let otter_io_block job _ exps errors =
    let job, pointers, errors =
        List.fold_left
            (fun (job, pointers, errors) exp ->
                 let job, pointer, errors = Expression.rval job exp errors in
                 (job, pointer::pointers, errors)
            )
            (job, [], errors)
            exps
    in
    otter_io_block_common job pointers errors

(** [c_array_to_ocaml_list bytes elt_size num_elts] converts a C array into an OCaml list
    @param bytes the bytes representing the array
    @param elt_size the size, in bytes, of the elements in the array
    @param num_elts the number of elements in the array
    @return a list of bytes representing the elements of the array *)
let c_array_to_ocaml_list bytes elt_size num_elts =
    let rec get_elt i acc =
        if i < 0
        then acc
        else
            let (), elt = BytesUtility.bytes__read () bytes (int_to_bytes i) elt_size in
            get_elt (i - elt_size) (elt :: acc)
    in
    get_elt (elt_size * (num_elts - 1)) []

(* takes an array of pointers to blocks to watch, and the array's length *)
let otter_io_block_array job _ exps errors =
    let ptr_to_array_exp, length_exp =
        match exps with
          | [ ptr_to_array_exp; length_exp ] -> (ptr_to_array_exp, length_exp)
          | _ ->
                FormatPlus.failwith "io_block expects exactly two arguments but got (%a)" (FormatPlus.pp_print_list Printcil.exp ", ") exps
    in
    let job, length, errors = Expression.rval job length_exp errors in
    let length = bytes_to_int_auto length in
    let ptr_size = Cil.bitsSizeOf Cil.voidPtrType / 8 in
    let job, _, lvals, errors = BuiltinFunctions.access_exp_with_length job ptr_to_array_exp errors (ptr_size * length) in
    let job, array_of_ptrs = MemOp.state__deref job (lvals, ptr_size * length) in
    let pointers = c_array_to_ocaml_list array_of_ptrs ptr_size length in
    otter_io_block_common job pointers errors

let otter_time_wait job retopt exps errors =
	match exps with
		| [CastE (_, h)] | [h] ->
			let job, bytes, errors = Expression.rval job h errors in
			let time = bytes_to_int_auto bytes in
			let job = BuiltinFunctions.end_function_call job in
			if time <= 0 then 
				(Active job, errors)
			else
				(
					Active (job#with_priority (TimeWait time)),
					errors
				)
		| _ -> failwith "timewait invalid arguments"

let otter_begin_atomic job retopt exps errors =
    let job = BuiltinFunctions.end_function_call job in
    let enter_atomic = function Atomic n -> Atomic (succ n) | _ -> Atomic 0 in
    (
        Active (job#with_priority (enter_atomic job#priority)),
        errors
    )

let otter_end_atomic job retopt exps errors =
    let leave_atomic = function
          Atomic 0 -> Running
        | Atomic n -> Atomic (pred n)
        | _ -> failwith "end_atomic outside an atomic section"
    in
    let job = BuiltinFunctions.end_function_call job in
    (
        Active (job#with_priority (leave_atomic job#priority)),
        errors
    )

let interceptor job interceptor =
	try
		(

		(intercept_function_by_name_internal "__otter_multi_fork"               libc_fork) @@
		(intercept_function_by_name_internal "__otter_multi_gmalloc"            otter_gmalloc) @@
		(intercept_function_by_name_internal "__otter_multi_gfree"              otter_gfree) @@
		(intercept_function_by_name_internal "__otter_multi_get_pid"            otter_get_pid) @@
		(intercept_function_by_name_internal "__otter_multi_get_parent_pid"     otter_get_parent_pid) @@
		(intercept_function_by_name_internal "__otter_multi_set_parent_pid"     otter_set_parent_pid) @@
		(intercept_function_by_name_internal "__otter_multi_io_block"           otter_io_block) @@
		(intercept_function_by_name_internal "__otter_multi_io_block_array"     otter_io_block_array) @@
		(intercept_function_by_name_internal "__otter_multi_time_wait"          otter_time_wait) @@
		(intercept_function_by_name_internal "__otter_multi_begin_atomic"       otter_begin_atomic) @@
		(intercept_function_by_name_internal "__otter_multi_end_atomic"         otter_end_atomic) @@

		(* pass on the job when none of those match *)
		interceptor

		) job
	with Failure msg ->
		if !Executeargs.arg_failfast then failwith msg;
		(Complete (Abandoned (`Failure msg, job)))
