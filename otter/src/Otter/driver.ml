open DataStructures
open OcamlUtilities
open Executeargs
open OtterBytes
open Bytes
open Types
open Interceptors


let unreachable_global varinfo = not (Cilutility.VarinfoSet.mem varinfo (!GetProgInfo.reachable_globals))

let init_symbolic_pointer state varinfo size =
	(* TODO: what's the right size?
	* For now, assume that each pointer points to an array of size 1.
	*)
	let name = FormatPlus.sprintf "Two-fold Sym Ptr (%s)" varinfo.Cil.vname in
	let block = block__make name size Block_type_Aliased in
	let addrof_block = make_Bytes_Address (block, bytes__zero) in
	let state = MemOp.state__add_block state block (bytes__symbolic size) in
	(state, make_Bytes_Conditional (conditional__from_list [Unconditional bytes__zero; Unconditional addrof_block]))


let init_symbolic_varinfo state varinfo =
	let typ = varinfo.Cil.vtype in
	let size = (Cil.bitsSizeOf (typ)) / 8 in
	let size = if size <= 0 then 1 else size in
	match typ with
		| Cil.TPtr (basetyp,_) ->
			assert (size==4);
			Output.set_mode Output.MSG_REG;
			Output.printf "Initialize %s to ITE(?,null,non-null)\n" varinfo.Cil.vname;
			init_symbolic_pointer state varinfo size
		| _ ->
			Output.set_mode Output.MSG_REG;
			Output.printf "Initialize %s to symbolic\n" varinfo.Cil.vname;
			(state, bytes__symbolic size)


let init_symbolic_globalvars state globals =
	List.fold_left begin fun state g -> match g with
		| Cil.GVar (varinfo, _, _)
		| Cil.GVarDecl (varinfo, _)
				when not (Cil.isFunctionType varinfo.Cil.vtype)
					 && not (Executeargs.run_args.arg_noinit_unreachable_globals && unreachable_global varinfo) ->
			let state,init_bytes = init_symbolic_varinfo state varinfo in
			MemOp.state__add_global state varinfo init_bytes
		| _ ->
			state
	end state globals


let init_globalvars state globals =
	List.fold_left begin fun state g -> match g with
		| Cil.GVar(varinfo, { Cil.init=Some init }, _)
				when not (Executeargs.run_args.arg_noinit_unreachable_globals && unreachable_global varinfo) ->
			Output.set_mode Output.MSG_MUSTPRINT;
			let lhost_typ = varinfo.Cil.vtype in
			let size = (Cil.bitsSizeOf (lhost_typ)) / 8 in
			let zeros = bytes__make size in
			let rec myInit (offset:Cil.offset) (i:Cil.init) (state, acc) =
				match i with
					| Cil.SingleInit(exp) ->
						let state, off, typ = Eval.flatten_offset state lhost_typ offset in
						let state, off_bytes = Eval.rval state exp in
						let size = (Cil.bitsSizeOf typ) / 8 in
						let init_bytes = BytesUtility.bytes__write acc off size off_bytes in
						(state, init_bytes)
					| Cil.CompoundInit(typ, list) ->
						Cil.foldLeftCompound
							~implicit:false
							~doinit:(fun off i t (state, acc) -> myInit (Cil.addOffset off offset) i (state, acc))
							~ct:typ
							~initl:list
							~acc:(state, acc)
			in
			let state, init_bytes = myInit Cil.NoOffset init (state, zeros) in

			Output.set_mode Output.MSG_REG;
			if init_bytes == zeros then
				Output.printf "Initialize %s to zeros@\n" varinfo.Cil.vname
			else
				Output.printf "Initialize %s to@ @[%a@]@\n" varinfo.Cil.vname BytesPrinter.bytes init_bytes;

			MemOp.state__add_global state varinfo init_bytes

		| Cil.GVar(varinfo, _, _)
		| Cil.GVarDecl(varinfo, _)
				when not (Cil.isFunctionType varinfo.Cil.vtype)
					&& not (Executeargs.run_args.arg_noinit_unreachable_globals && unreachable_global varinfo) ->
				(* I think the list of globals is always in the same order as in the source
					code. In particular, I think there will never be a declaration of a
					variable after that variable has been defined, since CIL gets rid of
					such extra declarations. If this is true, then this should work fine. If
					not, a declaration occuring *after* a definition will wipe out the
					definition, replacing the value with zeros. *)
			let size = (Cil.bitsSizeOf (varinfo.Cil.vtype)) / 8 in
			let size = if size <= 0 then 1 else size in
			let init_bytes = bytes__make size (* zeros *) in

			Output.set_mode Output.MSG_REG;
			Output.printf "Initialize %s to zeros\n" varinfo.Cil.vname;

			MemOp.state__add_global state varinfo init_bytes

		| _ ->
			state
	end state globals



(* Initialize arguments for an entry function to purely symbolic *)
let init_symbolic_argvs state entryfn =
	let state = MemOp.state__add_frame state in
	let state, args, bytesToVars =
		List.fold_right (* TODO: Does ordering matter? *)
			begin fun varinfo (state, args, bytesToVars) ->
				let state,init = init_symbolic_varinfo state varinfo in
				(MemOp.state__add_formal state varinfo init, init::args, (init, varinfo)::bytesToVars)
			end
			entryfn.Cil.sformals
			(state, [], [])
	in
	let exHist = { emptyHistory with bytesToVars = bytesToVars } in
	(state, args, exHist)


(* To initialize the arguments, we need to create a bytes which represents argc and
	one which represents argv. The one for argc is simple: count how many arguments
	and make that value into a bytes. For argv, we need a bytes which is the address
	of a byteArray of byte-s, each of which is the address of some offset into one
	big memory block which holds all of the arguments (as strings). This last memory
	block is itself a byteArray of concrete byte-s (the characters in the arguments).
	Because of the way things point to each other, we construct these three layers
	bottom-up. *)
let init_cmdline_argvs state argstr =
	(* How many arguments were there? *)
	let num_args = List.length argstr in

	(* Convert the number of arguments into a 'bytes' *)
	let argc = int_to_bytes num_args in

	(* C's standard is to have the arguments be consecutive strings. For example, if the
		executed code were "./run abc de fgh", this would lead to the following chunk of
		memory:
		['.','/','r','u','n','\000','a','b','c','\000','d','e','\000','f','g','h','\000']
		Let's create this bytes. *)
	(* First we make the concatenated string *)
	let argv_strings = String.concat "\000" argstr in

	(* Then we make a bytes (specifically, a make_Bytes_ByteArray) out of this string *)
	let argv_strings_bytes = string_to_bytes argv_strings in

	(* Make a block to point to this bytes. *)
	(* The block's size will be one more than the length of argv_strings (because of the
		terminating null byte). *)
	let argv_strings_block = block__make "argv_strings" ((String.length argv_strings) + 1) Block_type_Local in

	(* Map the block we just made to the bytes we just made *)
	let state = MemOp.state__add_block state argv_strings_block argv_strings_bytes in

	let charPtrSize = Cil.bitsSizeOf Cil.charPtrType / 8 in

	(* TODO: argv[argc] is supposed to be a null pointer. [Standard 5.1.2.2.1] *)
	(* Now, make a block for the array of pointers, with room for a pointer for each argument *)
	let argv_ptrs_block = block__make "argv_pointers" (num_args * charPtrSize) Block_type_Local in

	(* Make the byteArray of pointers by making each individual pointer and putting them
		 into the array using MemOp's bytes__write function. *)
	let rec impl argstr ptrsSoFar charsSoFar bytes =
		match argstr with
			| [] -> bytes
			| h::t ->
				(* Print out each argument *)
				Output.set_mode Output.MSG_DEBUG;
				Output.printf "With arguments: \"%s\"@\n" h;
				let h_bytes = make_Bytes_Address (argv_strings_block, int_to_bytes charsSoFar) in
				impl t (ptrsSoFar + 1)
					(charsSoFar + String.length h + 1 (* '+ 1' for the null character *))
					(BytesUtility.bytes__write bytes (int_to_bytes (ptrsSoFar * charPtrSize)) charPtrSize h_bytes)
	in
	let argv_ptrs_bytes =
		impl argstr 0 0 (make_Bytes_ByteArray (ImmutableArray.make (num_args * charPtrSize) byte__zero)) in

	(* Map the pointers block to its bytes *)
	let state = MemOp.state__add_block state argv_ptrs_block argv_ptrs_bytes in

	(* Make the top-level address that is the actual argv. It is the address of
		argv_ptrs_bytes. We do not have to map this to anything; we just pass it as the
		argument to main. *)
	let argv = make_Bytes_Address (argv_ptrs_block, bytes__zero) in

	(* Finally, return the updated state and the list of arguments *)
	(state, [argc; argv])


(* set up the file for symbolic execution *)
let prepare_file file =
	(* makeCFGFeature must precede the call to getProgInfo. *)
	Cilly.makeCFGFeature.Cil.fd_doit file;

	Cil.iterGlobals file begin function
		| Cil.GFun(fundec,_) ->
			(* Reset sids to be unique only within a function, rather than globally, so that they will be given
				consistent sids across different analysis runs even if different files are merged
			*)
			ignore (List.fold_left (fun n stmt -> stmt.Cil.sid <- n; succ n) 0 fundec.Cil.sallstmts);

		| _ ->
			()
	end;

	if Executeargs.run_args.arg_noinit_unreachable_globals then
		GetProgInfo.computeReachableCode file;

	(* Find all lines, blocks, edges, and conditions. *)
	(* TODO: wrap the listings of Lines,Edges,etc... *)
	let setOfLines, setOfBlocks, setOfEdges, setOfConds = GetProgInfo.getProgInfo file run_args.arg_fns in
	run_args.arg_num_lines <- LineSet.cardinal setOfLines;
	run_args.arg_num_blocks <- StmtInfoSet.cardinal setOfBlocks;
	run_args.arg_num_edges <- EdgeSet.cardinal setOfEdges;
	run_args.arg_num_conds <- CondSet.cardinal setOfConds;
	if run_args.arg_list_lines then begin
		Output.printf "Total number of %s: %d\n" "Lines" run_args.arg_num_lines;
		LineSet.iter
			(fun (file, lineNum) -> Output.printf "%s:%d\n" file lineNum)
			setOfLines;
		Output.printf "\n"
	end;
	if run_args.arg_list_blocks then begin
		Output.printf "Total number of %s: %d\n" "Blocks" run_args.arg_num_blocks;
		StmtInfoSet.iter
			(fun stmtInfo -> Output.printf "%a\n" TypesPrinter.stmtInfo stmtInfo)
			setOfBlocks;
		Output.printf "\n"
	end;
	if run_args.arg_list_edges then begin
		Output.printf "Total number of %s: %d\n" "Edges" run_args.arg_num_edges;
		EdgeSet.iter
			(fun (srcStmtInfo, destStmtInfo) ->
				 Output.printf "%a -> %a\n"
					 TypesPrinter.stmtInfo srcStmtInfo
					 TypesPrinter.stmtInfo destStmtInfo)
			setOfEdges;
		Output.printf "\n"
	end;
	if run_args.arg_list_conds then begin
		Output.printf "Total number of %s: %d\n" "Conditions" run_args.arg_num_conds;
		CondSet.iter
			(fun (stmtInfo, truth) -> Output.printf "%a %c\n" TypesPrinter.stmtInfo stmtInfo (if truth then 'T' else 'F'))
		setOfConds;
		Output.printf "\n"
	end


(* create a job that begins at a function, given an initial state *)
let job_for_function ?(exHist=emptyHistory) file state fn argvs =
	let state = MemOp.state__start_fcall state Runtime fn argvs in
	let trackedFns = List.fold_left (fun set elt -> StringSet.add elt set) StringSet.empty run_args.arg_fns in
	(* create a new job *)
	{
		file = file;
		state = state;
		exHist = exHist;
		instrList = [];
		stmt = List.hd fn.Cil.sallstmts;
		trackedFns = trackedFns;
		inTrackedFn = StringSet.mem fn.Cil.svar.Cil.vname trackedFns;
		jid = Counter.next Types.job_counter;
	}


(* create a job that begins in the middle of a file at some entry function with some optional constraints *)
let job_for_middle file entryfn =

    (* Initialize the state with symbolic globals *)
    let state = MemOp.state__empty in
    let state = init_symbolic_globalvars state file.Cil.globals in

    let state, entryargs, exHist = init_symbolic_argvs state entryfn in

    (* create a job starting at entryfn *)
    job_for_function ~exHist:exHist file state entryfn entryargs


(* create a job that begins at the main function of a file, with the initial state set up for the file *)
let job_for_file file cmdline =
	let main_func =
		try Cilutility.find_fundec_by_name file "main"
		with Not_found -> failwith "No main function found!"
	in

	(* Initialize the state with zeroed globals *)
	let state = MemOp.state__empty in
	let state = init_globalvars state file.Cil.globals in

	(* prepare the command line arguments, if needed *)
	let state, main_args =
		match main_func.Cil.svar.Cil.vtype with
			| Cil.TFun (_, Some [], _, _) -> state, [] (* main has no arguments *)
			| _ -> init_cmdline_argvs state cmdline
	in

	(* create a job starting at main *)
	job_for_function file state main_func main_args


let find_entryfn file =
	let fname = Executeargs.run_args.arg_entryfn in
	try
		Cilutility.find_fundec_by_name file fname
	with Not_found ->
		FormatPlus.failwith "Entry function %s not found" fname



(** GET JOB **)

let get_job_list = function
	| [] -> None
	| h::t -> Some (h, t)


(** PROCESS RESULT **)

let output_completion_info completion =
(* log some interesting errors *)
	match completion with
		| Types.Abandoned (msg, loc, { result_state=state; result_history=hist }) ->
			Output.set_mode Output.MSG_MUSTPRINT;
			Output.printf "Error \"%s\"@ occurs at %a.@\n"
				msg Printcil.f_loc loc;
			if Executeargs.print_args.arg_print_callstack then
				Output.printf "Call stack:@\n  @[%a@]@\n" (TypesPrinter.callingContext_list "@\n") state.callContexts;
			Output.printf "Abandoning path.@\n"

		| _ ->
			()

let rec process_result result completed job_queue =
	match result with
		| Active job ->
			(completed, job::job_queue)

		| Fork states ->
			List.fold_left (fun (completed, job_queue) state -> process_result state completed job_queue) (completed, job_queue) states

		| Complete completion ->
			output_completion_info completion;
			((completion::completed), job_queue)

		| _ ->
			(completed, job_queue)


(** MAIN LOOP **)

let main_loop get_job interceptor process_result job_queue =
	let rec main_loop job_queue completed =
		match get_job job_queue with
			| Some (job, job_queue) ->
				let result_opt =
					try
						let result, job_queue = interceptor job job_queue in
						let completed, job_queue = process_result result completed job_queue in
						Some (job_queue, completed)
					with SignalException s ->
						(* if we got a signal, stop and return the completed results *)
						Output.set_mode Output.MSG_MUSTPRINT;
						Output.printf "%s@\n" s;
						None
				in
				begin match result_opt with
					| Some (job_queue, completed) -> main_loop job_queue completed
					| None -> completed
				end
			| None ->
				completed
	in
	main_loop job_queue []

let run job =
	main_loop
		get_job_list
		(
			set_output_formatter_interceptor @@
			Builtin_function.interceptor @@
			Core.step
		)
		process_result
		[job]

let run_with_libc job =
	main_loop
		get_job_list
		(
			set_output_formatter_interceptor @@
			Builtin_function.interceptor @@
			Builtin_function.libc_interceptor @@
			Core.step
		)
		process_result
		[job]

