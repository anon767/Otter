open DataStructures
open OcamlUtilities
open CilUtilities
open OtterBytes
open OtterCore


let unreachable_global varinfo = not (Coverage.VarinfoSet.mem varinfo (!Coverage.reachable_globals))


let init_globalvars state globals =
	List.fold_left begin fun state g -> match g with
		| Cil.GVar(varinfo, { Cil.init=Some init }, _)
				when not (!Executeargs.arg_noinit_unreachable_globals && unreachable_global varinfo) ->
			let lhost_typ = varinfo.Cil.vtype in
			let size = (Cil.bitsSizeOf (lhost_typ)) / 8 in
			let zeros = Bytes.bytes__make size in
			let rec myInit (offset:Cil.offset) (i:Cil.init) (state, acc) =
				match i with
					| Cil.SingleInit(exp) ->
						let state, off, typ, errors  = Expression.flatten_offset state lhost_typ offset [] in
						let state, off_bytes, errors = Expression.rval state exp errors in
						assert(errors = []); (* there shouldn't be any errors during initialization *)

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
					&& not (!Executeargs.arg_noinit_unreachable_globals && unreachable_global varinfo) ->
				(* I think the list of globals is always in the same order as in the source
					code. In particular, I think there will never be a declaration of a
					variable after that variable has been defined, since CIL gets rid of
					such extra declarations. If this is true, then this should work fine. If
					not, a declaration occuring *after* a definition will wipe out the
					definition, replacing the value with zeros. *)
			let size = (Cil.bitsSizeOf (varinfo.Cil.vtype)) / 8 in
			let size = if size <= 0 then 1 else size in
			let init_bytes = Bytes.bytes__make size (* zeros *) in

			Output.set_mode Output.MSG_REG;
			Output.printf "Initialize %s to zeros\n" varinfo.Cil.vname;

			MemOp.state__add_global state varinfo init_bytes

		| _ ->
			state
	end state globals


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
	let argc = Bytes.int_to_bytes num_args in

	(* C's standard is to have the arguments be consecutive strings. For example, if the
		executed code were "./run abc de fgh", this would lead to the following chunk of
		memory:
		['.','/','r','u','n','\000','a','b','c','\000','d','e','\000','f','g','h','\000']
		Let's create this bytes. *)
	(* First we make the concatenated string *)
	let argv_strings = String.concat "\000" argstr in

	(* Then we make a bytes (specifically, a make_Bytes_ByteArray) out of this string *)
	let argv_strings_bytes = Bytes.string_to_bytes argv_strings in

	(* Make a block to point to this bytes. *)
	(* The block's size will be one more than the length of argv_strings (because of the
		terminating null byte). *)
	let argv_strings_block = Bytes.block__make "argv_strings" ((String.length argv_strings) + 1) Bytes.Block_type_Local in

	(* Map the block we just made to the bytes we just made *)
	let state = MemOp.state__add_block state argv_strings_block argv_strings_bytes in

	let charPtrSize = Cil.bitsSizeOf Cil.charPtrType / 8 in

	(* TODO: argv[argc] is supposed to be a null pointer. [Standard 5.1.2.2.1] *)
	(* Now, make a block for the array of pointers, with room for a pointer for each argument *)
	let argv_ptrs_block = Bytes.block__make "argv_pointers" (num_args * charPtrSize) Bytes.Block_type_Local in

	(* Make the byteArray of pointers by making each individual pointer and putting them
		 into the array using MemOp's bytes__write function. *)
	let rec impl argstr ptrsSoFar charsSoFar bytes =
		match argstr with
			| [] -> bytes
			| h::t ->
				(* Print out each argument *)
				Output.set_mode Output.MSG_DEBUG;
				Output.printf "With arguments: \"%s\"@\n" h;
				let h_bytes = Bytes.make_Bytes_Address (argv_strings_block, Bytes.int_to_bytes charsSoFar) in
				impl t (ptrsSoFar + 1)
					(charsSoFar + String.length h + 1 (* '+ 1' for the null character *))
					(BytesUtility.bytes__write bytes (Bytes.int_to_bytes (ptrsSoFar * charPtrSize)) charPtrSize h_bytes)
	in
	let argv_ptrs_bytes =
		impl argstr 0 0 (Bytes.make_Bytes_ByteArray (ImmutableArray.make (num_args * charPtrSize) Bytes.byte__zero)) in

	(* Map the pointers block to its bytes *)
	let state = MemOp.state__add_block state argv_ptrs_block argv_ptrs_bytes in

	(* Make the top-level address that is the actual argv. It is the address of
		argv_ptrs_bytes. We do not have to map this to anything; we just pass it as the
		argument to main. *)
	let argv = Bytes.make_Bytes_Address (argv_ptrs_block, Bytes.bytes__zero) in

	(* Finally, return the updated state and the list of arguments *)
	(state, [argc; argv])


(* create a job that begins at the main function of a file, with the initial state set up for the file *)
let make file cmdline =
	let main_func =
		try FindCil.fundec_by_name file "main"
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
	Job.make file state main_func main_args



