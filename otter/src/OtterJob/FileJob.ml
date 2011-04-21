open DataStructures
open OcamlUtilities
open CilUtilities
open OtterBytes
open OtterCore

let unreachable_global varinfo = not (Coverage.VarinfoSet.mem varinfo (!Coverage.reachable_globals))

let init_globalvars job globals =
    let job, _ = List.fold_left begin fun (job, varinfo_to_block) g -> match g with
        | Cil.GVar(varinfo, { Cil.init=Some init }, _)
                when not (!Executeargs.arg_noinit_unreachable_globals && unreachable_global varinfo) ->

            let job, block, varinfo_to_block =
                (* Initialize the block first, for recursive initializations such as 'int p = (int)&p;'. *)
                try
                    (job, State.VarinfoMap.find varinfo varinfo_to_block, varinfo_to_block)
                with Not_found ->
                    let job, block = MemOp.state__add_global job varinfo in
                    (job, block, State.VarinfoMap.add varinfo block varinfo_to_block)
            in
            let job, bytes = Expression.evaluate_initializer job varinfo.Cil.vtype init in

            Output.set_mode Output.MSG_REG;
            Output.printf "@[Initialize %s to@ @[%a@]@]@." varinfo.Cil.vname BytesPrinter.bytes bytes;

            (MemOp.state__add_block job block bytes, varinfo_to_block)

        | Cil.GVar(varinfo, _, _)
        | Cil.GVarDecl(varinfo, _)
                when not (Cil.isFunctionType varinfo.Cil.vtype)
                    && not (!Executeargs.arg_noinit_unreachable_globals && unreachable_global varinfo)
                    && not (State.VarinfoMap.mem varinfo varinfo_to_block) ->
            Output.set_mode Output.MSG_REG;
            Output.printf "Initialize %s to zeros@." varinfo.Cil.vname;

            let job, block = MemOp.state__add_global job varinfo in
            (job, State.VarinfoMap.add varinfo block varinfo_to_block)

        | _ ->
            (job, varinfo_to_block)
    end (job, State.VarinfoMap.empty) globals in
    job


(* To initialize the arguments, we need to create a bytes which represents argc and
	one which represents argv. The one for argc is simple: count how many arguments
	and make that value into a bytes. For argv, we need a bytes which is the address
	of a byteArray of byte-s, each of which is the address of some offset into one
	big memory block which holds all of the arguments (as strings). This last memory
	block is itself a byteArray of concrete byte-s (the characters in the arguments).
	Because of the way things point to each other, we construct these three layers
	bottom-up. *)
let init_cmdline_argvs job argstr =
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
	let argv_strings_block = Bytes.block__make "argv_strings" (Bytes.int_to_bytes (String.length argv_strings + 1)) Bytes.Block_type_Local in

	(* Map the block we just made to the bytes we just made *)
	let job = MemOp.state__add_block job argv_strings_block argv_strings_bytes in

	let charPtrSize = Cil.bitsSizeOf Cil.charPtrType / 8 in

	(* TODO: argv[argc] is supposed to be a null pointer. [Standard 5.1.2.2.1] *)
	(* Now, make a block for the array of pointers, with room for a pointer for each argument *)
	let argv_ptrs_block = Bytes.block__make "argv_pointers" (Bytes.int_to_bytes (num_args * charPtrSize)) Bytes.Block_type_Local in

	(* Make the byteArray of pointers by making each individual pointer and putting them
		 into the array using MemOp's bytes__write function. *)
	let rec impl argstr ptrsSoFar charsSoFar bytes =
		match argstr with
			| [] -> bytes
			| h::t ->
				(* Print out each argument *)
				Output.debug_printf "@[With arguments: \"%s\"@]@." h;
				let h_bytes = Bytes.make_Bytes_Address (argv_strings_block, Bytes.int_to_bytes charsSoFar) in
				impl t (ptrsSoFar + 1)
					(charsSoFar + String.length h + 1 (* '+ 1' for the null character *))
					(snd (BytesUtility.bytes__write () bytes (Bytes.int_to_bytes (ptrsSoFar * charPtrSize)) charPtrSize h_bytes))
	in
	let argv_ptrs_bytes =
		impl argstr 0 0 (Bytes.make_Bytes_ByteArray (ImmutableArray.make (num_args * charPtrSize) Bytes.byte__zero)) in

	(* Map the pointers block to its bytes *)
	let job = MemOp.state__add_block job argv_ptrs_block argv_ptrs_bytes in

	(* Make the top-level address that is the actual argv. It is the address of
		argv_ptrs_bytes. We do not have to map this to anything; we just pass it as the
		argument to main. *)
	let argv = Bytes.make_Bytes_Address (argv_ptrs_block, Bytes.bytes__zero) in

	(* Finally, return the updated state and the list of arguments *)
	(job, [argc; argv])


(* create a job that begins at the main function of a file, with the initial state set up for the file *)
class ['abandoned, 'truncated] t file cmdline =
    let main_func = ProgramPoints.get_main_fundec file in
    object (self : 'self)
        inherit ['abandoned, 'truncated] OtterCore.Job.t file main_func
        initializer
            let job = self in

            (* Initialize the state with zeroed globals *)
            let job = init_globalvars job file.Cil.globals in

            (* prepare the command line arguments, if needed *)
            let job, main_args =
                match main_func.Cil.svar.Cil.vtype with
                    | Cil.TFun (_, Some [], _, _) -> (job, []) (* main has no arguments *)
                    | _ -> init_cmdline_argvs job cmdline
            in

            (* enter the function *)
            let job = MemOp.state__start_fcall job State.Runtime main_func main_args in

            self#become job
    end

