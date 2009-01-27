open Cil
open Types
open Executeargs

let rec init_globalvars state globals =
	match globals with
		| [] -> state
		| GVar(varinfo, initinfo, loc):: tail ->
				Output.set_mode Output.MSG_MUSTPRINT;
					begin
					let lhost_typ = varinfo.vtype in
					let size = (Cil.bitsSizeOf (varinfo.vtype)) / 8 in
					let zeros = MemOp.bytes__make size in
					let init_bytes = match initinfo.init with
						| None -> zeros
						| Some(init) ->
								(* make the offset argument "accumulative",
								   i.e. not following the spec of Cil's offset in init type
								 *)
								let rec accumulate_offset offset off' =
									match offset with
										| NoOffset -> off'
										| Field(f,offset2) -> Field(f,accumulate_offset offset2 off')
										| Index(exp,offset2) -> Index(exp,accumulate_offset offset2 off')
								in
								let rec myInit  (offset:Cil.offset) (i:Cil.init) acc =
									match i with 
										| SingleInit(exp) -> 
											let (off,typ) = Eval.flatten_offset state lhost_typ offset in
											MemOp.bytes__write acc off (Cilutility.bitsSizeOfExp exp) (Eval.rval state exp)
										| CompoundInit(typ, list) ->          
											foldLeftCompound 
												~implicit: false
												~doinit: (fun off' i' t' acc ->	myInit  (accumulate_offset offset off') i' acc)
												~ct: typ
												~initl: list
												~acc: acc
								in
									myInit NoOffset init zeros
					in
					Output.print_endline ("Initialize "^varinfo.vname(*^" with value "*)
						(*^(To_string.bytes init_bytes)*)
					);					
					let state2 = MemOp.state__add_global state varinfo init_bytes in
					init_globalvars state2 tail
				end
		| GVarDecl(varinfo, loc):: tail when (not(Cil.isFunctionType varinfo.vtype)) ->
				(* I think the list of globals is always in the same order as in the source
					 code. In particular, I think there will never be a declaration of a
					 variable after that variable has been defined, since CIL gets rid of
					 such extra declarations. If this is true, then this should work fine. If
					 not, a declaration occuring *after* a definition will wipe out the
					 definition, replacing the value with zeros. *)
				Output.set_mode Output.MSG_DEBUG;
				Output.print_endline ("Initialize "^varinfo.vname^" without initial value"
					(*^(To_string.bytes init)*)
				);
				let size = (Cil.bitsSizeOf (varinfo.vtype)) / 8 in
				let size = if size <= 0 then 1 else size in
				let init_bytes = MemOp.bytes__make size (* zeros *)
				in
				let state2 = MemOp.state__add_global state varinfo init_bytes in
				init_globalvars state2 tail
		| _:: tail -> init_globalvars state tail
;;

(* To initialize the arguments, we need to create a bytes which represents argc and
	 one which represents argv. The one for argc is simple: count how many arguments
	 and make that value into a bytes. For argv, we need a bytes which is the address
	 of a byteArray of byte-s, each of which is the address of some offset into one
	 big memory block which holds all of the arguments (as strings). This last memory
	 block is itself a byteArray of concrete byte-s (the characters in the arguments).
	 Because of the way things point to each other, we construct these three layers
	 bottom-up. *)
let init_cmdline_argvs state fileName =
	(* (argstr : string list) is the list of arguments beginning with the name of the command itself
		 and continuing with all arguments passed by using '--arg' *)
	let argstr = fileName :: Executeargs.run_args.arg_cmdline_argvs in

	(* How many arguments were there? *)
	let num_args = List.length argstr in

	(* Convert the number of arguments into a 'bytes' *)
	let argc = Convert.lazy_int_to_bytes num_args in

	(* C's standard is to have the arguments be consecutive strings. For example, if the
		 executed code were "./run abc de fgh", this would lead to the following chunk of
		 memory:
		 ['.','/','r','u','n','\000','a','b','c','\000','d','e','\000','f','g','h','\000']
		 Let's create this bytes. *)
	(* First we make the concatenated string *)
	let argv_strings = String.concat "\000" argstr in

	(* Then we make a bytes (specifically, a Bytes_ByteArray) out of this string *)
	let argv_strings_bytes = Convert.string_to_bytes argv_strings in

	(* Make a block to point to this bytes. *)
	(* The block's size will be one more than the length of argv_strings (because of the
		 terminating null byte). *)
	let argv_strings_block = MemOp.block__make "argv_strings" ((String.length argv_strings) + 1) Block_type_Local in

	(* Map the block we just made to the bytes we just made *)
	let state' = MemOp.state__add_block state argv_strings_block argv_strings_bytes in

(* TODO: argv[argc] is supposed to be a null pointer. [Standard 5.1.2.2.1] *)
	(* Now, make a block for the array of pointers, with room for a pointer for each argument *)
	let argv_ptrs_block = MemOp.block__make "argv_pointers" (num_args * word__size) Block_type_Local in

	(* Make the byteArray of pointers by making each individual pointer and putting them
		 into the array using MemOp's bytes__write function. *)
	let rec impl argstr ptrsSoFar charsSoFar bytes : bytes =
		match argstr with
			[] -> bytes
		| h::t ->
				(* Print out each argument *)
				Output.set_mode Output.MSG_DEBUG;
				Output.print_endline ("With arguments: \""^h^"\"");
				let h_bytes =
					Bytes_Address (Some argv_strings_block, Convert.lazy_int_to_bytes charsSoFar) in
				impl t (ptrsSoFar + 1)
					(charsSoFar + String.length h + 1 (* '+ 1' for the null character *))
					(MemOp.bytes__write bytes (Convert.lazy_int_to_bytes (ptrsSoFar * word__size)) word__size h_bytes)
	in
	let argv_ptrs_bytes =
		impl argstr 0 0 (Bytes_ByteArray (ImmutableArray.make (num_args * word__size) MemOp.byte__zero)) in

	(* Map the pointers block to its bytes *)
	let state'' = MemOp.state__add_block state' argv_ptrs_block argv_ptrs_bytes in

	(* Make the top-level address that is the actual argv. It is the address of
		 argv_ptrs_bytes. We do not have to map this to anything; we just pass it as the
		 argument to main. *)
	let argv = Bytes_Address (Some argv_ptrs_block, MemOp.bytes__zero) in

	(* Finally, return the updated state and the list of arguments *)
	(state'', [argc; argv])
;;

module AnnotatedBytesSet = Set.Make
	(struct
		 type t = annotated_bytes
		 let compare = compare
	 end)

let doExecute (f: file) =
  if not !Cilutil.makeCFG then
    Errormsg.s (Errormsg.error
                  "--doexecute: you must also specify --domakeCFG\n");
	(*
	let f = Frontc.parse "int main(){return 0;}" () in
	*)
	Random.init 226;

	Output.set_mode Output.MSG_REG;
	Output.print_newline ();
  Output.print_endline "A (symbolic) executor for C";
  Output.print_newline ();
	
	Cil.initCIL ();
  Executedata.file := f;

	let (main_func,main_loc) = 
		try Function.from_signature "main"  
		with Not_found -> try Function.from_signature "main : int (void)"
		with Not_found -> try Function.from_signature "main : int (int,char**)"
		with Not_found -> failwith "No main function found!"
	in
(*
	let cfgOutChannel = open_out (Filename.basename (Filename.chop_extension main_loc.file)^".cfg") in
  iterGlobals f (fun g ->
		match g with GFun(fd,_) ->
      Cfg.printCfgChannel cfgOutChannel fd
    | _ -> ());
*)

	let state = MemOp.state__empty in
	let state1 = init_globalvars state f.globals in
	let (state2,main_args) = init_cmdline_argvs state1 f.fileName in
	let state3 = 
		try
			Driver.exec_function state2 emptyHistory main_func main_args MainEntry
		with Function.Notification_Exit (state_exit,_) -> state_exit		
	in    
	let rep = Report.format state3 in
	
	Output.set_mode Output.MSG_MUSTPRINT;
	Output.print_endline rep;
	Output.print_endline (Executedebug.get_log ());
	(* function stat 
	Output.print_endline "\nFunction call stat:";
	Cilutility.FundecMap.iter (fun f c -> Output.print_endline ((To_string.fundec f)^" : "^(string_of_int c))) (!MemOp.function_stat);	
	*)
	Output.print_endline "\n# of times STP was invoked:";
	Output.print_endline (string_of_int (!Types.stp_count));

	if run_args.arg_branch_coverage then
		begin
			print_endline "Branch coverage:";
			let hashtblAsList =
				Hashtbl.fold
					(fun a b acc -> (a,b) :: acc)
					branches_taken
					[]
			and cmpByLoc ((_,loc1),_) ((_,loc2),_) = Cilutility.compareLoc loc1 loc2
			in
			let sortedList = List.sort cmpByLoc hashtblAsList and
					printPcSet pcSet =
						let counter = ref 0 in
						PcSet.iter
							(fun pc ->
								let str = To_string.annotated_bytes_list pc in
								counter := !counter + 1;
								print_endline ("Condition " ^ (string_of_int !counter) ^ ":");
								print_endline (if str = "" then "[None]" else str);
								print_newline ())
							pcSet
			in
			List.iter
				(fun ((exp,loc), (true_pcSet_ref,false_pcSet_ref)) ->
					print_endline ((To_string.location loc) ^ ", " ^ (To_string.exp exp));
					if not (PcSet.is_empty !true_pcSet_ref) then
						(print_endline "True branch taken under the following conditions:";
						 printPcSet !true_pcSet_ref);
					if not (PcSet.is_empty !false_pcSet_ref) then
						(print_endline "False branch taken under the following conditions:";
						 printPcSet !false_pcSet_ref);
					print_newline ())
				sortedList
		end;

(*
	List.iter
		(fun (pc1,eS1) ->
			List.iter
				(fun (pc2,eS2) ->
					if pc1 < pc2 then( (* Compare each pair only once---that is, don't compare (a,b) and then (b,a) *)
						print_endline "\n====================\n";
						print_endline ("A =\n" ^ (To_string.annotated_bytes_list pc1) ^ "\n\nand B =\n" ^ (To_string.annotated_bytes_list pc2) ^ "\n");
						Printf.printf "A executed %d edges; B executed %d\n\n" (EdgeSet.cardinal eS1) (EdgeSet.cardinal eS2);
						if EdgeSet.equal eS1 eS2
						then print_endline ("A and B cover the same edges.\n")
						else(
							let printDiffs set1 set2 =
								let diffSet = EdgeSet.diff set1 set2 in
								let numDiffs = EdgeSet.cardinal diffSet in
								print_int numDiffs;
								print_newline ();
								if numDiffs < 10 then (print_newline (); Driver.dumpEdges diffSet; print_newline ())
							in
							(print_endline ("Number of edges executed under A but not B:");
							 printDiffs eS1 eS2;
							 print_endline ("Number of edges executed under B but not A:");
							 printDiffs eS2 eS1))))
				!Driver.coverage)
		!Driver.coverage;
*)

	if !Driver.coverage = []
	then(
		Printf.printf "All %d paths had errors.\n"
			(List.length !Driver.abandonedPaths);
		exit 0)
	else
		Printf.printf "%d paths ran to completion successfully; %d had errors.\n"
			(List.length !Driver.coverage) (List.length !Driver.abandonedPaths);

	let (alwaysExecuted,everExecuted) =
		List.fold_left
			(fun (interAcc,unionAcc) (_,eS) -> EdgeSet.inter interAcc eS, EdgeSet.union unionAcc eS)
			(snd (List.hd !Driver.coverage),snd (List.hd !Driver.coverage))
			(List.tl !Driver.coverage)
	in

	print_newline ();
	print_string "In all, ";
	print_int (EdgeSet.cardinal everExecuted);
	print_string " edges were executed, of which ";
	print_int (EdgeSet.cardinal alwaysExecuted);
	print_endline " were always executed.\n";
(*
	(* Print number of non-universal edges executed by each path condition *)
	let coverageWithoutUniversalEdges =
		List.map
			(fun (pc,eS) ->
				 let eS' = EdgeSet.diff eS alwaysExecuted in
				 print_int (EdgeSet.cardinal eS');
				 print_endline " non-universal edges were executed under condition\n";
				 print_endline
					 (let str = (To_string.annotated_bytes_list pc) in
						if str = "" then "<None>\n" else str ^ "\n");
				 (pc,eS'))
			!Driver.coverage;
	in

	(* Gather all proposition from all path conditions, after removing initial 'not's *)
	let rec stripNots bytes = match bytes with
		| Annot_Bytes_Op(OP_LNOT,[(b',_)]) -> stripNots b'
		| _ -> bytes
	in
  let allProps =
		let getPropSetFromPc pc =
			List.fold_left (fun acc bytes -> AnnotatedBytesSet.add (stripNots bytes) acc) AnnotatedBytesSet.empty pc
		in
		List.fold_left (fun acc (pc,_) -> AnnotatedBytesSet.union (getPropSetFromPc pc) acc)
			AnnotatedBytesSet.empty coverageWithoutUniversalEdges
	in

(*	let controlledEdges = ref [] in*)
	(* For each proposition, see what edges are executed if [prop] is
		 true, if [prop] is false, *only if* [prop] is true, and *only if*
		 [prop] is false. *)
	AnnotatedBytesSet.iter
		(fun prop ->
			 (* Partition the set of path conditions into ones with [prop] set
					to true on the one hand, and anything else on the other. (A
					path condition which doesn't mention [prop] goes into the
					'other' category because it includes the case where [prop] is
					false.) *)
			 let rec saysPropIs t_or_f q =
				 match q with
					 | Annot_Bytes_Op(OP_LNOT,[(q',_)]) -> saysPropIs (not t_or_f) q'
					 | _ -> t_or_f && q = prop
			 in
			 let (propTrue,others) =
				 List.partition
					 (fun (pc,_) -> List.exists (saysPropIs true) pc)
					 coverageWithoutUniversalEdges
			 in
			 let (propFalse,propUnmentioned) =
				 List.partition
					 (fun (pc,_) -> List.exists (saysPropIs false) pc)
					 others
			 in
			 let bigUnion =
				 List.fold_left EdgeSet.union EdgeSet.empty
			 and bigInter setList =
				 match setList with
					 | [] -> everExecuted (* An empty intersection leaves you with everything *)
					 | h::t -> List.fold_left EdgeSet.inter h t
							 (* Slight optimization: if the list is not empty, start
									with the first set, rather than with everything. *)
			 in
			 let edgeSetsWithPropTrue = List.map snd propTrue and
					 edgeSetsWithPropFalse = List.map snd propFalse and
					 edgeSetsWithoutProp = List.map snd propUnmentioned in
			 let trueUnion = bigUnion edgeSetsWithPropTrue and
					 trueInter = bigInter edgeSetsWithPropTrue and
					 falseUnion = bigUnion edgeSetsWithPropFalse and
					 falseInter = bigInter edgeSetsWithPropFalse and
					 unmentionedUnion = bigUnion edgeSetsWithoutProp and
					 unmentionedInter = bigInter edgeSetsWithoutProp in
			 let pImpliesE = EdgeSet.inter trueInter unmentionedInter and
					 notPImpliesE = EdgeSet.inter falseInter unmentionedInter and
					 pImpliesNotE = EdgeSet.diff falseUnion (EdgeSet.union trueUnion unmentionedUnion) and
					 notPImpliesNotE = EdgeSet.diff trueUnion (EdgeSet.union falseUnion unmentionedUnion) in
			 let vennSplit set1 set2 =
				 let theIntersection = EdgeSet.inter set1 set2 in
				 (EdgeSet.diff set1 theIntersection, theIntersection, EdgeSet.diff set2 theIntersection)
			 in
			 let (pImplE,pIffE,notPImplNotE) = vennSplit pImpliesE notPImpliesNotE and
					 (notPImplE,notPIffE,pImplNotE) = vennSplit notPImpliesE pImpliesNotE in
(*			 controlledEdges := (prop,notPImpliesNotE,pImpliesNotE) :: !controlledEdges;*)
			 print_endline ("For the proposition\nP = " ^ (To_string.annotated_bytes prop));
			 let printEdgeSet messageFormat edgeSet =
				 if not (EdgeSet.is_empty edgeSet) then(
					 Printf.printf messageFormat (EdgeSet.cardinal edgeSet);
					 Driver.dumpEdges edgeSet;
					 print_newline ())
			 in
			 printEdgeSet "these %d edges are executed iff P:\n\n" pIffE;
			 printEdgeSet "and these %d edges are executed iff (not P):\n\n" notPIffE;
			 if not (List.for_all EdgeSet.is_empty [pImplE;notPImplNotE;notPImplE;pImplNotE])
			 then(
				 print_string "In addition, ";
				 printEdgeSet "these %d edges are executed if P:\n\n" pImplE;
				 printEdgeSet "these %d edges are executed only if P:\n\n" notPImplNotE;
				 printEdgeSet "these %d edges are executed if (not P):\n\n" notPImplE;
				 printEdgeSet "these %d edges are executed only if (not P):\n\n" pImplNotE))
		allProps;
*)
	let greedySetCover coverageList =
		let rec helper acc cvrgList remaining =
			if EdgeSet.is_empty remaining then acc
			else(
				match cvrgList with
					| [] -> failwith "Impossible to cover universe."
					| h::t -> let nextPick = ref h and
								score x = EdgeSet.cardinal (EdgeSet.inter (snd x) remaining) in
						let nextPickScore = ref (score h) in
						List.iter
							(fun s -> let sScore = score s in
							 if sScore > !nextPickScore (*|| (sScore > 1 && Random.bool ())*)
							 then (nextPickScore := sScore; nextPick := s))
							cvrgList;
						Printf.printf "Covering %d new edges\n" !nextPickScore;
						helper (fst !nextPick::acc)
							(List.filter ((!=) !nextPick) cvrgList)
							(EdgeSet.diff remaining (snd !nextPick)))
		in helper [] coverageList (EdgeSet.diff everExecuted alwaysExecuted)
	in
	let coveringSet = greedySetCover !Driver.coverage in
	if coveringSet = [] then print_endline "No constraints: any run covers all edges"
	else(
		print_endline "Here is a set of configurations which covers all the edges:";
		List.iter
			(fun pc -> print_endline (To_string.annotated_bytes_list pc ^ "\n"))
			coveringSet);

(*
	List.iter
		(fun (prop1,true1,false1) ->
			 List.iter
				 (fun (prop2,true2,false2) ->
						if prop1 < prop2 then(
							let sameTrues = EdgeSet.inter true1 true2 in
							if not (EdgeSet.is_empty sameTrues)
							then(
								Printf.printf "%s\n\nand\n%s\n\nmust both be true if any of these edges were executed:\n"
									(To_string.annotated_bytes prop1) (To_string.annotated_bytes prop2);
								Driver.dumpEdges sameTrues);
							let sameFalses = EdgeSet.inter false1 false2 in
							if not (EdgeSet.is_empty sameFalses)
							then(
								Printf.printf "%s\n\nand\n%s\n\nmust both be false any of these edges were executed:\n"
									(To_string.annotated_bytes prop1) (To_string.annotated_bytes prop2);
								Driver.dumpEdges sameFalses)))
				 !controlledEdges)
		!controlledEdges;
*)
(*	List.iter                                                                                           *)
(*		(fun (pc,eS) ->                                                                                   *)
(*			print_endline "The following edges were executed only under condition";                         *)
(*			print_endline                                                                                   *)
(*				((let str = (To_string.annotated_bytes_list pc) in if str = "" then "<None>" else str)^"\n"); *)
(*			Driver.dumpEdges (List.fold_left (fun acc (_,eS') -> EdgeSet.diff acc eS') eS !Driver.coverage);*)
(*			print_endline "\nand these non-universal edges were executed:";                                 *)
(*			Driver.dumpEdges (EdgeSet.diff eS alwaysExecuted))                                              *)
(*		!Driver.coverage;                                                                                 *)
(*	print_endline "Always executed";*)
(*	Output.dumpEdges alwaysExecuted;*)
(*	print_endline "Edges executed at least once but not always:";*)
(*	Driver.dumpEdges (EdgeSet.diff everExecuted alwaysExecuted); *)
	
(*	List.iter                                                                                          *)
(*		(fun (pc,edgeSet) -> print_endline (To_string.annotated_bytes_list pc); Driver.dumpEdges edgeSet)*)
(*		!Driver.coverage;                                                                                *)

  Output.print_endline "Finished.";
	()
;;


let feature : featureDescr = 
  { fd_name = "execute";              
    fd_enabled = ref false;
    fd_description = "(symbolic) executor for C";
		fd_extraopt = [
			
			(**
					Running options
			 *)
			(* Regression test output *)
			("--regression",
			Arg.Unit (fun () -> Executeargs.run_args.arg_run_regression <- true),
			" Output for regression test \n");

			(**
					Printing options
			 *)
			(* TODO: for each msg type, a --print and --noprint option*)
			(* STP *)
			("--printSTP",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_stp <- true),
			" Print STP programs \n");
			(* Assignment in the form lval = rval *)
			("--printAssign",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_assign <- true),
			" Print assignments (from rval to lval) \n");
			(* Assignment in the form lval = rval *)
			("--printFunctionCall",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_func <- true),
			" Print function calls \n");
			(* Print the guard of an if statement *)
			("--printIf",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_guard <- true),
			" Print the guard of an if statement \n");
			(* No Printing *)
			("--printNothing",
			Arg.Unit (fun () -> 
				Executeargs.print_args.arg_print_reg <- false;
				Executeargs.print_args.arg_print_ifstmt <- false;
				Executeargs.print_args.arg_print_misc <- false;
				Executeargs.print_args.arg_print_stmt <- false;
				Executeargs.print_args.arg_print_func <- false;
				Executeargs.print_args.arg_print_assign <- false;
				()
			),
			" Print nothing \n");
			(*  *)
			("--printCharAsInt",
			Arg.Unit (fun () -> Executeargs.print_args.arg_print_char_as_int <- true),
			" Print char as int \n");
			(** 
					Argvs
			 *)
			("--arg",
			Arg.String (fun argv -> Executeargs.run_args.arg_cmdline_argvs <- Executeargs.run_args.arg_cmdline_argvs @ [argv]),
			"<argv> Run with command line arguments <argvs>");	

		  ("--symbolicExternFns",
			 Arg.Unit (fun () -> Executeargs.run_args.arg_symbolic_extern_fns <- true),
			 " Return a fresh symbolic value (instead of dying) upon encountering an undefined function. This effectively assumes that undefined functions are nondeterministic and side-effect-free, which is usually not true. (Note that ending program execution is a side-effect that will be ignored.)");

			("--branchCoverage",
			 Arg.Unit (fun () -> run_args.arg_branch_coverage <- true),
			 " Track branch coverage")
		];
		fd_post_check = true;
    fd_doit = doExecute
  } 
	;;
