open Cil
open Bytes
open Types

(*
(* If we can ensure no collisions (or if we can disambiguate collisions),
	hashing would be a great option (assuming we don't need to retrive the
	paths themselves). It would be faster and it would result in much
	smaller files, because we could just print out the hash value. *)
let my_compress str =
	let hashVal = string_of_int (Hashtbl.hash str) in
	hashVal
;;
*)

let my_compress str =
	let str' = Stats.time "bzip" (fun () -> Bz2.compress str 0 (String.length str)) () in
	let res = Stats.time "uuencode" Uuencode.uuencode str' in
	res
(*
let my_uncompress str =
	let str' = Stats.time "uudecode" Uuencode.uudecode str in
	let res = Stats.time "unbzip" (fun () -> Bz2.uncompress str' 0 (String.length str')) () in
	res
*)

(*
(* For some reason, this didn't work. The zipped strings seemed to
	have gotten truncated somehow, preventing a complete unzipping
	and making me worry that there might be collisions. *)
let gzip str =
	let from_gzip,to_gzip = Unix.open_process "gzip" in
	output_string to_gzip str;
	Unix.close (Unix.descr_of_out_channel to_gzip);
	let buf = Buffer.create 100 in
	try
		while true do
			(* I think I can't use input_line here because it strips the final '\n' *)
			Buffer.add_char buf (input_char from_gzip);
		done;
		assert false (* Unreachable *)
	with End_of_file ->
		assert (Unix.close_process (from_gzip,to_gzip) = Unix.WEXITED 0);
		Buffer.contents buf
;;

let uuencode str =
	let from_uuencode,to_uuencode = Unix.open_process "uuencode /dev/stdout" in
	output_string to_uuencode str;
	Unix.close (Unix.descr_of_out_channel to_uuencode);
	let buf = Buffer.create 100 in
	try
		(* uuencode doesn't use lowercase letters, so I'll mark newlines with 'x' *)
		while true do
			Buffer.add_string buf (input_line from_uuencode);
			Buffer.add_char buf 'x';
		done;
		assert false (* Unreachable *)
	with End_of_file ->
		assert (Unix.close_process (from_uuencode,to_uuencode) = Unix.WEXITED 0);
		Buffer.contents buf
;;

let gzip_uuencode str =
	uuencode (gzip str)
;;
*)

let string_of_path (path : stmtInfo list) : string =
	let buf = Buffer.create 10000 in
	List.iter
		(fun si ->
			 Buffer.add_string buf (To_string.stmtInfo si);
			 Buffer.add_char buf ',')
		path;
	Buffer.contents buf
;;

let rec eliminate_untracked apc apct =
	match apc,apct with 
		| [],[]->([],[])
		| apch::apct,apcth::apctt -> 
				let (apct1,apct2) = eliminate_untracked apct apctt in
				if apcth then (apch::apct1,apct2) else  (apct1,apch::apct2)
		| _ -> failwith "Impossible: path_condition and path_condition_tracked must be of equal length"
;;

let getSatisfyingAssignment state (bytesToVars:(bytes * varinfo) list) =
  let pc_all = state.path_condition in
  let (pc_branch,pc_assume) = eliminate_untracked (state.path_condition) (state.path_condition_tracked) in

	let mentionedSymbols = Stp.allSymbolsInList pc_branch in
	(* STP seems to still have some leaks in it, which caused
	   trouble here. Invoking STP in a child process works around
	   the problem, because the memory STP uses is freed when the
	   child exits. *)
(*
	let valuesForSymbols = Stp.getValues pc_all (SymbolSet.elements mentionedSymbols) in
*)
	let pipeExit,pipeEntrance = Unix.pipe () in
	flush_all (); (* It's important to flush before forking; otherwise data gets printed out multiple times. *)
	if Unix.fork () = 0 then (
		(* Child process: invoke STP, send result to parent, and exit *)
		Marshal.to_channel (Unix.out_channel_of_descr pipeEntrance) (Stp.getValues pc_all (SymbolSet.elements mentionedSymbols)) [];
		exit 0
	);
	(* Parent process: Read result from child, close pipe, and continue *)
	ignore(Unix.wait()); (* Reap child process *)
	Unix.close pipeEntrance;
	let valuesForSymbols = (Marshal.from_channel (Unix.in_channel_of_descr pipeExit) : (symbol*char) list) in
	Unix.close pipeExit;

	(* Keep track of which symbols we haven't given values to.
		 This would happen if there are untracked symbolic values in the
		 path condition. *)
	let unboundSymbols = ref mentionedSymbols in

	(* Get the value of a symbolic ByteArray *)
	let getVal = function
		| Bytes_ByteArray bytArr ->
				let byteOptArray =
					ImmutableArray.map
						(function
							 | Byte_Symbolic s ->
									 (try
											let valueForS = List.assoc s valuesForSymbols in
											(* Now s is bound *)
											unboundSymbols := SymbolSet.remove s !unboundSymbols;
											Some (make_Byte_Concrete valueForS)
										with Not_found -> None)
							 | _ -> failwith "Impossible: tracked symbolic value must be fully symbolic")
						bytArr
				in
				if ImmutableArray.exists (* Check if any byte is constrained *)
					(function Some _ -> true | _ -> false)
					byteOptArray
				then (
					(* Return a Some with the bytearray, filling in
						 unconstrained bytes with 0. *)
					Some (make_Bytes_ByteArray
									(ImmutableArray.map
										 (function Some b -> b | None -> byte__zero)
										 byteOptArray))
				) else (
					(* Return None for a totally unconstrained value *)
					None
				)
		| _ -> failwith "Impossible: symbolic bytes must be a ByteArray"
	in

	(* Make an association list of the concrete values for the variables in bytesToVars *)
	let satisfyingAssignment =
		List.fold_left
			(fun acc (bytes,varinf) ->
				 match getVal bytes with
					 | None -> acc (* Don't record anything for an unconstrained value *)
					 | Some concreteByteArray ->
							 match bytes_to_constant concreteByteArray varinf.vtype with
								 | CInt64 (n,_,_) -> (* Is it okay to ignore the type? Or might we have to truncate? *)
										 (varinf.vname, n) :: acc
								 | _ -> failwith "Unimplemented: non-integer symbolic value")
			[]
			bytesToVars
	in

	satisfyingAssignment,!unboundSymbols
;;

(* We need a bytesToVars in order to print out the conditions in a
	 readable way.  I think the way we are doing things now, all of the
	 bytesToVars should be the same, so picking an arbitrary one should be
	 fine, but is there a better, more general way to map bytes to
	 variables? *)
let bytesToVars_ref = ref []

(* Map variable names to possible values *)
let varToVals = Hashtbl.create 20

let makeEquality bytes value =
	(* Is intType okay here? *)
	Bytes_Op(OP_EQ,[(bytes,intType);
									(lazy_int_to_bytes value, intType)])

let rec makeOr_aux acc = function
		[] -> acc
	| h::t -> makeOr_aux (Bytes_Op (OP_LOR, [(h,intType);(acc,intType)])) t
let makeOr = function
		[] -> invalid_arg "makeOr called with empty list"
	| h::t -> makeOr_aux h t

let readInValues valuesFile =
	let inChan = open_in valuesFile in
	(* Read in the possible variable values *)
	try
		while true do
			match Str.split (Str.regexp "[\t ]+") (input_line inChan) with
					[] -> failwith "Badly formatted input"
				| var::valList ->
						List.iter
							(fun str -> Hashtbl.add varToVals var (int_of_string str))
							valList
		done
	with End_of_file ->
		close_in inChan

(* This requires bytesToVars_ref to have already been set (by
	 addPathCoverageFromFile) *)
let varNameToBytes str =
	fst (List.find (fun (_,varinfo) -> varinfo.vname = str) !bytesToVars_ref)

let speclist = [
	("--fileWithPossibleValues",
	 Arg.String readInValues,
	 "<filename> File from which to read the possible values for variables
\t\t\tFormat is \"varname val_1 val_2 ... val_k\", with one variable per line,
\t\t\tand the values must all be integers\n");
]
;;

let usageMsg =
	"Usage: guaranteedCoverage [options] coverage-file[...]

You must specify a possibleValues file; it can come before, after, or among the
coverage files.
"

let coverageFiles = ref []

let constraintFor name =
	try
		let varBytes = varNameToBytes name in
		makeOr (List.map (fun n -> makeEquality varBytes n) (Hashtbl.find_all varToVals name))
	with Not_found ->
		(* If the variable was never even set symbolic, its constraint is
			 'true' (which means no constraint at all) . *)
		Bytes_Constant (Cil.CInt64 (1L,IInt,None))

let addPathCoverageFromFile filename =
	let inChan = open_in_bin filename in
	(* Read in the coverage data structure *)
	let jobResults = (Marshal.from_channel inChan : job_result list) in
	close_in inChan;
	if !bytesToVars_ref = [] then (
		bytesToVars_ref := (List.hd jobResults).result_history.bytesToVars;
	);
	(* To constrain the values we get to those in the possible values file, we add
		 disjunctions of equalities to the path condition. *)
	let varNames = Hashtbl.fold (fun name _ set -> StringSet.add name set) varToVals StringSet.empty in
	let valueRestrictions = StringSet.fold (fun name acc -> constraintFor name :: acc) varNames [] in
	List.iter
		(fun { result_state = state ; result_history = hist } ->
			 (* Make sure all bytesToVars are the same *)
			 if hist.bytesToVars <> !bytesToVars_ref then failwith "Not all bytesToVars are equal";
			 let samplePathCondition,unboundSymbols =
				 getSatisfyingAssignment
					 { state with
							 path_condition = valueRestrictions @ state.path_condition;
							 (* None of these additions are 'tracked', i.e. none are from real
									branches in the program. *)
							 path_condition_tracked =
							 List.map (fun _ -> false) valueRestrictions @ state.path_condition_tracked ; }
					 hist.bytesToVars
			 in
			 (* Check to see if we've bound all of the symbols in the path condition *)
			 if not (SymbolSet.is_empty unboundSymbols)
			 then (
				 Format.printf "Warning! The following configuration does not assign values to these symbolic values:";
				 SymbolSet.iter
					 (fun s -> Format.printf " %d" s.symbol_id)
					 unboundSymbols;
				 Format.printf "\n";
			 );
			 Format.printf "Under the condition\n";
			 List.iter (fun (varName,value) -> Format.printf "%s=%Ld\n" varName value) samplePathCondition;
			 Format.printf "these 1 paths are hit\n%s\n\n"
				 (my_compress (string_of_path (List.rev hist.executionPath)));
		)
		jobResults;
		List.iter
			(fun s -> Format.printf "%.2fs running %s\n" (Stats.lookupTime s) s)
			["bzip";"uuencode"];
;;

let main () =
	Output.set_mode Output.MSG_REG;
	initCIL();
	Arg.parse
		(Arg.align speclist)
		(fun filename -> coverageFiles := filename::!coverageFiles) (* Unnamed arguments are treated as coverage files *)
		usageMsg;
	Stats.reset Stats.HardwareIfAvail; (* Enable timing *)
	if Hashtbl.length varToVals = 0
	then (print_endline usageMsg; exit(1));
	List.iter addPathCoverageFromFile !coverageFiles
;;

main ()
