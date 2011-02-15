open OcamlUtilities
open CilUtilities
open OtterCore
open OtterQueue
open OtterReporter
open DataStructures

open BuiltinFunctions
open Expression
open OtterBytes
open Bytes
open BytesUtility
open Job
open State
open Printf

(**/**)
let (>>>) = Interceptor.(>>>)
(**/**)

let printCommands () = 
	printf "\n\nThis debugger works by typed commands.  The following commands are implemented:\n";
	printf "\tstep -- make a step in the execution\n";
	printf "\tnext -- keep stepping until you reach the next C statement in the file\n";
	printf "\thelp -- print these instructions\n";
	printf "\tprint file -- print the contents of the file being executed\n";
    printf "\tprint path condition -- print the current path condition\n";
    printf "\trun -- continue execution to end of file\n";
    printf "\tbreakpoint -- set a breakpoint.  You will be asked for the line number\n";
    printf "\tprint instruction -- print the current instruction\n";
	printf "\tquit -- stop execution and quit the program\n"
	
	
let printFileContents job = 
	let loc = OtterCore.Job.get_loc job in 
	let ic = open_in (loc.Cil.file) in
	let lineno = ref 0 in 
	printf "Printing file %s\n" loc.Cil.file;
	try
		while true do
			let line = input_line ic in
			printf "%d: %s\n" !lineno line;
			lineno := !lineno + 1
		done
	with End_of_file ->
		close_in ic

let printPathCondition job =
	printf "\nPath Condition: \n";

    FormatPlus.ksprintf print_string
        "@.  %t @.\n"

        begin fun ff ->
            if job#state.path_condition = [] then
                Format.pp_print_string ff "true"
            else
                FormatPlus.pp_print_list BytesPrinter.bytes "\nAND\n  " ff job#state.path_condition
        end  
        
let printCurrentInstruction job =
      let instr = OtterCore.Job.get_instruction job in
	  Format.printf "@.Current instruction:@ @[%a@]@." OtterCFG.Instruction.printer instr



let findValue value=
	let findValue' k v l :Cil.varinfo list = 
		if (k.Cil.vname = value)
			then k::l
		else
			l
	in
	findValue'
	  
let printValue value job =
	let f = findValue value in
	let l = VarinfoMap.fold f (List.hd job#state.locals) [] in
		if (List.length l > 0) then 
		let vinfo = List.hd l in
		let _,v,_ = Expression.rval job (Cil.Lval (Cil.Var vinfo, Cil.NoOffset)) [] in
		printf "length of list %d\n" (List.length l);
		(*printf "bytes returned from rval @[%a@]@\n\n" (BytesPrinter.bytes Format.std_formatter v);*)
		printf "Variable %s has value %d in locals" vinfo.Cil.vname (OtterBytes.Bytes.bytes_to_int_auto v );
		printf "type:";
		Pretty.fprint stdout 80 (Cil.printType Cil.defaultCilPrinter () vinfo.Cil.vtype)
	 

	  
let main_loop interceptor queue reporter job =
	let doWork = ref true in
	let currJob = ref job in
	let step = fun job -> fst (interceptor job () Statement.step) in
	let run = ref false in
	let next = ref false in
	let breakpoint = ref 0 in
	printCommands ();
	while (!doWork) do 

		(*Output.set_formatter (new Output.plain);*)
	    if (!run = false && !next = false) then printf "\n\nEnter Command\n";
		let cmd = (
			if !run then "step"
			else if !next then "next"
			else
				read_line ()
		) in
		(match cmd with 
		|"step" -> ( 
			if !run then (
				if !breakpoint > 0 then
					let loc = OtterCore.Job.get_loc !currJob in 
					if (loc.Cil.line) == !breakpoint then run := false
			);
			let jobstate = step !currJob in 
			match jobstate with
			| Job.Active jobr -> currJob := jobr
			| Job.Fork [Active job1; Active job2] ->  
				let pick = ref "default" in
				while (!pick <> "true" && !pick <> "false") do
					printf "Execute true or false job?  Type \"true\" or \"false\"\n"; 
					pick := read_line () ;
				done;
				if !pick = "true" then (printf "job #%d was picked for true\n" job1#path_id; currJob := job1)
				else (printf "job #%d was picked for false\n" job2#path_id; currJob := job2 )
			| Job.Complete completion ->  doWork := false
			| default -> printf "unmatched job\n"; currJob := !currJob
			)
		|"next" -> ( let jobstate = step !currJob in  
			match jobstate with
			| Job.Active jobr -> 
				let loc1 = OtterCore.Job.get_loc !currJob in
				let loc2 = OtterCore.Job.get_loc jobr in
				if (loc1 == loc2) then next := true else next := false;
				currJob := jobr
			| Job.Fork [Active job1; Active job2] ->  
				let pick = ref "default" in
				while (!pick <> "true" && !pick <> "false") do
					printf "Execute true or false job?  Type \"true\" or \"false\"\n"; 
					pick := read_line () ;
				done;
				if !pick = "true" then (printf "job #%d was picked for true\n" job1#path_id; currJob := job1)
				else (printf "job #%d was picked for false\n" job2#path_id; currJob := job2 )
			| Job.Complete completion ->  doWork := false
			| default -> printf "unmatched job\n"; currJob := !currJob				
			)
		(*Cil.dumpStmt Cil.defaultCilPrinter stdout 5 job.stmt;*)
		|"quit" | "exit" -> doWork := false
		| "help" -> printCommands ()
		| "print file" -> printFileContents !currJob
		| "run" -> run := true
		| "breakpoint" -> printf "Which line?\n"; let line = read_int () in
				run := true; breakpoint := line
		| "print path condition" -> printPathCondition !currJob
		| "print instruction" -> printCurrentInstruction !currJob
		| "read value" -> (*doesn't currently work *) printf "Which variable?\n"; let value = read_line () in printValue value job
		| _ -> printf "Invalid Instruction\n"; doWork := true
		);	
		
		Output.myflush ();
	done;
	
	printf "\n\nFinal\n";
	printPathCondition !currJob;
	(queue, reporter)



(*(** Main symbolic execution loop. *)
let main_loop interceptor queue reporter =
    (* set up a checkpoint to rollback to upon SignalException *)
    let checkpoint = ref (queue, reporter) in
    try
        (* compose the interceptor with the core symbolic executor *)
        (* TODO: remove job_queue from interceptors/Statement.step *)
        let step = fun job -> fst (interceptor job () Statement.step) in
        let rec run (queue, reporter) =
            checkpoint := (queue, reporter);
            match queue#get with
                | Some (queue, job) ->
                user_input;
                    let result = step job in
                    let rec process_result (queue, reporter) result =
                        let reporter = reporter#report result in
                        match result with
                            | Job.Active job ->
                                (queue#put job, reporter)
                            | Job.Fork results ->
                                List.fold_left process_result (queue, reporter) results
                            | Job.Complete completion ->
                                (queue, reporter)
                            | Job.Paused _ ->
                                invalid_arg "unexpected Job.Paused"
                    in
                    let queue, reporter = process_result (queue, reporter) result in
                    if reporter#should_continue then
                        run (queue, reporter)
                    else
                        (queue, reporter)
                | None ->
                    (queue, reporter)
        in
        run (queue, reporter)
    with State.SignalException s ->
        (* if we got a signal, stop and return the checkpoint results *)
        Output.set_mode Output.MSG_MUSTPRINT;
        Output.printf "%s@\n" s;
        !checkpoint
        *)

(* This is "Otter" in OtterBenchmark *)
let run ?(random_seed=(!Executeargs.arg_random_seed))
        ?(interceptor=Interceptor.identity_interceptor)
        ?(queue=Queue.get_default ())
        reporter
        job =
	Random.init random_seed;
    let queue = queue#put job in
    main_loop interceptor queue reporter job

(** {1 Precomposed drivers for common use cases} *)

(** Driver using the core symbolic executor only. *)
let run_core reporter job =
    run reporter job

(** As with {!run}, using the core symbolic executor and core built-in functions. *)
let run_basic reporter job =
    let interceptor =
        Interceptor.set_output_formatter_interceptor
        >>> BuiltinFunctions.interceptor
    in
    run ~interceptor reporter job


let foo_interceptor job job_queue interceptor =
	interceptor job job_queue	


(** As with {!run}, using the core symbolic executor, core and libc built-in functions. *)
let run_with_libc reporter job =
    let interceptor =
        Interceptor.set_output_formatter_interceptor
        >>> BuiltinFunctions.libc_interceptor
        >>> BuiltinFunctions.interceptor
        >>> foo_interceptor
    in
    run ~interceptor reporter job



