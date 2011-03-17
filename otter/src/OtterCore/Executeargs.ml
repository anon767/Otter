open OcamlUtilities
open OtterBytes

(*
 * Command line arguments
 *)

let arg_failfast = ref false
let arg_bounds_checking = ref true
let arg_cfg_pruning = ref false
let arg_init_malloc_zero = ref false
let arg_init_local_zero = ref false
let arg_noinit_unreachable_globals = ref false
let arg_examfn = ref "" (* none *)

let arg_print_callstack = ref false

let arg_cond_coverage = ref false
let arg_edge_coverage = ref false
let arg_block_coverage = ref false
let arg_line_coverage = ref false
let arg_path_coverage = ref false
let arg_list_lines = ref false
let arg_list_blocks = ref false
let arg_list_edges = ref false
let arg_list_conds = ref false
let arg_print_complement_coverage = ref false
let arg_print_covering_sets = ref false
let arg_print_stmtInfo_locs = ref false (** Option to print the location of {!State.stmtInfo}. *)
let arg_marshal_file = ref "" (** File to which to marshal coverage information *)

let arg_random_seed = ref 226

let options = [
	(*
		Running options
	*)
	("--failfast",
		Arg.Set arg_failfast,
		" Abort execution if any path encounters an error\n");

	("--noboundsChecking",
		Arg.Clear arg_bounds_checking,
		" Disable bounds checking on memory accesses\n");

	("--cfgPruning",
		Arg.Set arg_cfg_pruning,
		" Remove unreachable statements from main (e.g., if(0) induced by switch statements) when calculating coverage\n");

    ("--initMallocZero",
        Arg.Set arg_init_malloc_zero,
        " Initialize memory allocated by malloc() to zero."); ("", Arg.Tuple [],
        " (By default, such memory contains undefined values which cause an error if they ever get passed to the SMT solver.)\n");

    ("--initLocalZero",
        Arg.Set arg_init_local_zero,
        " Initialize local variables to zero."); ("", Arg.Tuple [],
        " (By default, such memory contains undefined values which cause an error if they ever get passed to the SMT solver.)\n");

	("--noinitUnreachableGlobals",
		Arg.Set arg_noinit_unreachable_globals,
		" Do NOT initialize unreachable globals\n");

	("--examfn",
		Arg.Set_string arg_examfn,
		"<fname> Function to be examined (default: none) \n");

    ("--random-seed",
        Arg.Set_int arg_random_seed,
        "<seed> Set the random seed (constant by default)");
	(*
		Printing options
	*)
	(* TODO: for each msg type, a --print and --noprint option*)
	(* STP *)
	("--printSTP",
		Arg.Set Output.arg_print_stp,
		" Print STP programs");

	(* Assignment in the form lval = rval *)
	("--printAssign",
		Arg.Set Output.arg_print_assign,
		" Print assignments (from rval to lval)");

	("--printFunctionCall",
		Arg.Set Output.arg_print_func,
		" Print function calls");

	(* Print the guard of an if statement *)
	("--printIf",
		Arg.Set Output.arg_print_guard,
		" Print the guard of an if statement");

	("--printCallStack",
		Arg.Set arg_print_callstack,
		" Print call stack (when branching)");

	(* Sparse printing *)
	("--printLittle",
		Arg.Unit (fun () ->
			Output.arg_print_reg := false;
			Output.arg_print_stmt := false;
			Output.arg_print_func := false;
			Output.arg_print_assign := false;
		),
		" Suppress most output");

	("--printNothing",
		Arg.Unit (fun () -> Output.arg_print_mute := 1),
		" Suppress (pretty much) all output. This trumps all other --print* options");

	("--printCharAsInt",
		Arg.Set BytesPrinter.print_char_as_int,
		" Print char as int");

	("--printStmtLocs",
		Arg.Set arg_print_stmtInfo_locs,
		" Print file and line number for statements, in addition to function name an id number, for block and edge coverage\n");

	(*
		Coverage
	*)
	("--condCov",
		Arg.Set arg_cond_coverage,
		" Track condition coverage");
	("--edgeCov",
		Arg.Set arg_edge_coverage,
		" Track edge coverage");
	("--blockCov",
		Arg.Set arg_block_coverage,
		" Track statement coverage");
	("--lineCov",
		Arg.Set arg_line_coverage,
		" Track line coverage");
	("--pathCov",
		Arg.Set arg_path_coverage,
		" Track path coverage");
	("--listAllLines",
		Arg.Set arg_list_lines,
		" Before execution, print out all of the lines in the program.");
	("--listAllBlocks",
		Arg.Set arg_list_blocks,
		" Before execution, print out all of the basic blocks in the program.");
	("--listAllEdges",
		Arg.Set arg_list_edges,
		" Before execution, print out all of the intraprodecural edges in the program.");
	("--listAllConds",
		Arg.Set arg_list_conds,
		" Before execution, print out all of the conditions in the program.\n");
	("--printCoveringSets",
		Arg.Set arg_print_covering_sets,
		" Print covering sets.\n");
	("--printComplementCoverages",
		Arg.Set arg_print_complement_coverage,
		" Print complement coverages (i.e., uncovered entities).\n");


	("--marshalCoverageTo",
		Arg.Set_string arg_marshal_file,
		"<file> Marshal coverage information to <file>.\n");

]

