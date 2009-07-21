(* 
 * Command line arguments
 *)

type print_args =
	{
		mutable arg_print_reg : bool;
		mutable arg_print_assign : bool;
		mutable arg_print_rval : bool;
		mutable arg_print_lval : bool;
		mutable arg_print_guard : bool;
		mutable arg_print_no_escaped_string : bool;
		mutable arg_print_callstack : bool;
		mutable arg_print_stp : bool;
		mutable arg_print_stmt : bool;
		mutable arg_print_func : bool;
		mutable arg_print_ifstmt : bool;
		mutable arg_print_misc : bool;
		mutable arg_print_debug : bool;
		mutable arg_print_mustprint : bool;
		mutable arg_print_char_as_int : bool;
		mutable arg_print_nothing : bool; (* If true, overrides all other flags *)
	}	;;
(*let print_args =
	{
		arg_print_reg = false;
		arg_print_assign = false;
		arg_print_rval = false;
		arg_print_lval = false;
		arg_print_guard = false;
		arg_print_stp = false;
		arg_print_stmt = false;
		arg_print_func = false;
		arg_print_ifstmt = false;
		arg_print_misc = false;
		arg_print_debug = false;
		arg_print_mustprint = true;
		arg_print_char_as_int = false;
	}	;;*)
let print_args =
	{
		arg_print_reg = true;
		arg_print_assign = false;
		arg_print_rval = false;
		arg_print_lval = false;
		arg_print_guard = false;
		arg_print_no_escaped_string = false;
		arg_print_callstack = false;
		arg_print_stp = false;
		arg_print_stmt = true;
		arg_print_func = true;
		arg_print_ifstmt = false;
		arg_print_misc = true;
		arg_print_debug = false;
		arg_print_mustprint = true;
		arg_print_char_as_int = false;
		arg_print_nothing = false;
	}	;;
type run_args = 
	{
		mutable arg_run_regression : bool;
		mutable arg_cmdline_argvs : string list;
(*		mutable arg_symbolic_extern_fns : bool;*)
		mutable arg_cond_coverage : bool;
		mutable arg_edge_coverage : bool;
		mutable arg_stmt_coverage : bool;
		mutable arg_line_coverage : bool;
		mutable arg_total_lines : int;
		mutable arg_total_stmts : int;
		mutable arg_total_edges : int;
		mutable arg_total_conds : int;
		mutable arg_fns : Types.StringSet.t;
		mutable arg_timeout : int;
		(** How many seconds to allow the executor to run. *)
		mutable arg_merge_paths : bool;
		mutable arg_marshal_file : string;
		mutable arg_calculate_dependencies : bool;
		mutable arg_list_executable_lines : bool;
		mutable arg_list_executable_edges : bool;
		mutable arg_list_executable_conds : bool;
	};;

let run_args = 
	{
		arg_run_regression = false;
		arg_cmdline_argvs = [];
(*		arg_symbolic_extern_fns = false;*)
		arg_cond_coverage = false;
		arg_edge_coverage = false;
		arg_stmt_coverage = false;
		arg_line_coverage = false;
		arg_total_lines = -1;
		arg_total_stmts = -1;
		arg_total_edges = -1;
		arg_total_conds = -1;
		arg_fns = Types.StringSet.empty;
		arg_timeout = 0;
		arg_merge_paths = false;
		arg_marshal_file = ""; (* File to which to marshal coverage information *)
		arg_calculate_dependencies = false;
		arg_list_executable_lines = false;
		arg_list_executable_edges = false;
		arg_list_executable_conds = false;
	} ;;

let readCovStatsFromFile filename =
	let inChan = open_in filename in
	try
		while true do
			run_args.arg_fns <- Types.StringSet.add (input_line inChan) run_args.arg_fns
		done
	with End_of_file -> close_in inChan
