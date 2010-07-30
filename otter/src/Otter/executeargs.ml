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
		mutable arg_print_stmt_locs : bool;
	}
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
	}	*)
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
		arg_print_stmt_locs = false;
	}
type run_args =
	{
		mutable arg_cmdline_argvs : string list;
(*		mutable arg_symbolic_extern_fns : bool;*)
		mutable arg_cond_coverage : bool;
		mutable arg_edge_coverage : bool;
		mutable arg_block_coverage : bool;
		mutable arg_line_coverage : bool;
		mutable arg_path_coverage : bool;
		mutable arg_num_lines : int;
		mutable arg_num_blocks : int;
		mutable arg_num_edges : int;
		mutable arg_num_conds : int;
		mutable arg_fns : Utility.StringSet.t;
		mutable arg_yaml : string;
		mutable arg_entryfn : string;
		mutable arg_assertfn : string;
		mutable arg_examfn : string;
		mutable arg_timeout : int;
		(** How many seconds to allow the executor to run. *)
		mutable arg_marshal_file : string;
		mutable arg_calculate_dependencies : bool;
		mutable arg_noinit_unreachable_globals : bool;
		mutable arg_use_conditional_exceptions : bool;
		mutable arg_simplify_path_condition : bool;
		mutable arg_list_lines : bool;
		mutable arg_list_blocks : bool;
		mutable arg_list_edges : bool;
		mutable arg_list_conds : bool;
		mutable arg_opt_hash_consing : bool;
		mutable arg_opt_bytes_eval_cache : bool;
		mutable arg_opt_stpbv_cache : bool;
		mutable arg_failfast : bool;
		mutable arg_init_malloc_zero : bool;
		mutable arg_bounds_checking : bool;
		mutable arg_cfg_pruning : bool;
		mutable arg_callchain_backward : bool;
	}

let run_args =
	{
		arg_cmdline_argvs = [];
(*		arg_symbolic_extern_fns = false;*)
		arg_cond_coverage = false;
		arg_edge_coverage = false;
		arg_block_coverage = false;
		arg_line_coverage = false;
		arg_path_coverage = false;
		arg_num_lines = -1;
		arg_num_blocks = -1;
		arg_num_edges = -1;
		arg_num_conds = -1;
		arg_fns = Utility.StringSet.empty;
		arg_yaml = "";
		arg_entryfn = "main";
		arg_assertfn = "__ASSERT";
		arg_examfn = ""; (* none *)
		arg_timeout = 0;
		arg_marshal_file = ""; (* File to which to marshal coverage information *)
		arg_calculate_dependencies = false;
		arg_noinit_unreachable_globals = false;
		arg_use_conditional_exceptions = false;
		arg_simplify_path_condition = false;
		arg_list_lines = false;
		arg_list_blocks = false;
		arg_list_edges = false;
		arg_list_conds = false;
		arg_opt_hash_consing = false;  (* This module depends on Utility!!! *)
		arg_opt_bytes_eval_cache = false;
		arg_opt_stpbv_cache = false;
		arg_failfast = false;
		arg_init_malloc_zero = false;
		arg_bounds_checking = true;
		arg_cfg_pruning = false;
		arg_callchain_backward = false;
	}


let readCovStatsFromFile filename =
	let inChan = open_in filename in
	try
		while true do
			run_args.arg_fns <- Utility.StringSet.add (input_line inChan) run_args.arg_fns
		done
	with End_of_file -> close_in inChan
  
let readYamlFromFile filename =
	let inChan = open_in filename in
	let len = in_channel_length inChan in
	let str = String.create len in
	really_input inChan str 0 len;
	run_args.arg_yaml <- str;
	close_in inChan


