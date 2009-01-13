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
		mutable arg_print_stp : bool;
		mutable arg_print_stmt : bool;
		mutable arg_print_func : bool;
		mutable arg_print_ifstmt : bool;
		mutable arg_print_misc : bool;
		mutable arg_print_debug : bool;
		mutable arg_print_mustprint : bool;
		mutable arg_print_char_as_int : bool;
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
		arg_print_stp = false;
		arg_print_stmt = true;
		arg_print_func = true;
		arg_print_ifstmt = false;
		arg_print_misc = true;
		arg_print_debug = false;
		arg_print_mustprint = true;
		arg_print_char_as_int = false;
	}	;;
type run_args = 
	{
		mutable arg_run_regression : bool;
		mutable arg_cmdline_argvs : string list;
		mutable arg_symbolic_extern_fns : bool;
		mutable arg_branch_coverage : bool;
	};;

let run_args = 
	{
		arg_run_regression = false;
		arg_cmdline_argvs = [];
		arg_symbolic_extern_fns = false;
		arg_branch_coverage = false;
	} ;;
