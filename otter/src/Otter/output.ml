open Executeargs

(*
let do_print_flag = ref true
let set_print flag = do_print_flag := flag
let do_print () = !do_print_flag
let toggle_print () = do_print_flag := not (!do_print_flag)
*)

let last_time = ref 0.0
let cur_time_elapsed = ref 0.0
let time_elapsed () =
	let cur_time = Sys.time () in
	let time = cur_time -. (!last_time) in
		last_time := cur_time;
		cur_time_elapsed := time;
		time


let cur_loc = ref Cil.locUnknown
let set_cur_loc loc = cur_loc := loc

let runningJobId = ref 0
let runningJobDepth = ref 0
let jidCounter = ref 0

let pid = ref 0
let set_pid id = pid := id

(*let internalIndent = ref 0*)

let print_loc loc = 
  if loc==Cil.locUnknown then "" else
  loc.Cil.file^":"^(string_of_int loc.Cil.line)^" pid:"^(string_of_int !pid)^" : "

let getIndent () = 
	Format.sprintf "[%d,%d] %s" !runningJobId !runningJobDepth (print_loc (!cur_loc))

	(*
  let rec f x = if x<=0 then "" else "    "^(f (x-1)) in
    (f (!internalIndent))^"~ "
  *)
	
	
let insertIndent str = 
	let rec impl str = 
		if String.length str = 0 then ""
		else if String.contains str '\n' then
	  	let i = String.index str '\n' in
	  	let s1 = String.sub str 0 i in
	  	let s2 = String.sub str (i+1) ((String.length str) - i - 1) in
	    	(getIndent())^s1^"\n"^(impl s2)
    else (getIndent())^str
	in
		impl str

(*
let increase () =
	internalIndent := (!internalIndent)+1


let decrease () =
	internalIndent := (!internalIndent)-1

*)

type msg_type = 
	| MSG_REG
	| MSG_STMT
	| MSG_RVAL
	| MSG_LVAL
	| MSG_ASSIGN
	| MSG_FUNC
	| MSG_STP
	| MSG_GUARD
	| MSG_IFSTMT
	| MSG_MISC
	| MSG_DEBUG
	| MSG_MUSTPRINT


let current_msg_type = ref MSG_REG
let set_mode msg_type = current_msg_type := msg_type
let get_mode () = !current_msg_type 

let need_print msg_type =
	if Executeargs.print_args.arg_print_nothing then false else
	match msg_type with
	| MSG_REG 		-> Executeargs.print_args.arg_print_reg
	| MSG_STMT		-> Executeargs.print_args.arg_print_stmt
	| MSG_RVAL		-> Executeargs.print_args.arg_print_rval
	| MSG_LVAL		-> Executeargs.print_args.arg_print_lval
	| MSG_ASSIGN	-> Executeargs.print_args.arg_print_assign
	| MSG_FUNC		-> Executeargs.print_args.arg_print_func
	| MSG_STP			-> Executeargs.print_args.arg_print_stp
	| MSG_GUARD		-> Executeargs.print_args.arg_print_guard
	| MSG_IFSTMT	-> Executeargs.print_args.arg_print_ifstmt
	| MSG_MISC		-> Executeargs.print_args.arg_print_misc
	| MSG_DEBUG		-> Executeargs.print_args.arg_print_debug
	| MSG_MUSTPRINT -> Executeargs.print_args.arg_print_mustprint


let fprintf ff format =
	if (need_print (!current_msg_type)) then
		Format.fprintf ff format
	else
		Format.ifprintf ff format


let std_alwaysflush_formatter = 
  Format.make_formatter 
    (fun  str pos len -> output stdout str pos len; flush stdout) 
    (fun () -> ())

let printf format = fprintf std_alwaysflush_formatter format 
let mprintf format = Format.fprintf std_alwaysflush_formatter format 


let banner_buffer = Buffer.create 100
let banner_out = Buffer.add_substring banner_buffer
let banner_flush () = 
  let s = Buffer.contents banner_buffer in
    Buffer.reset banner_buffer;
    let ss = Str.split (Str.regexp "\n") s in
    let max = List.fold_left (fun x s -> let l=String.length s in if l>x then l else x) 0 ss in
      mprintf "\n%s\n" (String.make (max) '{');
      List.iter (mprintf "%s\n") ss;
      mprintf "%s\n\n" (String.make (max) '}')


let banner_formatter = Format.make_formatter banner_out banner_flush
let banner_printf level format = 
  if level == 0 then Format.ifprintf banner_formatter format
  else Format.fprintf banner_formatter format

let print_string str = 
	if (need_print (!current_msg_type)) then
		(
		Pervasives.print_string (insertIndent str);
		Pervasives.flush_all ()
		)
	else
		()
	

let print_endline str = 
	if str <> "" then print_string (str^"\n")
	

let print_newline () =
	print_string "\n"
	
