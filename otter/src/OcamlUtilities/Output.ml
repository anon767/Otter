
let arg_print_nothing = ref false
let arg_print_reg = ref true
let arg_print_stmt = ref true
let arg_print_assign = ref false
let arg_print_func = ref true
let arg_print_stp = ref false
let arg_print_guard = ref false
let arg_print_debug = ref false
let arg_print_mustprint = ref true


class virtual t =
	object (self : 'self)
		val virtual formatter : Format.formatter

		method printf : 'a . ('a, Format.formatter, unit) format -> 'a =
			Format.fprintf formatter

		method kprintf : 'a 'b . ('self -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b = fun k ->
			Format.kfprintf (fun _ -> k self) formatter
	end

class plain =
	object
		inherit t
		val formatter =
			(* flush after every write *)
			Format.make_formatter (fun str pos len -> output stdout str pos len; flush stdout) (fun () -> ())
	end

class labeled label =
	object
		inherit t
		val formatter =
			(* flush after every line, prefixing each line with a label *)
			let buffer = Buffer.create 80 in
			let rec labeled_output str pos len =
				let newline_index = 1 + try String.index_from str pos '\n' - pos with Not_found -> len in
				if newline_index <= len then begin
					(* print the label *)
					Pervasives.output_string stdout label;
					(* print the buffer *)
					Buffer.output_buffer stdout buffer;
					Buffer.clear buffer;
					(* print the new string up to the end of the line *)
					Pervasives.output stdout str pos newline_index;
					Pervasives.flush stdout;
					(* recurse on the remainder *)
					labeled_output str (newline_index + pos) (len - newline_index)
				end else begin
					Buffer.add_substring buffer str pos len;
				end
			in
			let formatter = Format.make_formatter labeled_output (fun () -> ()) in
			(* adjust the margin to account for the length of the label *)
			Format.pp_set_margin formatter (Format.pp_get_margin formatter () - (String.length label));
			formatter
	end

let formatter = ref (new plain)

type msg_type = 
	| MSG_REG
	| MSG_STMT
	| MSG_ASSIGN
	| MSG_FUNC
	| MSG_STP
	| MSG_GUARD
	| MSG_DEBUG
	| MSG_MUSTPRINT


let current_msg_type = ref MSG_REG
let set_mode msg_type = current_msg_type := msg_type
let get_mode () = !current_msg_type 

let need_print msg_type =
	if !arg_print_nothing then false else
	match msg_type with
		| MSG_REG 		-> !arg_print_reg
		| MSG_STMT		-> !arg_print_stmt
		| MSG_ASSIGN	-> !arg_print_assign
		| MSG_FUNC		-> !arg_print_func
		| MSG_STP		-> !arg_print_stp
		| MSG_GUARD		-> !arg_print_guard
		| MSG_DEBUG		-> !arg_print_debug
		| MSG_MUSTPRINT -> !arg_print_mustprint


let printf format =
	if (need_print (!current_msg_type)) then
		!formatter#printf format
	else
		Format.ifprintf Format.std_formatter format

let kprintf k format =
	if (need_print (!current_msg_type)) then
		!formatter#kprintf k format
 	else
		FormatPlus.ikfprintf (fun _ -> k !formatter) Format.std_formatter format


let mprint_formatter =
  Format.make_formatter 
    (fun  str pos len -> output stdout str pos len; flush stdout) 
    (fun () -> ())
let mprintf format = Format.fprintf mprint_formatter format


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

