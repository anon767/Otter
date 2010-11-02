
let arg_print_mute = ref 0
let arg_print_reg = ref true
let arg_print_stmt = ref true
let arg_print_assign = ref false
let arg_print_func = ref true
let arg_print_stp = ref false
let arg_print_guard = ref false
let arg_print_debug = ref false
let arg_print_mustprint = ref true


let is_console () =
    (* detecting terminal capabilities is serious black magic; this will at least catch Emacs' shell mode *)
    Unix.isatty Unix.stdin && Unix.isatty Unix.stdout && (try Unix.getenv "TERM" <> "dumb" with Not_found -> false)


let get_console_size =
    let default_size = (24, 80) in
    let previous_size = ref default_size in
    fun () ->
        if is_console () then
            (* if on a terminal, use xterm escape sequence to query for column size *)
            let attr = Unix.tcgetattr Unix.stdin in
            try
                (* turn off line buffering, echoing, set the minimum characters for read to return and the
                   read timeout on stdin and flush the input *)
                Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
                    { attr with Unix.c_icanon = false; Unix.c_echo = false; Unix.c_vmin = 0; Unix.c_vtime = 1 };
                Unix.tcflush Unix.stdin Unix.TCIFLUSH;
                (* send the "query screen size" escape sequence *)
                ignore (Unix.write Unix.stdout "\027[18t" 0 5);
                (* read the result *)
                let s = String.create 16 in
                ignore (Unix.read Unix.stdin s 0 16);
                (* restore the state of stdin *)
                Unix.tcsetattr Unix.stdin Unix.TCSANOW attr;
                (* parse the result *)
                Scanf.sscanf s "\027[8;%d;%dt" begin fun h w ->
                    previous_size := (h, w);
                    (h, w)
                end
            with e ->
                (* whatever exceptions arise, restore the terminal and flush it *)
                Unix.tcsetattr Unix.stdin Unix.TCSANOW attr;
                Unix.tcflush Unix.stdin Unix.TCIFLUSH;
                match e with
                    | Unix.Unix_error _ | Scanf.Scan_failure _ ->
                        (* if the exception was from here, return the an old result which may still be right *)
                        !previous_size
                    | e ->
                        raise e
        else
            default_size


class virtual t =
	object (self : 'self)
		val virtual formatter : Format.formatter

		method printf : 'a . ('a, Format.formatter, unit) format -> 'a =
			Format.fprintf formatter

		method kprintf : 'a 'b . ('self -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b = fun k ->
			Format.kfprintf (fun _ -> k self) formatter

		method flush =
			Format.pp_print_flush formatter ()
	end

class plain =
	object
		inherit t
		val formatter =
			(* flush after every write *)
			Format.make_formatter (fun str pos len -> output stdout str pos len; flush stdout) (fun () -> ())
	end

class colored color =
	object
		inherit t
		val formatter =
                (* flush after every write *)
                Format.make_formatter begin fun str pos len ->
                    if is_console () then begin
                        output_string stdout "\027";
                        output_string stdout color;
                        output stdout str pos len;
                        output_string stdout "\027[0m"
                    end else begin
                        output stdout str pos len
                    end;
                    flush stdout
                end (fun () -> ())
	end

class labeled label =
	object
		inherit t
		val formatter =
			(* flush after every line, prefixing each line with a label *)
			let _, width = get_console_size () in
			let buffer = Buffer.create width in
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
			Format.pp_set_margin formatter (width - String.length label);
			formatter
	end

let formatter = ref (new plain)

let set_formatter ff =
	!formatter#flush;
	formatter := ff

let () = at_exit (fun () -> !formatter#flush)

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
	if !arg_print_mute > 0 then false else
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

let must_printf format =
    let old_mode = get_mode () in
    let old_formatter = !formatter in
    set_mode MSG_MUSTPRINT;
    set_formatter (new colored "[0;31m");
    kprintf (fun _ -> set_formatter old_formatter; set_mode old_mode) format

let debug_printf format =
    let old_mode = get_mode () in
    let old_formatter = !formatter in
    set_mode MSG_DEBUG;
    set_formatter (new colored "[0;36m");
    kprintf (fun _ -> set_formatter old_formatter; set_mode old_mode) format


let mprint_formatter =
  Format.make_formatter
    (fun  str pos len -> output stdout str pos len; flush stdout)
    (fun () -> ())
let mprintf format = Format.fprintf mprint_formatter format

