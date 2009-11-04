(* TODO: perhaps introduce laziness? *) 
open Format
open Cil
open Types

let strlen = 1000;;
let donotprint() = not (Output.need_print (!Output.current_msg_type));;

(** Print out a bytearray as though it were a string, stopping at the
		first (concrete) null byte. *)
let bytestring arr =
	let byte b = 
  match b with
	| Byte_Concrete (c) -> 
        if Char.code c >=32 && Char.code c <= 126 then sprintf "%c" c
				else if c = '\n' then "\n"
        else sprintf "/%d" (Char.code c)
	| Byte_Symbolic (s) -> sprintf "\\%d" (s.symbol_id)
	| Byte_Bytes (b,i) ->  "."
	in
	let rec helper n result =
		if n = ImmutableArray.length arr
		then result
		else (
			match byte (ImmutableArray.get arr n) with
				| "/0" -> result
				| s -> helper (succ n) (result ^ s)
		)
	in
	helper 0 ""
;;

let location loc = 
	loc.file^":"^(string_of_int loc.line)
	;;

let varinfo v =
	(v.vname)^"@"^(location v.vdecl)
	;;

(* TODO: avoid rec *)
let rec

callstack s =
    match s with 
    | [] -> "\n"
    | context::tail -> (
        let instr_p i = 
            "\t"^(location (get_instrLoc i))
        in
        match context with
        | Runtime -> "\t(END)"
        | Source (_,_,i,_) -> instr_p i
        | NoReturn (i) -> instr_p i
    )^"\n"^(callstack tail)

and

stmt s =
  if donotprint() then "" else
	match s.skind with
		| If(e,_,_,_) -> "IF ("^(exp e)^")"
		| Instr(_) -> ""
		| Loop(_,_,_,_) -> "LOOP:"
		| Block(_) -> "BLOCK:"
		| _ ->	Pretty.sprint strlen (Cil.d_stmt () s)
and

instr i = 
  if donotprint() then "" else
	try
	Pretty.sprint strlen (Cil.d_instr () i)
	with
		| Errormsg.Error -> ""

and

typ t = Pretty.sprint strlen (Cil.d_type () t)

and

lval l = Pretty.sprint strlen (Cil.d_lval () l)

and

exp e = 
  if donotprint() then "" else
	let str = Pretty.sprint strlen (Cil.d_exp () e) in
	match e with
		| StartOf (_) -> "StartOf("^str^")"
		| _ -> str

and

fundec f = 
  if donotprint() then "" else
  f.svar.vname^" : "^(typ f.svar.vtype)

and

char_ff ff c = 
	if Executeargs.print_args.Executeargs.arg_print_char_as_int ||
		Char.code c < 32 || Char.code c > 126
	then
		fprintf ff "/%02X" (Char.code c)
	else if c = '/'
	then fprintf ff "//"
	else
		fprintf ff "%c" (c)

(*
char_ff ff c = fprintf ff "/%02X" (Char.code c)
*)
(*
char_ff ff c = fprintf ff "%s" 
	(let code = Char.code c in
		if (code>=32 && code<=126) || (code>=128 && code<=254) then Printf.sprintf "%c" c
		else Printf.sprintf "/%02X" code
	)
*)
and

symbol_ff ff s = fprintf ff "\\%d" (*if s.symbol_writable then "" else "u"*) s.symbol_id

and

indicator_ff ff = function
	(* TODO: print something more meaningful for indicator *)
	| Indicator i -> Format.fprintf ff "indicator(%d)" i
	| Indicator_Not s -> Format.fprintf ff "NOT @[%a@]" indicator_ff s
	| Indicator_And (s1, s2) -> Format.fprintf ff "(@[%a@]@ AND @[%a@])" indicator_ff s1 indicator_ff s2
and

byte_ff ff = function
		| Byte_Concrete (c) -> char_ff ff c
		| Byte_Symbolic (s) -> symbol_ff ff s
		| Byte_Bytes (b,i) -> 
			if i=0 then
				fprintf ff "BB[%a]" bytes_ff b
			else
				fprintf ff "\\B"
			(*
			fprintf ff "[(@[<hov>null,@,%a]),%d]" bytes_ff b i
			fprintf ff "\\B"
			*)
(*
and char c = char_ff str_formatter c; flush_str_formatter ()
and symbol s = symbol_ff str_formatter s; flush_str_formatter ()
and byte b = byte_ff str_formatter b; flush_str_formatter ()
*)


(* format entire bytes structure in function-like syntax: op(operand1, ...) *)
(* TODO: convert all formatting to use Format (exp, operation, string_of_int, ...) *)
and bytes_ff ff = bytes_ff_named [] ff
and bytes_ff_named bytes_to_var ff =
	let rec bytes_ff ff = function
		| Bytes_Constant (CInt64(n,ikind,_)) -> fprintf ff "Bytes(%s)" (exp (Const(CInt64(n,ikind,None))))
		| Bytes_Constant (n) -> fprintf ff "Bytes(%s)" (exp (Const(n)))

		| Bytes_ByteArray (bytearray) as bytes ->
			begin try
				let var = List.assoc bytes bytes_to_var in
				fprintf ff "%s" var.vname
			with Not_found ->
				fprintf ff "Bytearray(%a)" begin fun ff ->
					ImmutableArray.fold_left (fun _ -> byte_ff ff) ()
				end bytearray
			end

		| Bytes_Address (Some(block), offset) -> fprintf ff "(addrOf(%s) [%a] + %a)" (memory_block block) bytes_ff block.memory_block_addr bytes_ff offset
		(*| Bytes_Address (Some(block), offset) -> fprintf ff "addrOf(%s,%a)" (memory_block block) bytes_ff offset*)
		| Bytes_Address (None, offset) -> fprintf ff "addrOf(@[<hov>null,@,%a])" bytes_ff offset

		| Bytes_MayBytes (indicator, bytes1, bytes2) ->
			fprintf ff "@[<hv>IF @[%a@] THEN@;<1 2>@[%a@]@ ELSE@;<1 2>@[%a@]@]"
				indicator_ff indicator bytes_ff bytes1 bytes_ff bytes2

		| Bytes_IfThenElse (bytes0, bytes1, bytes2) ->
			fprintf ff "@[<hv>IF @[%a@] THEN@;<1 2>@[%a@]@ ELSE@;<1 2>@[%a@]@]"
				bytes_ff bytes0 bytes_ff bytes1 bytes_ff bytes2

		| Bytes_Op (op,(firstop,_)::[]) ->
			fprintf ff "%s(@[<hov>%a@])"
			(operation op)
			bytes_ff firstop
		| Bytes_Op (op,(firstop,_)::tailops) ->
			fprintf ff "%s(@[<hov>%a%a@])"
			(operation op)
			bytes_ff firstop
			(fun ff -> List.iter (fun (t,_) -> fprintf ff ",@\n%a" bytes_ff t)) tailops
		| Bytes_Op (op,[]) -> fprintf ff "%s()" (operation op)
		| Bytes_Read (content,off,len) -> fprintf ff "READ(@[<hov>%a,@,%a,@,%d@])" bytes_ff content bytes_ff off len
		| Bytes_Write (content,off,len,newbytes) -> fprintf ff "WRITE(@[<hov>%a,@,%a,@,%d,@,%a@])" bytes_ff content bytes_ff off len bytes_ff newbytes
		| Bytes_FunPtr (f,_) -> fprintf ff "funptr(%s)" (fundec f)
        | Bytes_Unbounded (name,id,size) -> fprintf ff "Unbounded(%s,%s,%a)" name (string_of_int id) bytes_ff size
	in
	bytes_ff ff

		
and bytes b = 
  if donotprint() then "" else
  (bytes_ff str_formatter b; flush_str_formatter ())


(* format only operators as tree; useful for checking logical structure *)
and bytes_nf_ff ff = function
    (* unary op:
     * OP OPERAND
     *)
    | Bytes_Op(op, [ not_op, _ ]) ->
        let format = match not_op with
            | Bytes_Op(op', _) when op = op' -> format_of_string "%s@ %a"
            | _ -> format_of_string "%s @[<hov>%a@]"
        in
        fprintf ff format (operation op) bytes_nf_ff not_op
    (* binary op:
     *   LEFT_OPERAND
     * OP
     *   RIGHT_OPERAND
     *)
    | Bytes_Op(op, [ left_op, _; right_op, _ ]) ->
        let format = match (left_op, right_op) with
            | Bytes_Op(op', _), Bytes_Op(op'', _) when op = op' & op = op'' -> format_of_string "@[<hov>%a@]@\n%s@\n@[<hov>%a@]"
            | Bytes_Op(op', _), _ when op = op' -> format_of_string "@[<hov>%a@]@\n%s@\n  @[<hov>%a@]"
            | _, Bytes_Op(op', _) when op = op' -> format_of_string "  @[<hov>%a@]@\n%s@\n@[<hov>%a@]" 
            | _, _ -> format_of_string "  @[<hov>%a@]@\n%s@\n  @[<hov>%a@]"
        in
        fprintf ff format bytes_nf_ff left_op (operation op) bytes_nf_ff right_op
    (* everything else *)
    | bv ->
        bytes_ff ff bv

and bytes_nf b = bytes_nf_ff str_formatter b; flush_str_formatter ()

and bytes_list_ff ff =
    let binop = (operation OP_LAND) in
    function
        | [] -> ()
        | hd::tl -> fprintf ff "  @[<hov>%a@]" bytes_ff hd;
                    List.iter (fun bv -> fprintf ff "@\n%s@\n  @[<hov>%a@]" binop bytes_ff bv) tl 

and bytes_list b = bytes_list_ff str_formatter b; flush_str_formatter ()

and

operation op =
		match op with
	| OP_PLUS -> "BVPLUS"
	| OP_SUB -> "BVSUB"
	| OP_MULT -> "BVMULT"
	| OP_DIV -> "BVDIV"
	| OP_MOD -> "BVMOD"
	| OP_LSL -> "<<"
	| OP_LSR -> ">>"
	| OP_LT -> "BVLT"
	| OP_GT -> "BVGT"
	| OP_LE -> "BVLE"
	| OP_GE -> "BVGE"
	| OP_EQ -> "=="
	| OP_NE -> "NE"
	| OP_BAND -> "&"
	| OP_BXOR -> "BVXOR"
	| OP_BOR -> "|"
	| OP_LAND -> "AND"
	| OP_LOR -> "OR"
	| OP_UMINUS -> "BVUMINUS"
	| OP_BNOT -> "~"
	| OP_LNOT -> "NOT"
	| OP_SX -> "BVSX"

and

memory_block block =
	"\""^block.memory_block_name^"\""


;;

(*
let humanReadableBytes bytesToVars bytes =
	let rec helper bytes =
		match bytes with
			| Bytes_Constant _ -> bytes_ff str_formatter bytes; flush_str_formatter ()
			| Bytes_ByteArray arr -> (
					try (List.assoc bytes bytesToVars).vname
					with Not_found -> bytes_ff str_formatter bytes; flush_str_formatter ()
				)
			| Bytes_Address (memBlockOpt,offsetBytes) -> (
					Printf.sprintf "addrOf(%s,%s)"
						(match memBlockOpt with None -> "null" | Some blk -> memory_block blk)
						(helper offsetBytes)
				)
(*
			| Bytes_Op (OP_LNOT,[(byts,_)]) ->
					"!" ^ helper byts
			| Bytes_Op (OP_LAND,bytes_typ_list) ->
					Printf.sprintf "(%s)"
						(String.concat " /\\ " (List.map (fun (a,_) -> helper a) bytes_typ_list))
			| Bytes_Op (OP_LOR,bytes_typ_list) ->
					Printf.sprintf "(%s)"
						(String.concat " \\/ " (List.map (fun (a,_) -> helper a) bytes_typ_list))
*)
			| Bytes_Op (op,bytes_typ_list) ->
					Printf.sprintf "%s(%s)"
						(operation op)
						(String.concat "," (List.map (fun (a,_) -> helper a) bytes_typ_list))
			| Bytes_Read (srcBytes,offsetBytes,len) ->
					Printf.sprintf "READ(%s,%s,%d)"
						(helper srcBytes)
						(helper offsetBytes)
						len
			| Bytes_Write (writeToTheseBytes,offsetBytes,len,writeTheseBytes) ->
					Printf.sprintf "WRITE(%s,%s,%d,%s)"
						(helper writeToTheseBytes)
						(helper offsetBytes)
						len
						(helper writeTheseBytes)
			| Bytes_FunPtr (fundec,_) ->
					Printf.sprintf "funptr(%s:%s)"
						fundec.svar.vname
						(typ fundec.svar.vtype)
	in
	helper bytes
*)
let humanReadableBytes bytes_to_var bytes =
	bytes_ff_named bytes_to_var str_formatter bytes;
	flush_str_formatter ()

let humanReadablePc pc bytes_to_var =
	List.iter (Format.fprintf str_formatter "%a@\n" (bytes_ff_named bytes_to_var)) pc;
	flush_str_formatter ()

let stmtInfo si =
	let str = si.siFuncName ^ " " ^ (string_of_int si.siStmt.sid) in
	if Executeargs.print_args.Executeargs.arg_print_stmt_locs
	then str ^ " (" ^ (location (get_stmtLoc si.siStmt.skind)) ^ ")"
	else str
