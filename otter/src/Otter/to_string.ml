(* TODO: perhaps introduce laziness? *) 
open Format
open Cil
open Bytes
open Types

let strlen = 1000;;
let force_print = ref false;;
let donotprint() = (not (!force_print)) && (not (Output.need_print (!Output.current_msg_type)));;

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
		| Instr(_) -> "(INSTRS)"
		| Loop(_,_,_,_) -> "(LOOP)"
		| Block(_) -> "(BLOCK)"
		| _ ->	Pretty.sprint strlen (Cil.d_stmt () s)
and

stmtkind skind =
  match skind with
    | Cil.Instr _ -> "instr"
    | Cil.Return _ -> "return"
    | Cil.Goto _ -> "goto"
    | Cil.Break _ -> "break"
    | Cil.Continue _ -> "continue"
    | Cil.If _ -> "if"
    | Cil.Switch  _ -> "switch"
    | Cil.Loop  _ -> "loop"
    | Cil.Block  _ -> "block"
    | Cil.TryFinally  _ -> "tryfinally"
    | Cil.TryExcept  _ -> "tryexcept"

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
	else if c = '\\'
	then fprintf ff "/\\"
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
*)
and symbol s = symbol_ff str_formatter s; flush_str_formatter ()

and

byte b = byte_ff str_formatter b; flush_str_formatter ()

and

guard_ff ff = function
	| Guard_True -> Format.fprintf ff "TRUE"
	| Guard_Not g -> Format.fprintf ff "NOT @[%a@]" guard_ff g
	| Guard_And (g1, g2) -> Format.fprintf ff "(@[%a@]@ AND @[%a@]@,)" guard_ff g1 guard_ff g2
	| Guard_Or (g1, g2) -> Format.fprintf ff "(@[%a@]@ OR @[%a@]@,)" guard_ff g1 guard_ff g2
	| Guard_Symbolic s -> symbol_ff ff s
	| Guard_Bytes b -> bytes_ff ff b

		
and guard g = 
  if donotprint() then "" else
  (guard_ff str_formatter g; flush_str_formatter ())



(* format entire bytes structure in function-like syntax: op(operand1, ...) *)
(* TODO: convert all formatting to use Format (exp, operation, string_of_int, ...) *)
and

bytes_ff ff = bytes_ff_named [] ff

and

bytes_ff_named bytes_to_var ff =
	let rec bytes_ff ff = function
		| Bytes_Constant (CInt64(n,ikind,_)) -> fprintf ff "Bytes(%s)" (exp (Const(CInt64(n,ikind,None))))
		| Bytes_Constant (n) -> fprintf ff "Bytes(%s)" (exp (Const(n)))

		| Bytes_ByteArray (bytearray) as bytes ->
			begin try
				let var = List.assoc bytes bytes_to_var in
				fprintf ff "%s" var.vname
			with Not_found ->
              let helper ff b c = fprintf ff "%a" byte_ff b; if c=1 then () else fprintf ff "\\{%d}" c 
              in
              fprintf ff "Bytearray(";
              let (last,count) = 
                 (ImmutableArray.fold_left 
                    (fun a b -> match a with 
                       | (None,_) -> (Some b,1) 
                       | (Some b',count) -> 
                           if (match b with Byte_Bytes _ -> false | _ -> b' = b) 
                           then (Some b',count+1) else (helper ff b' count ;(Some b,1))
                    )
                    (None,0) bytearray) 
               in 
                 match last with 
                   | Some b -> (helper ff b count); fprintf ff ")"
                   | None -> failwith "Unreachable"
			end

		| Bytes_Address (block, offset) -> fprintf ff "(addrOf(%s) [%a] + %a)" (memory_block block) bytes_ff block.memory_block_addr bytes_ff offset
		(*| Bytes_Address (block, offset) -> fprintf ff "addrOf(%s,%a)" (memory_block block) bytes_ff offset*)

		| Bytes_Conditional c ->
			conditional_ff bytes_ff ff c

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

and

lval_block_ff ff (block, offset) =
	fprintf ff "(@[%s@],@ @[%a@]@,)" (memory_block block) bytes_ff offset


and

conditional_ff unconditional_ff ff =
	let rec conditional_ff ff = function
		| IfThenElse (guard, t, f) ->
			fprintf ff "(IF @[%a@]@ THEN @[%a@]@ ELSE @[%a@]@,)" guard_ff guard conditional_ff t conditional_ff f
		| Unconditional x ->
			unconditional_ff ff x
     | ConditionalException e ->
			fprintf ff "(Exn @[%s@]@ @,)" (Utility.errormsg e) (* TODO: get the exception description *)

	in
	conditional_ff ff

and

conditional  unconditional_ff c = 
  if donotprint() then "" else
  (conditional_ff unconditional_ff str_formatter c; flush_str_formatter ())
;;



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

let deferred = function
	| Immediate b -> bytes b
	| Deferred _ -> "<deferred>"

let rec typedBytes b typ =
	match b,typ with
		| Bytes_ByteArray arr, TComp({cstruct=true;cfields=fields},_) ->
				string_of_struct arr typ fields
		| Bytes_ByteArray arr, TArray(baseType,_,_) ->
				string_of_array arr baseType
		| _ -> bytes b

and

string_of_struct bytearray structTyp fields =
	String.concat "\n"
		["{";
		 String.concat "\n"
			 (List.map
					(fun field -> field.fname ^ " = " ^
						 let offset,width = bitsOffset structTyp (Field (field,NoOffset)) in
						 let offset = offset/8 and width = width/8 in (* Convert bit sizes to bytes *)
						 let subarray = ImmutableArray.sub bytearray offset width in
						 typedBytes (Bytes_ByteArray subarray) field.ftype
					)
					fields);
		 "}"]

and

string_of_array arr baseTyp =
	let sizeOfBaseType = (bitsSizeOf baseTyp) / 8 in
	"[" ^ (String.concat ", " (strings_of_array_elts arr baseTyp sizeOfBaseType)) ^ "]"

and

strings_of_array_elts arr typ sizeOfTyp =
	if ImmutableArray.length arr = 0
	then []
	else
		typedBytes (Bytes_ByteArray (ImmutableArray.sub arr 0 sizeOfTyp)) typ ::
			(strings_of_array_elts (ImmutableArray.sub arr sizeOfTyp (ImmutableArray.length arr - sizeOfTyp)) typ sizeOfTyp)
