open Format
open OcamlUtilities.FormatPlus
open Bytes


(** Option to force characters to always be printed as hex codes. *)
let print_char_as_int = ref false


(** Print a character, escaping non-printing characters as necessary.
		@param ff is the formatter to which to print
		@param c is the character to print
*)
let char ff c =
	if !print_char_as_int then
		fprintf ff "\\%02X" (Char.code c)
	else match c with
		| '\\' ->
			pp_print_string ff "\\\\"
		| '\n' ->
			fprintf ff "\\n@\n"
		| '\032' .. '\126' ->
			pp_print_char ff c
		| c ->
			fprintf ff "\\x%02x" (Char.code c)


(** Print a {!type:Bytes.operator}.
		@param ff is the formatter to which to print
		@param op is the operator to print
*)
let operator ff op =
	let label = match op with
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
	in
	pp_print_string ff label


(** Print a {!type:Bytes.symbol}.
		@param ff is the formatter to which to print
		@param s is the {!type:Bytes.symbol} to print
*)
let symbol ff s = fprintf ff "\\<%d>" s.symbol_id


(** Print a {!type:Bytes.byte}, escaping characters using {!char}.
		@param ff is the formatter to which to print
		@param b is the {!type:Bytes.byte} to print
*)
let rec byte ff = function
	| Byte_Concrete c -> char ff c
	| Byte_Symbolic s -> symbol ff s
	| Byte_Bytes (b, 0) -> fprintf ff "BB[@[<hov>%a@]@,]" bytes b
	| Byte_Bytes _ -> pp_print_string ff "\\B"


(** Print a {!type:Bytes.byte ImmutableArray.t} as a sequence of {!type:Bytes.byte} using {!byte},
	representing a {!type:Bytes.byte} repeated {i n} times as ["<byte>\\{n}"].
		@param ff is the formatter to which to print
		@param arr is the {!type:Bytes.byte ImmutableArray.t} to print
*)
and bytearray ff arr =
	let byte_n b c =
		byte ff b;
		if c <> 1 then fprintf ff "\\{%d}" c;
		pp_print_cut ff ()
	in
	pp_print_string ff "Bytearray(";
	let last = ImmutableArray.fold_left begin fun b' b -> match b' with
		| Some (b', n) when match b with Byte_Bytes _ -> false | _ -> b = b' ->
			Some (b', n + 1)
		| Some (b', n) ->
			byte_n b' n;
			Some (b, 1)
		| None ->
			Some (b, 1)
	end None arr in
	match last with
		| Some (b, n) ->
			byte_n b n;
			pp_print_string ff ")"
		| None ->
			failwith "Unreachable"


(** Print a {!type:Bytes.byte ImmutableArray.t} as if it were a null-terminated C-style string. {!Bytes.Byte_Bytes}
	are not supported and will be printed as "\?".
		@param ff is the formatter to which to print
		@param arr is the {!type:Bytes.byte ImmutableArray.t} to print
*)
and bytestring ff arr =
	let byte = function
		| Byte_Concrete c ->
			char ff c
		| Byte_Symbolic s ->
			symbol ff s
		| Byte_Bytes _ ->
			pp_print_string ff "\\?"
	in
	let rec bytestring n =
		if n < ImmutableArray.length arr then
			match ImmutableArray.get arr n with
				| Byte_Concrete '\000' ->
					()
				| s ->
					byte s;
					pp_print_cut ff ();
					bytestring (n + 1)
	in
	bytestring 0


(** Print a {!type:Bytes.guard}.
		@param ff is the formatter to which to print
		@param g is the {!type:Bytes.guard} to print
*)
and guard ff = function
	| Guard_True -> pp_print_string ff "TRUE"
	| Guard_Not g -> fprintf ff "NOT @[<hov>%a@]" guard g
	| Guard_And (g1, g2) -> fprintf ff "(@[<hov>%a@]@ AND @[<hov>%a@]@,)" guard g1 guard g2
	| Guard_Or (g1, g2) -> fprintf ff "(@[<hov>%a@]@ OR @[<hov>%a@]@,)" guard g1 guard g2
	| Guard_Symbolic s -> symbol ff s
	| Guard_Bytes b -> bytes ff b


(** Print a {!type:Bytes.conditional}.
		@param unconditional is the printer to print the unconditional leaves. (Caveat: ocaml-3.11 and below do not
			assign polymorphic types to arguments of [let rec] binding if the argument is used with a concrete type
			elsewhere, so only {!bytes} can be passed as argument).
		@param ff is the formatter to which to print
		@param g is the {!type:Bytes.guard} to print
*)
(* TODO: uncomment the polymorphic type annotation after upgrading to ocaml-3.12 *)
and conditional (* : 'a . (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a conditional -> unit *) = fun unconditional ff ->
	let rec conditional ff = function
		| IfThenElse (g, t, f) ->
			fprintf ff "If @[<hov>%a@]@ Then @[<hov>%a@]@ Else @[<hov>%a@]" guard g conditional t conditional f
		| Unconditional x ->
			unconditional ff x
		| ConditionalException e ->
			fprintf ff "Exn %s" (Printexc.to_string e)
	in
	conditional ff


(** Print a {!type:Bytes.bytes}.
		@param ff is the formatter to which to print
		@param b is the {!type:Bytes.bytes} to print
*)
and bytes ff = bytes_named [] ff


(** Print a {!type:Bytes.bytes}, substituting readable names for {!type:Bytes.bytes} in subexpressions if provided.
		@param bytes_to_names is an associated list of {!type:Bytes.bytes} for which to subsitute with names
		@param ff is the formatter to which to print
		@param b is the {!type:Bytes.bytes} to print
*)
and bytes_named bytes_to_names ff =
	let rec bytes ff = function
		| Bytes_Constant (Cil.CInt64 (n, ikind, _)) ->
			fprintf ff "Bytes(@[<hov>%a@]@,)" Printcil.f_const (Cil.CInt64 (n, ikind, None))

		| Bytes_Constant n ->
			fprintf ff "Bytes(@[<hov>%a@]@,)" Printcil.f_const n

		| Bytes_ByteArray arr as bytes ->
			begin try
				let var = List.assoc bytes bytes_to_names in
				pp_print_string ff var.Cil.vname
			with Not_found ->
				bytearray ff arr
			end

		| Bytes_Address (block, offset) ->
			fprintf ff "Addr(@[<hov>%a@]@ + @[<hov>%a@]@,)" memory_block block bytes offset

		| Bytes_Conditional c ->
			conditional bytes ff c

		| Bytes_Op (op, operands) ->
			fprintf ff "%a(@[<hov>%a@]@,)" operator op (pp_print_list (fun ff (x, _) -> bytes ff x) ",@ ") operands

		| Bytes_Read (content, off, len) ->
			fprintf ff "Read(@[<hov>%a@],@ @[<hov>%a@],@ %d@,)" bytes content bytes off len

		| Bytes_Write (content, off, len, newbytes) ->
			fprintf ff "Write(@[<hov>%a@],@ @[<hov>%a@],@ %d,@ @[<hov>%a@]@,)" bytes content bytes off len bytes newbytes

		| Bytes_FunPtr (v, _) ->
			fprintf ff "Funptr(%s)" v.Cil.vname

		| Bytes_Unbounded (name, id, size) ->
			fprintf ff "Unbounded(%s,@ %d,@ @[<hov>%a@]@,)" name id bytes size
	in
	bytes ff


(** Print a {!type:Bytes.memory_block}.
		@param ff is the formatter to which to print
		@param b is the {!type:Bytes.memory_block} to print
*)
and memory_block ff block =
	let block_type = match block.memory_block_type with
		| Block_type_StringLiteral -> "StringLiteral"
		| Block_type_Global -> "Global"
		| Block_type_Local -> "Local"
		| Block_type_Heap -> "Heap"
		| Block_type_Aliased -> "Aliased"
	in
	fprintf ff "\"%s\"@@%s(@[<hov>%a@]@,)" block.memory_block_name block_type bytes block.memory_block_addr


(** Print a {!type:Bytes.bytes}, formatting the outermost unary/binary expressions as a tree.
		@param ff is the formatter to which to print
		@param b is the {!type:Bytes.bytes} to print
*)
let rec bytes_tree ff = function
	(* unary op:
	 * OP OPERAND
	 *)
	| Bytes_Op(op, [ not_op, _ ]) ->
		let op_tree ff x = match not_op with
			| Bytes_Op(op', _) when op = op' -> bytes_tree ff x
			| _ -> fprintf ff "@[<hov>%a@]" bytes_tree x
		in
		fprintf ff "%a@ %a" operator op op_tree not_op

	(* binary op:
	 *   LEFT_OPERAND
	 * OP
	 *   RIGHT_OPERAND
	 *)
	| Bytes_Op(op, [ left_op, _; right_op, _ ]) ->
		let op_tree ff x = match x with
			| Bytes_Op(op', _) when op = op' -> fprintf ff "@[<hov>%a@]" bytes_tree x
			| _ -> fprintf ff "  @[<hov>%a@]" bytes_tree x
		in
		fprintf ff "%a@\n%a@\n%a" op_tree left_op operator op op_tree right_op

	(* everything else *)
	| bv ->
		bytes ff bv

