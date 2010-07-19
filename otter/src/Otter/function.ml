open Cil
open Bytes
open BytesUtility
open Types

let from_name_in_file = Cilutility.get_fundec

let from_varinfo state varinfo args =
	try
		Cilutility.search_function varinfo
	with Not_found ->
		failwith ("Function "^varinfo.vname^" not found.")

let add_guard_to_state state guard = (*big hack; there should be a nicer way to do this*)
	MemOp.state__add_path_condition state (Bytes_Conditional(Bytes.IfThenElse(guard, Unconditional(lazy_int_to_bytes 1), Unconditional(lazy_int_to_bytes 0)))) true

let from_exp state exp args: (state * fundec) list =
	match exp with
		| Lval(Var(varinfo), NoOffset) ->
			[(state, from_varinfo state varinfo args)]
		| Lval(Mem(exp2), NoOffset) ->
			let state, bytes = Eval.rval state exp2 in
			let rec getall fp =
				let fold_func acc pre leaf =
					let acc =
						match leaf with
							| Bytes_FunPtr(fundec,_) -> 
								(add_guard_to_state state pre, fundec)::acc
							| _ -> acc (* should give a warning here about a non-valid function pointer*)
					in
					(acc, Unconditional(leaf))
				in
				let acc, fp = Bytes.conditional__map_fold ~test:(Stp.query_guard state.path_condition) fold_func [] fp in
				(*Output.set_mode Output.MSG_MUSTPRINT;				
				Output.print_endline (To_string.bytes (Bytes_Conditional(fp)));*)
				acc
			in
			begin match bytes with
				| Bytes_FunPtr(fundec,_) -> [(state, fundec)]
				| Bytes_Read(bytes2, offset, len) -> 
					let fp = (BytesUtility.expand_read_to_conditional bytes2 offset len) in
					(*Output.print_endline (To_string.bytes (Bytes_Conditional(fp)));*)
					(getall fp)
				| Bytes_Conditional(c) ->
					(getall c)

				| _ -> failwith ("Non-constant function ptr not supported : "^(To_string.exp exp2))
			end
		| _ ->
			failwith ("Non-constant function ptr not supported : "^(To_string.exp exp))

