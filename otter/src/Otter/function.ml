open Cil
open Bytes
open BytesUtility
open Types

exception Notification_Exit of bytes option

(* borrow aspect-oriented programming terminology *)
type pointcut = string
type advice = state -> bytes list -> Cil.instr -> state
type aspect = pointcut * advice

type function_type =
	| Ordinary of Cil.fundec
	| Aspect of aspect
	| Symbolic
	| CurrentState
	| CompareState
	| AssertEqualState
	| PrintState


let aspect_tbl : (pointcut, advice) Hashtbl.t = Hashtbl.create 8
let with_aspect (pointcut, advice) fn =
    Hashtbl.add aspect_tbl pointcut advice;
    let res = fn () in
        Hashtbl.remove aspect_tbl pointcut;
        res

let from_name_in_file = Cilutility.get_fundec

let from_varinfo state varinfo args =
	begin match varinfo.vname with
		| f when (Hashtbl.mem aspect_tbl f) -> Aspect(f, Hashtbl.find aspect_tbl f)
		| "__SYMBOLIC" -> Symbolic
		| "__CURRENT_STATE" -> CurrentState
		| "__COMPARE_STATE" -> CompareState
		| "__ASSERT_EQUAL_STATE" -> AssertEqualState
		| "__PRINT_STATE" -> PrintState
		| f ->
				try
					Ordinary(Cilutility.search_function varinfo)
				with Not_found ->
					failwith ("Function "^varinfo.vname^" not found.")
	end


let add_guard_to_state state guard = (*big hack; there should be a nicer way to do this*)
	MemOp.state__add_path_condition state (Bytes_Conditional(Bytes.IfThenElse(guard, Unconditional(lazy_int_to_bytes 1), Unconditional(lazy_int_to_bytes 0)))) true

let from_exp state exp args: (state * function_type) list =
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
								(add_guard_to_state state pre, Ordinary(fundec))::acc
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
				| Bytes_FunPtr(fundec,_) -> [(state, Ordinary (fundec))]
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

