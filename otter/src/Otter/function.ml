open Cil
open Bytes
open Types

exception Notification_Exit of bytes option

(* borrow aspect-oriented programming terminology *)
type pointcut = string
type advice = state -> bytes list -> Cil.instr -> state
type aspect = pointcut * advice

type function_type =
	| Assume
	| Assert
	| Ordinary of Cil.fundec
	| Exit
	| NotFound
	| BooleanOp of Cil.binop
	| BooleanNot
	| Aspect of aspect
	| Builtin of Builtin_function.t
	| BreakPt
	| Evaluate
	| EvaluateString
	| Symbolic
	| SymbolicStatic
	| Comment
	| CurrentState
	| CompareState
    | SymbolicState
	| AssertEqualState
	| IsConcrete
	| PathCondition
    | StringEqual
    | TruthValue
    | Clone
    | Given
    | IfThenElse
(*    | DataStructureOp of Data_structure.ds_op*) 
(*	| Fresh *)
	| PrintState
;;

let aspect_tbl : (pointcut, advice) Hashtbl.t = Hashtbl.create 8;;
let with_aspect (pointcut, advice) fn =
    Hashtbl.add aspect_tbl pointcut advice;
    let res = fn () in
        Hashtbl.remove aspect_tbl pointcut;
        res

let from_name_in_file vname file =
	let rec search = function
		| GFun (fundec, _)::_ when fundec.svar.vname = vname -> fundec
		| _::t -> search t
		| [] -> raise Not_found
	in search file.globals

let from_varinfo state varinfo args =
	begin match varinfo.vname with
		| f when (Hashtbl.mem aspect_tbl f) -> Aspect(f, Hashtbl.find aspect_tbl f)
		| f when (Builtin_function.can_apply_builtin state f args) -> Builtin(Builtin_function.get f)
		| "__ASSUME" -> Assume
		| "__ASSERT" -> Assert
		| "__EVAL" -> Evaluate
		| "__EVALSTR" -> EvaluateString
		| "__BREAKPT" -> BreakPt
		| "__SYMBOLIC" -> Symbolic
		| "__SYMBOLIC_STATIC" -> SymbolicStatic
(*		| "__FRESH" -> Fresh *)
		| "__COMMENT" -> Comment
		| "__CURRENT_STATE" -> CurrentState
		| "__COMPARE_STATE" -> CompareState
		| "__SYMBOLIC_STATE" -> SymbolicState
		| "__ASSERT_EQUAL_STATE" -> AssertEqualState
		| "__ISCONCRETE" -> IsConcrete
		| "__PATHCONDITION" -> PathCondition
(*      | "__STRING_EQUAL" -> StringEqual *)
        | "__TRUTH_VALUE" -> TruthValue
        | "__CLONE" -> Clone
        | "__GIVEN" -> Given
        | "__ITE" -> IfThenElse
(*        | "__SET_INIT" -> DataStructureOp (Data_structure.op__SET_INIT) *)
(*        | "__SET_FIND" -> DataStructureOp (Data_structure.op__SET_FIND) *)
		| "exit" -> Exit			(* exit is so special that can't be put in builtin *)
		| "AND" -> BooleanOp(Cil.LAnd)
		| "OR" -> BooleanOp(Cil.LOr)
		| "NOT" -> BooleanNot
		| "__PRINT_STATE" -> PrintState
		| f ->
				try
					Ordinary(Cilutility.search_function varinfo)
				with Not_found ->
(*					if Executeargs.run_args.arg_symbolic_extern_fns then
						(Output.print_endline(varinfo.vname^" is undefined; returning fresh symbolic value.");
						 NotFound)
					else*)
						failwith ("Function "^varinfo.vname^" not found.")
	end
;;

let from_exp state exp args: (state * function_type) list =
	match exp with
		| Lval(Var(varinfo), NoOffset) ->
			[(state, from_varinfo state varinfo args)]
		| Lval(Mem(exp2), NoOffset) ->
			let state, bytes = Eval.rval state exp2 in
			let rec getall fp =
				match fp with
					| Bytes.IfThenElse (g, x, y) -> (getall x)@(getall y)
					| Bytes.Unconditional x -> 
						match x with
							| Bytes_FunPtr(fundec,_) -> [(state, Ordinary(fundec))]
							| _ -> []
			in
			begin match bytes with
				| Bytes_FunPtr(fundec,_) -> [(state, Ordinary (fundec))]
				| Bytes_Read(bytes2, offset, len) -> 
					let fp = (Eval.expand_read_to_conditional state bytes2 len offset) in
					Output.print_endline (To_string.bytes (Bytes_Conditional(fp)));
					(getall fp)
				| Bytes_Conditional(c) ->
					(getall c)

				| _ -> failwith ("Non-constant function ptr not supported : "^(To_string.exp exp2))
			end
		| _ ->
			failwith ("Non-constant function ptr not supported : "^(To_string.exp exp))
;;
