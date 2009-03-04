open Cil
open Types
open Executeargs

exception Notification_Exit

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
	| AssertEqualState
	| IsConcrete
	| PathCondition
    | StringEqual
    | TruthValue
    | Clone
    | DataStructureOp of Data_structure.ds_op 
(*	| Fresh *)
;;

let aspect_tbl : (pointcut, advice) Hashtbl.t = Hashtbl.create 8;;
let with_aspect (pointcut, advice) fn =
    Hashtbl.add aspect_tbl pointcut advice;
    let res = fn () in
        Hashtbl.remove aspect_tbl pointcut;
        res

(* This function is only called by executemain *)
let from_signature (s : string) =
	let rec search ss gs =
		match gs with
			| [] -> raise Not_found
			| GFun (fundec,loc) :: t ->
				if fundec.svar.vname = ss then (fundec,loc) else search ss t
			| _ :: t -> search ss t
	in search s (Executedata.globals ())
	;;

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
		| "__ASSERT_EQUAL_STATE" -> AssertEqualState
		| "__ISCONCRETE" -> IsConcrete
		| "__PATHCONDITION" -> PathCondition
(*      | "__STRING_EQUAL" -> StringEqual *)
        | "__TRUTH_VALUE" -> TruthValue
        | "__CLONE" -> Clone
(*        | "__SET_INIT" -> DataStructureOp (Data_structure.op__SET_INIT) *)
(*        | "__SET_FIND" -> DataStructureOp (Data_structure.op__SET_FIND) *)
		| "exit" -> Exit			(* exit is so special that can't be put in builtin *)
		| "AND" -> BooleanOp(Cil.LAnd)
		| "OR" -> BooleanOp(Cil.LOr)
		| "NOT" -> BooleanNot
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
let from_exp state exp args: function_type =
	match exp with
		| Lval(Var(varinfo), NoOffset) -> from_varinfo state varinfo args
		| Lval(Mem(exp2),NoOffset) -> 
			begin match Eval.rval state exp2 with
				| Bytes_FunPtr(fundec,_) -> Ordinary (fundec)
				| _ -> failwith ("Non-constant function ptr not supported : "^(To_string.exp exp2))
			end
		| _ -> failwith ("Non-constant function ptr not supported : "^(To_string.exp exp))
;;
