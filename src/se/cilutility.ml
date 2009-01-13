open Cil

module VarinfoMap =
	Utility.MakeMap (
	struct
		type t = Cil.varinfo
		let compare a b =
			(* try Output.print_endline ("Compare "^a.Cil.vname^" and            *)
			(* "^b.Cil.vname);                                                   *)
			let c = a.Cil.vid - b.Cil.vid in
				(* Output.print_endline ("Result = "^(string_of_int c)); *)
			c
	end
	)

let bitsSizeOfExp exp =
	(Cil.bitsSizeOf (Cil.typeOf exp)) / 8;;

let rec search_function varinfo gs =
	match gs with
		| [] -> failwith "Function not found"
		| GFun (fundec, _) :: t ->
				if fundec.svar == varinfo then fundec else search_function varinfo t
		| _ :: t -> search_function varinfo t
;;

module FundecMap =
	Utility.MakeMap (
	struct
		type t = Cil.fundec
		let compare a b = let id x = x.svar.vid in id a - id b
	end
	)