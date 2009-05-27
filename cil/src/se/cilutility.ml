open Cil

module VarinfoMap =
	Utility.MakeMap (
	struct
		type t = Cil.varinfo
		let compare a b =
			(* try Output.print_endline ("Compare "^a.Cil.vname^" and            *)
			(* "^b.Cil.vname);                                                   *)
			let c = Pervasives.compare a.Cil.vid b.Cil.vid in
				(* Output.print_endline ("Result = "^(string_of_int c)); *)
			c
	end
	)

let bitsSizeOfExp exp =
	(Cil.bitsSizeOf (Cil.typeOf exp)) / 8;;

let fundecHashtbl : (varinfo, fundec) Hashtbl.t = Hashtbl.create 100;;

let search_function = Hashtbl.find fundecHashtbl;;

module FundecMap =
	Utility.MakeMap (
	struct
		type t = Cil.fundec
		let compare a b = let id x = x.svar.vid in Pervasives.compare (id a) (id b)
	end
	)

(** This is like {!Cil.unrollType} except that it only keeps the
		attributes in the base (unnamed) type. *)
let unrollType = function
	| TNamed (typInfo,_) -> unrollType typInfo.ttype
	| x -> x
