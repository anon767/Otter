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

let fundecHashtbl : (varinfo, fundec) Hashtbl.t = Hashtbl.create 100;;

let search_function = Hashtbl.find fundecHashtbl;;

module FundecMap =
	Utility.MakeMap (
	struct
		type t = Cil.fundec
		let compare a b = let id x = x.svar.vid in id a - id b
	end
	)

(* Order locations by file name, then by line, then by offset *)
let compareLoc loc1 loc2 =
	let fileCmp = compare loc1.file loc2.file in
	if fileCmp = 0 then
		let lineCmp = compare loc1.line loc2.line in
		if lineCmp = 0 then compare loc1.byte loc2.byte
		else lineCmp
	else fileCmp
