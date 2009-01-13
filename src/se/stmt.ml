(* Module of statements *)
open Cil



let hasNext stmt =
	List.length stmt.succs > 0
	;;

let next stmt = 
	List.hd stmt.succs
	;;