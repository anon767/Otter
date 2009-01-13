
module type OrderedMemStruct =
  sig
    type t
    val compare: t -> t -> int
  end

module MakeMap(Ord: OrderedMemStruct) = Map.Make (Ord)
module MakeSet(Ord: OrderedMemStruct) = Set.Make (Ord)

module IndexMap = MakeMap
(struct
	type t = int
	let compare a b = a - b
end)

let rec add_all add_function keys values map = 
	match (keys,values) with
		| ([],[]) -> map
		| (k::ks,v::vs) -> let map2 = add_function k v map in add_all add_function ks vs map2
		| _ -> raise (Failure "wow")
;;

let next_id idref = 
	let id = !idref in
	idref:=id+1; id
;;

let rec print_list print list delim =
	match list with
		| [] -> ""
		| h::[] -> (print h)
		| h::t -> (print h)^delim^(print_list print t delim)
;;

let pause () =
	Scanf.scanf "%c" (fun x->())
;;	
	
	