
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
	let compare (a : int) (b : int) = Pervasives.compare a b
end)

let rec add_all add_function keys values map = 
	match (keys,values) with
		| ([],[]) -> map
		| (k::ks,v::vs) -> let map2 = add_function k v map in add_all add_function ks vs map2
		| _ -> raise (Failure "wow")
;;

let increment iref =
  iref:=(!iref)+1

let next_id idref = 
	let id = !idref in
      increment idref; id
;;

let rec print_list print list delim =
	String.concat delim (List.map print list)
;;

let pause () =
	Scanf.scanf "%c" (fun x->())
;;	
