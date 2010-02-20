
module type OrderedMemStruct =
  sig
    type t
    val compare: t -> t -> int
  end

module MakeMap(Ord: OrderedMemStruct) = struct
	include Map.Make (Ord)

	type 'a combination2 =
		| Both of 'a * 'a
		| Left of 'a
		| Right of 'a

	(** fold over the bindings of two maps *)
	let fold2 f map_x map_y acc =

		(* first, fold over map_x and pull out the corresponding value from map_y *)
		let map_y, acc = fold begin fun key_x val_x (map_y, acc) ->
			try
				let val_y = find key_x map_y in
				let map_y = remove key_x map_y in
				(map_y, f key_x (Both (val_x, val_y)) acc)
			with Not_found ->
				(map_y, f key_x (Left val_x) acc)
		end map_x (map_y, acc) in

		(* then, fold over the remainder of map_y *)
		let acc = fold begin fun key_y val_y acc ->
			f key_y (Right val_y) acc
		end map_y acc in

        acc
end
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
