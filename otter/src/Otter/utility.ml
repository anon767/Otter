
module IndexMap = Map.Make (struct
	type t = int
	let compare (a : int) (b : int) = Pervasives.compare a b
end)

let rec print_list print list delim =
	String.concat delim (List.map print list)

module StringSet = Set.Make(String)

