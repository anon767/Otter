
module IndexMap = Map.Make (struct
	type t = int
	let compare (a : int) (b : int) = Pervasives.compare a b
end)

module StringSet = Set.Make(String)

