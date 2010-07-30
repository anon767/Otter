
module IndexMap = Map.Make (struct
	type t = int
	let compare (a : int) (b : int) = Pervasives.compare a b
end)

let rec add_all add_function keys values map = 
	match (keys,values) with
		| ([],[]) -> map
		| (k::ks,v::vs) -> let map2 = add_function k v map in add_all add_function ks vs map2
		| _ -> raise (Failure "wow")


let increment iref =
  iref:=(!iref)+1

let next_id idref = 
	let id = !idref in
      increment idref; id


(* TODO: decide where these actually belong*)
let jidCounter = ref 0
let next_jid = next_id jidCounter

let rec print_list print list delim =
	String.concat delim (List.map print list)


let getchar () =
	Scanf.scanf "%c" (fun x->x)
	


module StringSet = Set.Make(String)

let errormsg e = match e with Failure s -> s | _ -> "Unknown error"

type color = { color_code: string }

let red = {color_code="31"}
let green = {color_code="32"}
let blue = {color_code="34"}

let colorize color text =
  Printf.sprintf "\x1b[%sm%s\x1b[m" color.color_code text

