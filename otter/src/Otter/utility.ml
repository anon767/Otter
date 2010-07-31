
module IndexMap = Map.Make (struct
	type t = int
	let compare (a : int) (b : int) = Pervasives.compare a b
end)

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

