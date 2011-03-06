
module IndexMap = Map.Make (struct
	type t = int
	let compare (a : int) (b : int) = Pervasives.compare a b
end)


type 'a t =
	{
		map: 'a IndexMap.t;
		length: int;
		offset: int;
		default: 'a option;
	}


let length array = array.length


let get array i =
	try
		IndexMap.find (array.offset+i) array.map
	with Not_found ->
		match array.default with
			| None -> failwith "Array element not initialized" (* error occurs when someone declares an array of size 0, and read from it *)
			| Some(elm) -> elm


let set array i elm =
	let ii = i + array.offset in
	let length =
		 (* TODO: should disallow silent expansion? *)
		if ii >= array.offset + array.length then
			ii + 1
		else
			array.length
	in
	let map = IndexMap.add ii elm array.map in
	{ array with length; map }


let add array elm =
	set array (length array) elm


let sub array offset length =
	{ array with length; offset = array.offset + offset }

	
let empty =
	{
		length = 0;
		offset = 0;
		map = IndexMap.empty;
		default = None;
	}


let make n initval =
	{ empty with
		length = n;
		default = Some(initval);
	}


let of_list (lst: 'a list) : 'a t =
	if lst=[] then empty 
	else
	let default = List.hd lst in
	let rec impl lst n =
		match lst with
			| [] -> make 0 default
			| head:: tail ->
				let array = impl tail (n + 1) in
					set array n head
	in
	impl lst 0 


let equal eq xs ys =
	let module E = struct exception Not_equal end in
	let deep_equal xs ys =
		try
			(* compare xs.map bindings in ys.map or against ys.default *)
			let ymap = IndexMap.fold begin fun xk xv ymap ->
				try
					let yk = xk - xs.offset + ys.offset in
					let yv = IndexMap.find yk ymap in
					if eq xv yv then
						IndexMap.remove yk ymap
					else
						raise E.Not_equal
				with Not_found ->
					match ys.default with
						| Some yv when eq xv yv -> ymap
						| _ -> raise E.Not_equal
			end xs.map ys.map in

			(* compare the remainder of ys.map against xs.default *)
			IndexMap.is_empty ymap
			|| match xs.default with
				| Some xv -> IndexMap.for_all (fun _ yv -> eq xv yv) ymap
				| None -> false
		with E.Not_equal ->
			false
	in
	xs.length = ys.length && (xs.offset = ys.offset && xs.map == ys.map) || deep_equal xs ys


(* TODO: remove everything below, as uses of the below cannot take advantage of the sparsity of ImmutableArrays *)

let fold_left ff a bs =
	let len = length bs in
	let rec impl ff a bs i =
		if i>=len then a 
		else let aa = ff a (get bs i) in
			impl ff aa bs (i+1)
	in
		impl ff a bs 0

let map ff bs =
	let fff lst bb =
		(ff bb)::lst
	in 
	let l1 = fold_left fff [] bs in
	let l2 = List.rev_append l1 [] in
		of_list l2

let exists p arr =
	let len = length arr in
	let rec exists_aux i =
		if i >= len
		then false
		else p (get arr i) || exists_aux (succ i)
	in
	exists_aux 0

let for_all p arr =
	let len = length arr in
	let rec for_all_aux i =
		if i >= len
		then true
		else p (get arr i) && for_all_aux (succ i)
	in
	for_all_aux 0
