module IndexMap = Utility.IndexMap

type 'a t =
	{
		map: 'a IndexMap.t;
		length: int;
		offset: int;
		default: 'a option;
	}
;;

let length array =
	let len = array.length-array.offset in
		if(len<0) then failwith "Array length can't be less than zero" else
		len
;;

let get array i =
	try
		IndexMap.find (array.offset+i) array.map
	with Not_found ->
		match array.default with
			| None -> failwith "Array element not initialized" (* error occurs when someone declares an array of size 0, and read from it *)
			| Some(elm) -> elm
;;

let set array i elm =
	let ii = i+array.offset in
	{ array with
		length = 
			if (ii+1) > array.length then 
				(ii+1) 
			else array.length; (*should expand? *)
		map = IndexMap.add ii elm array.map;
	}
;;

let add array elm =
	set array (length array) elm
;;

let sub array offset2 size = 
	let offset3 = array.offset+offset2 in
	{	array with
		length = size+offset3;
		offset = offset3;
	}
;;
	
let empty =
	{
		length = 0;
		offset = 0;
		map = IndexMap.empty;
		default = None;
	}
;;

let make n initval =
	{ empty with
		length = n;
		default = Some(initval);
	}
;;

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
;;

let fold_left ff a bs =
	let len = length bs in
	let rec impl ff a bs i =
		if i>=len then a 
		else let aa = ff a (get bs i) in
			impl ff aa bs (i+1)
	in
		impl ff a bs 0
;;
 
let map ff bs =
	let fff lst bb =
		(ff bb)::lst
	in 
	let l1 = fold_left fff [] bs in
	let l2 = List.rev_append l1 [] in
		of_list l2
;;

let fold2_left ff a bs1 bs2 =
	let len = length bs1 in
	if len <> length bs2 then failwith "fold2_left: input arrays are of unequal length" else
	let rec impl ff a bs1 bs2 i =
		if i>=len then a 
		else let aa = ff a (get bs1 i) (get bs2 i) in
			impl ff aa bs1 bs2 (i+1)
	in
		impl ff a bs1 bs2 0
;;

let exists p arr =
	let len = length arr in
	let rec exists_aux i =
		if i >= len
		then false
		else p (get arr i) || exists_aux (succ i)
	in
	exists_aux 0

let exists2 p arr arr' =
	let len = length arr in
	if len <> length arr' then failwith "exists2: input arrays are of unequal length";
	let rec exists2_aux i =
		if i >= len
		then false
		else p (get arr i) (get arr' i) || exists2_aux (succ i)
	in
	exists2_aux 0

let for_all p arr =
	let rec for_all_aux i =
		if i >= length arr
		then true
		else p (get arr i) && for_all_aux (succ i)
	in
	for_all_aux 0
