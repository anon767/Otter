(** An extension of Stpvc that deals with endian problem *)
include Stpvc

let is_little_endian = ref true

(** DONE *)
let reverse_bytes vc (len:int) bv = 
	(*Output.print_endline ("Reverse "^(Stpvc.to_string bv)^" of length "^(string_of_int len));*)
	let rec impl vc len bv index =
		if index+8 >= len then (Stpvc.e_bvextract vc bv (index+7) index)
		else Stpvc.e_bvconcat vc (Stpvc.e_bvextract vc bv (index+7) index) (impl vc len bv (index+8))
	in	
	if (!is_little_endian) then
		impl vc len bv 0
	else
		bv


(** DONE *)
let binop_arith op vc len bv1 bv2 = reverse_bytes vc len (op vc len (reverse_bytes vc len bv1) (reverse_bytes vc len bv2))
let binop_compare op vc len bv1 bv2 =  (op vc  (reverse_bytes vc len bv1) (reverse_bytes vc len bv2))
let binop_shift op vc len1 bv1 len2 bv2 = reverse_bytes vc len1 (op vc len1 (reverse_bytes vc len1 bv1) len2 (reverse_bytes vc len2 bv2))

(** DONE *)
let e_bvplus = binop_arith Stpvc.e_bvplus 
let e_bvminus = binop_arith Stpvc.e_bvminus 
let e_bvmult = binop_arith Stpvc.e_bvmult 
let e_bvdiv = binop_arith Stpvc.e_bvdiv 
let e_bvmod = binop_arith Stpvc.e_bvmod 

(** DONE *)
(* Note: signed info is specified by argument *)
let e_bvlt isSigned = binop_compare (if isSigned then Stpvc.e_bvslt else Stpvc.e_bvlt) 
let e_bvle isSigned = binop_compare (if isSigned then Stpvc.e_bvsle else Stpvc.e_bvle) 
let e_bvgt isSigned = binop_compare (if isSigned then Stpvc.e_bvsgt else Stpvc.e_bvgt) 
let e_bvge isSigned = binop_compare (if isSigned then Stpvc.e_bvsge else Stpvc.e_bvge) 


let e_bvslt ()= failwith "Stpc: unused bvsxx functions" 
let e_bvsle ()= failwith "Stpc: unused bvsxx functions" 
let e_bvsgt ()= failwith "Stpc: unused bvsxx functions" 
let e_bvsge ()= failwith "Stpc: unused bvsxx functions" 

let e_bvneg vc bv len = reverse_bytes vc len (Stpvc.e_bvneg vc (reverse_bytes vc len bv)) 

(** TODO *)
let e_bvsextend = Stpvc.e_bvsextend 
let e_bvconstshiftleft = Stpvc.e_bvconstshiftleft 
let e_bvconstshiftright = Stpvc.e_bvconstshiftright 
let e_bvconstshiftright_arith = Stpvc.e_bvconstshiftright_arith 


(** DONE *)
let e_bvshiftleft = binop_shift Stpvc.e_bvshiftleft 
let e_bvshiftright = binop_shift Stpvc.e_bvshiftright 
let e_bvshiftright_arith = binop_shift Stpvc.e_bvshiftright_arith 


let e_bv_of_int vc len n = reverse_bytes vc len (Stpvc.e_bv_of_int vc len n)
  (** Create a bitvector from the given int *)
	
(*
let e_bv_of_int32 = Stpvc.e_bv_of_int32 
  (** Create a 32-bit bitvector from the given int32 *)
let e_bv_of_int64 = Stpvc.e_bv_of_int64 
  (** Create a bitvectr from the given int64 *)
*)

(*
let int_of_e = Stpvc.int_of_e 
  (** Convert a constant bitvector expression into an int *)
let int64_of_e = Stpvc.int64_of_e 
  (** Convert a constant bitvector expression into an int64 *)
*)

(* convenient to check C-style boolean (false == 0, true != 0)  *)
let e_cfalse vc len bv = e_eq vc bv (Stpvc.e_bv_of_int vc len 0)
let e_ctrue vc len bv = e_not vc (e_cfalse vc len bv)

let assert_cfalse vc len bv = do_assert vc (e_cfalse vc len bv)
let assert_ctrue vc len bv = do_assert vc (e_ctrue vc len bv)

let query_cfalse vc len bv = query vc (e_cfalse vc len bv)
let query_ctrue vc len bv = query vc (e_ctrue vc len bv)

