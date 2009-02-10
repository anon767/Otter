(** Higher level interface to Libstp.
   
   Note that for garbage collection to work properly, all expressions must
   contain a reference to the vc they were created in, because when the
   vc is destroyed, the expressions are no longer valid.
   This will be fixed in the next version. (Until recently, there wasn't any
   way to free resources)
  
   The easiest way to do that of course would be to have exp include a
   reference to the vc, which also has the advantage that functions that
   take an exp wouldn't need to get the vc explicitly.
  
   The shift by expression problem could be solved by also keeping track
   of the type in the expression. (or having stp provide a way to get it)
  
   Another idea would be to have separate ocaml types for boolean,
   bitvector, and array expressions. This would let the ocaml typechecker
   catch errors at compile time, rather than defering those for STP to catch
   at runtime. Maybe something like:
   type bool_exp
   type bv_exp
   type ('a,'b) array_exp
   It would be nice if we could get the width of the bitvectors in there
   somehow.
  
   @author Ivan Jager
 *)


(* These commmands are reasonably useful for debugging

let vc = Stpvc.create_validity_checker()
let f = Stpvc.e_false vc
let t = Stpvc.e_true vc
let bool = Stpvc.bool_t vc
let bv32 = Stpvc.bitvector_t vc 32
let b = Stpvc.e_var vc "b" bool
let x = Stpvc.e_var vc "x" bv32
let y = Stpvc.e_var vc "y" bv32
let bv = Stpvc.e_bv_of_int64 vc 32 0L
let e = (Stpvc.e_not vc (Stpvc.e_bvbitextract vc bv 0))
;;
Stpvc.query vc e;;
*)

open Libstp


type vc = Libstp.vc
type exp = Libstp.expr 
type typ = Libstp.typ 


(* some helper functions *)
let wrap3bv f arg1 arg2 arg3 =
  if arg2 > 0
  then (f arg1 arg2 arg3)
  else failwith "bitvector width must be greater than 0"
(* end helper functions *)


let create_validity_checker = vc_createValidityChecker

(* functions for constructing types *)
let bool_t = vc_boolType
let array_t = vc_arrayType
let bitvector_t = vc_bvType


let e_simplify = vc_simplify

(* functions for constructing expressions *)
let e_var = vc_varExpr
let e_eq = vc_eqExpr
let e_true = vc_trueExpr
let e_false = vc_falseExpr
let e_not = vc_notExpr
let e_and = vc_andExpr
let e_or = vc_orExpr
let e_implies = vc_impliesExpr
let e_iff = vc_iffExpr
let e_ite = vc_iteExpr

let e_boolbv = vc_boolToBVExpr
let e_read = vc_readExpr
let e_write = vc_writeExpr

let e_bv_of_string = vc_bvConstExprFromStr
let e_bv_of_int = wrap3bv vc_bvConstExprFromInt
let e_bv_of_int32 = vc_bv32ConstExprFromInt
let e_bv_of_int64 = wrap3bv vc_bvConstExprFromLL

let e_bvconcat = vc_bvConcatExpr
let e_bvplus = vc_bvPlusExpr
let e_bvminus = vc_bvMinusExpr
let e_bvmult = vc_bvMultExpr
let e_bvdiv = vc_bvDivExpr
let e_bvmod = vc_bvModExpr
let e_bvand = vc_bvAndExpr
let e_bvor = vc_bvOrExpr
let e_bvxor = vc_bvXorExpr
let e_bvneg = vc_bvUMinusExpr
let e_bvnot = vc_bvNotExpr

let e_bvextract = vc_bvExtract

(* vc_bvBoolExtract seems to be implemented backwards and unlikely to get fixed
let e_bvbitextract = vc_bvBoolExtract *)
let e_bvbitextract vc child bit =
  e_eq vc (e_bvextract vc child bit bit) (e_bv_of_int vc 1 1)

let e_bvsextend vc w e = vc_bvSignExtend vc e w
(* some shift functions that are a bit more consistent with the other 
 * STP functions *)
let e_bvconstshiftleft vc w e sh_amt =
  if sh_amt = 0
  then e_bvextract vc e (w-1) 0
  else e_bvextract vc (e_bvconcat vc e (e_bv_of_int vc sh_amt 0)) (w-1) 0
let e_bvconstshiftright vc w e sh_amt =
  let shifted = (e_bvextract vc e (w-1) sh_amt) in
  if sh_amt = 0
  then shifted 
  else e_bvconcat vc (e_bv_of_int vc sh_amt 0) shifted
let e_bvconstshiftright_arith vc w e sh_amt = 
  e_bvsextend vc w (e_bvextract vc e (w-1) sh_amt)

(* make a variable shift out of ITE and constant shifts *)
(* w2 is the width of the expression to shift by. Hopefull we can get rid of it
 * soonish. *)
let var_sh csh vc w e w2 sh =
  let make_one n other =
    e_ite vc (e_eq vc sh (e_bv_of_int vc w2 n)) (csh vc w e n) other
  in 
  let rec make_them n last =
    if n = -1 then last
    else make_them (n-1) (make_one n last)
  in
    e_simplify vc (make_them (w-1) (e_bv_of_int vc w 0))

let e_bvshiftleft = var_sh e_bvconstshiftleft
let e_bvshiftright = var_sh e_bvconstshiftright
let e_bvshiftright_arith = var_sh e_bvconstshiftright_arith

let e_bvlt = vc_bvLtExpr
let e_bvle = vc_bvLeExpr
let e_bvgt = vc_bvGtExpr
let e_bvge = vc_bvGeExpr
let e_bvslt = vc_sbvLtExpr
let e_bvsle = vc_sbvLeExpr
let e_bvsgt = vc_sbvGtExpr
let e_bvsge = vc_sbvGeExpr


let do_assert = vc_assertFormula

let query vc exp =
  match vc_query vc exp with
      0 -> false (* invalid *)
    | 1 -> true (* valid *)
    | 2 -> failwith "vc_query failed"
    | _ -> failwith "vc_query returned unexpected result"

let get_counterexample = vc_getCounterExample
let int_of_e = getBVInt
let int64_of_e = getBVUnsignedLongLong

let to_string = exprString
let type_to_string = typeString


(* register the error handler, so errors raise exceptions, rather than
 * exiting the program from C code. *)
let () = libstp_regerrorhandler()



(* OOP interface *)
(* The only advantage of this is that if you don't open Stpvc, you can
 * write "vc#func" rather than "Stpvc.func vc". A nicer approach might be to
 * write a functor, that will return a structure with functions already bound
 * to a vc, so you could open it and just type "func" 
class validity_checker =
object
  val vc = create_validity_checker ()
  method bool_t = bool_t vc
  method array_t = array_t vc
  method bitvector_t = bitvector_t vc

  method exp_var = exp_var vc
  method exp_eq = exp_eq vc
end
*)
