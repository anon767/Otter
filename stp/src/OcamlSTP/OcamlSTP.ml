
type context
type ('a, 'b) expr
type validity = Invalid | Valid | Undecided

external init : unit -> unit = "OcamlSTP_init"
let () = init ()

external make_context : unit -> context = "OcamlSTP_make_context"

external bool_var : context -> string -> ([`bool], [>`var]) expr = "OcamlSTP_bool_var"
external bv_var : context -> string -> int -> ([`bv], [>`var]) expr = "OcamlSTP_bv_var"
external array_var : context -> string -> int -> int -> ([`array], [>`var]) expr = "OcamlSTP_array_var"

external bool_true : context -> ([`bool], [>`const]) expr = "OcamlSTP_bool_true"
external bool_false : context -> ([`bool], [>`const]) expr = "OcamlSTP_bool_false"

external bv_of_int : context -> int -> int -> ([`bv], [>`const]) expr = "OcamlSTP_bv_of_int"
external bv_of_int32 : context -> int -> int32 -> ([`bv], [>`const]) expr = "OcamlSTP_bv_of_int32"
external bv_of_int64 : context -> int -> int64 -> ([`bv], [>`const]) expr = "OcamlSTP_bv_of_int64"
external bv_of_string : context -> int -> string -> int -> ([`bv], [>`const]) expr = "OcamlSTP_bv_of_int_string"

external bool_not : context -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bool_not"
external bool_and : context -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bool_and"
external bool_or : context -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bool_or"
external bool_nand : context -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bool_nand"
external bool_nor : context -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bool_nor"
external bool_xor : context -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bool_xor"
external bool_iff : context -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bool_iff"
external bool_implies : context -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bool_implies"

external bv_neg : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_neg"
external bv_add : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_add"
external bv_sub : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_sub"
external bv_mul : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_mul"
external bv_div : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_div"
external bv_rem : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_rem"
external bv_signed_div : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_signed_div"
external bv_signed_rem : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_signed_rem"
external bv_signed_mod : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_signed_mod"

external bv_shift_left : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_shift_left"
external bv_shift_right : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_shift_right"
external bv_signed_shift_right : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_signed_shift_right"
external bv_not : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_not"
external bv_and : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_and"
external bv_or : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_or"
external bv_xor : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_xor"
(* Buggy
external bv_nand : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_nand"
external bv_nor : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_nor"
external bv_xnor : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_xnor"
*)

external bv_get_bit : context -> ([`bv], [<`expr|`const|`var]) expr -> int -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_get_bit"
external bv_concat : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_concat"
external bv_extract : context -> ([`bv], [<`expr|`const|`var]) expr -> int -> int -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_extract"
external bv_zero_extend : context -> ([`bv], [<`expr|`const|`var]) expr -> int -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_zero_extend"
external bv_sign_extend : context -> ([`bv], [<`expr|`const|`var]) expr -> int -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_sign_extend"

external bv_eq : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_eq"
external bv_lt : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_lt"
external bv_le : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_le"
external bv_gt : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_gt"
external bv_ge : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_ge"
external bv_signed_lt : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_signed_lt"
external bv_signed_le : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_signed_le"
external bv_signed_gt : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_signed_gt"
external bv_signed_ge : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_signed_ge"

external array_read : context -> ([`array], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_array_read"
external array_write : context -> ([`array], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`array], [>`expr]) expr = "OcamlSTP_array_write"

external ite : context -> ([`bool], [<`expr|`const|`var]) expr -> ('a, [<`expr|`const|`var]) expr -> ('a, [<`expr|`const|`var]) expr -> ('a, [>`expr]) expr = "OcamlSTP_ite"

external vc_assert : context -> ([`bool], [<`expr|`const|`var]) expr -> unit = "OcamlSTP_vc_assert"
external vc_push : context -> unit = "OcamlSTP_vc_push"
external vc_pop : context -> unit = "OcamlSTP_vc_pop"
external vc_query : context -> ([`bool], [<`expr|`const|`var]) expr -> validity = "OcamlSTP_vc_query"

external vc_get_asserts : context -> ([`bool], [>`expr]) expr list = "OcamlSTP_vc_get_asserts"
external vc_get_counterexample : context -> ('a, [<`var]) expr -> ('a, [>`const]) expr option = "OcamlSTP_vc_get_counterexample"

external bv_width : context -> ([`bv], [<`expr|`const|`var]) expr -> int = "OcamlSTP_bv_width"
external array_value_width : context -> ([`array], [<`expr|`const|`var]) expr -> int = "OcamlSTP_array_value_width"
external array_index_width : context -> ([`array], [<`expr|`const|`var]) expr -> int = "OcamlSTP_array_index_width"

external to_bool : context -> ([`bool], [<`const]) expr -> bool = "OcamlSTP_to_bool"

external to_int : context -> ([`bv], [<`const]) expr -> int = "OcamlSTP_to_int"
external to_int32 : context -> ([`bv], [<`const]) expr -> int32 = "OcamlSTP_to_int32"
external to_int64 : context -> ([`bv], [<`const]) expr -> int64 = "OcamlSTP_to_int64"

external to_string : context -> ('a, 'b) expr -> string = "OcamlSTP_to_string"

