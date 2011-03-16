(** Ocaml bindings to STP. *)


(** {2 Types} *)

(** An STP context. *)
type context

(** A typed STP expression: the parameters of [('type, 'kind) expr] are phantom types to enforces correct API usage:
    - ['type] is the type of the STP expression which may be a boolean [[`bool]], a bitvector [[`bv]], or an array [[`array]];
    - ['kind] indicates whether the STP expression is an expression [[`expr]], a variable [[`var]], or a constant [[`const]].
*)
type ('a, 'b) expr

(** The result of an STP query. *)
type validity = Invalid | Valid | Undecided


(** {2 STP context} *)

(** [make_context ()] creates a new STP context. *)
external make_context : unit -> context = "OcamlSTP_make_context"

(** [set_seed stp seed] sets an optional seed for the internal random number generator used in the STP context [stp]. *)
external set_seed : context -> int option -> unit = "OcamlSTP_set_seed"


(** {2 Variables} *)

(** [bool_var stp name] looks up or creates a boolean variable named [name] in the STP context [stp]. @raise Invalid_arg is a non-boolean variable with the same name exists. *)
external bool_var : context -> string -> ([`bool], [>`var]) expr = "OcamlSTP_bool_var"

(** [bv_var stp name width] looks up or creates a bitvector variable named [name] of bit-width [width] in the STP context [stp]. @raise Invalid_arg is a non-bitvector variable or bitvector variable of different width with the same name exists. *)
external bv_var : context -> string -> int -> ([`bv], [>`var]) expr = "OcamlSTP_bv_var"

(** [array_var stp name index_width val_width] looks up or creates an array variable named [name] of with an index of bit-width [index_width] and values of bit-width [val_width] in the STP context [stp]. @raise Invalid_arg is a non-array variable or array variable of different widths with the same name exists. *)
external array_var : context -> string -> int -> int -> ([`array], [>`var]) expr = "OcamlSTP_array_var"


(** {2 Boolean constants} *)

(** [bool_true stp] returns a true boolean constant in the STP context [stp]. *)
external bool_true : context -> ([`bool], [>`const]) expr = "OcamlSTP_bool_true"

(** [bool_false stp] returns a false boolean constant in the STP context [stp]. *)
external bool_false : context -> ([`bool], [>`const]) expr = "OcamlSTP_bool_false"


(** {2 Bitvector constants} *)

(** [bv_of_int stp width val] returns a constant bitvector of bit-width [width] and value [val] from an [int] in the STP context [stp]. *)
external bv_of_int : context -> int -> int -> ([`bv], [>`const]) expr = "OcamlSTP_bv_of_int"

(** [bv_of_int32 stp width val] returns a constant bitvector of bit-width [width] and value [val] from an [int32] in the STP context [stp]. *)
external bv_of_int32 : context -> int -> int32 -> ([`bv], [>`const]) expr = "OcamlSTP_bv_of_int32"

(** [bv_of_int64 stp width val] returns a constant bitvector of bit-width [width] and value [val] from an [int64] in the STP context [stp]. *)
external bv_of_int64 : context -> int -> int64 -> ([`bv], [>`const]) expr = "OcamlSTP_bv_of_int64"

(** [bv_of_string stp width val_string base] returns a constant bitvector of bit-width [width] and value [val_string] represented as a string of base [base] in the STP context [stp]. *)
external bv_of_string : context -> int -> string -> int -> ([`bv], [>`const]) expr = "OcamlSTP_bv_of_int_string"


(** {2 Boolean expressions} *)

(** [bool_not stp expr] returns the boolean [not expr] in the STP context [stp]. *)
external bool_not : context -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bool_not"

(** [bool_and stp left right] returns the boolean [left && right] in the STP context [stp]. *)
external bool_and : context -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bool_and"

(** [bool_or stp left right] returns the boolean [left || right] in the STP context [stp]. *)
external bool_or : context -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bool_or"

(** [bool_nand stp left right] returns the boolean [not (left && right)] in the STP context [stp]. *)
external bool_nand : context -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bool_nand"

(** [bool_nor stp left right] returns the boolean [not (left || right)] in the STP context [stp]. *)
external bool_nor : context -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bool_nor"

(** [bool_xor stp left right] returns the boolean [left xor right] in the STP context [stp]. *)
external bool_xor : context -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bool_xor"

(** [bool_iff stp left right] returns the boolean [left iff right] (or equivalently, [left xnor right]) in the STP context [stp]. *)
external bool_iff : context -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bool_iff"

(** [bool_implies stp left right] returns the boolean [left implies right] (or equivalently, [not left or right]) in the STP context [stp]. *)
external bool_implies : context -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bool_implies"


(** {2 Bitvector arithmetic expressions} *)

(** [bv_neg stp expr] returns the bitvector [-expr] in the STP context [stp]. *)
external bv_neg : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_neg"

(** [bv_add stp left right] returns the bitvector [left + right] in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_add : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_add"

(** [bv_sub stp left right] returns the bitvector [left - right] in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_sub : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_sub"

(** [bv_mul stp left right] returns the bitvector [left * right] in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_mul : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_mul"

(** [bv_div stp left right] returns the bitvector [left / right] in the STP context [stp]. {b Terminates ungracefully if [right] is 0.} @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_div : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_div"

(** [bv_rem stp left right] returns the bitvector of the remainder of [left / right] (i.e. [left mod right]) in the STP context [stp]. {b Terminates ungracefully if [right] is 0.} @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_rem : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_rem"

(** [bv_signed_div stp left right] returns the bitvector of the signed division [left / right] in the STP context [stp]. {b Terminates ungracefully if [right] is 0.} @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_signed_div : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_signed_div"

(** [bv_signed_rem stp left right] returns the bitvector of the signed remainder of [left / right] rounded towards zero (e.g., -10 rem 3 = -1; i.e. [left mod right]) in the STP context [stp]. {b Terminates ungracefully if [right] is 0.} @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_signed_rem : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_signed_rem"

(** [bv_signed_mod stp left right] returns the bitvector of the signed remainder of [left]/[right] rounded towards -infinity (e.g., -10 mod 3 = 2) in the STP context [stp]; unless it divides exactly and the signs differ, then it returns [right]. {b Terminates ungracefully if [right] is 0.} @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_signed_mod : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_signed_mod"


(** {2 Bitvector bitwise expressions} *)

(** [bv_shift_left stp expr shift] returns the bitvector [expr] shifted left by [shift] (i.e. [expr lsl shift]) in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_shift_left : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_shift_left"

(** [bv_shift_right stp expr shift] returns the bitvector [expr] shifted right by [shift] (i.e. [expr lsr shift]) in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_shift_right : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_shift_right"

(** [bv_signed_shift_right stp expr shift] returns the bitvector [expr] shifted right by [shift] by replicating the sign bit (i.e. [expr asr shift]) in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_signed_shift_right : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_signed_shift_right"

(** [bv_not stp expr] returns the bitvector of bitwise [not expr] (i.e., [lnot expr]) in the STP context [stp]. *)
external bv_not : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_not"

(** [bv_and stp left right] returns the bitvector of bitwise [left && right] (i.e. [left land right]) in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_and : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_and"

(** [bv_or stp left right] returns the bitvector of bitwise [left || right] (i.e. [left lor right]) in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_or : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_or"

(** [bv_xor stp left right] returns the bitvector of bitwise [left xor right] (i.e., [left lxor right]) in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_xor : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_xor"

(* Buggy
external bv_nand : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_nand"
external bv_nor : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_nor"
external bv_xnor : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_xnor"
*)


(** {2 Bitvector slicing/extension} *)

(** [bv_get_bit stp expr index] returns the boolean of the [index]th bit of [expr] in the STP context [stp]. *)
external bv_get_bit : context -> ([`bv], [<`expr|`const|`var]) expr -> int -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_get_bit"

(** [bv_concat stp left right] returns the bitvector concatenation of [left] and [right] in the STP context [stp]. *)
external bv_concat : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_concat"

(** [bv_extract stp expr high_bit low_bit] returns the bitvector slice from [high_bit] to [low_bit] inclusive in the STP context [stp]. *)
external bv_extract : context -> ([`bv], [<`expr|`const|`var]) expr -> int -> int -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_extract"

(** [bv_zero_extend stp expr width] returns the bitvector [expr] extended to [width] bits in the STP context [stp]. *)
external bv_zero_extend : context -> ([`bv], [<`expr|`const|`var]) expr -> int -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_zero_extend"

(** [bv_zero_extend stp expr width] returns the bitvector [expr] extended to [width] bits by replicating the sign bit in the STP context [stp]. *)
external bv_sign_extend : context -> ([`bv], [<`expr|`const|`var]) expr -> int -> ([`bv], [>`expr]) expr = "OcamlSTP_bv_sign_extend"


(** {2 Bitvector comparison} *)

(** [bv_eq stp left right] returns the boolean of [left = right] in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_eq : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_eq"

(** [bv_lt stp left right] returns the boolean of [left < right] in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_lt : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_lt"

(** [bv_le stp left right] returns the boolean of [left <= right] in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_le : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_le"

(** [bv_gt stp left right] returns the boolean of [left > right] in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_gt : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_gt"

(** [bv_ge stp left right] returns the boolean of [left >= right] in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_ge : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_ge"

(** [bv_signed_lt stp left right] returns the boolean of signed [left < right] in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_signed_lt : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_signed_lt"

(** [bv_signed_le stp left right] returns the boolean of signed [left <= right] in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_signed_le : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_signed_le"

(** [bv_signed_gt stp left right] returns the boolean of signed [left > right] in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_signed_gt : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_signed_gt"

(** [bv_signed_ge stp left right] returns the boolean of signed [left >= right] in the STP context [stp]. @raise Invalid_arg is the widths of [left] and [right] differs. *)
external bv_signed_ge : context -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bool], [>`expr]) expr = "OcamlSTP_bv_signed_ge"


(** {2 Array operations} *)

(** [array_read stp array index] returns the bitvector of the [index]th element of [array] in the STP context [stp]. *)
external array_read : context -> ([`array], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [>`expr]) expr = "OcamlSTP_array_read"

(** [array_write stp array index value] returns the array of [array] with the [index]th element replaced with the bitvector [value] in the STP context [stp]. *)
external array_write : context -> ([`array], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`bv], [<`expr|`const|`var]) expr -> ([`array], [>`expr]) expr = "OcamlSTP_array_write"


(** {2 Conditional expressions} *)

(** [ite stp guard then else] returns a conditional expression that has the value [then] if [guard] is true, or [else] otherwise, in the STP context [stp]. *)
external ite : context -> ([`bool], [<`expr|`const|`var]) expr -> ('a, [<`expr|`const|`var]) expr -> ('a, [<`expr|`const|`var]) expr -> ('a, [>`expr]) expr = "OcamlSTP_ite"


(** {2 Validity checking} *)

(** [vc_assert stp expr] asserts the boolean [expr] in the STP context [stp]. *)
external vc_assert : context -> ([`bool], [<`expr|`const|`var]) expr -> unit = "OcamlSTP_vc_assert"

(** [vc_push stp] saves the current state of the STP context [stp] (i.e., the list of assertions). *)
external vc_push : context -> unit = "OcamlSTP_vc_push"

(** [vc_pop stp] restores the most recently saved state of the STP context [stp] (i.e., the list of assertions). *)
external vc_pop : context -> unit = "OcamlSTP_vc_pop"

(** [vc_query stp expr] returns [Valid] if the boolean [expr] is true in the STP context [stp] (i.e., [assertions implies expr] is true), or [Invalid] if it cannot be determined (i.e., [assertions implies expr] is may be true or false). *)
external vc_query : context -> ([`bool], [<`expr|`const|`var]) expr -> validity = "OcamlSTP_vc_query"

(** [vc_get_asserts stp] get the current list of assertions in the STP context [stp]. *)
external vc_get_asserts : context -> ([`bool], [>`expr]) expr list = "OcamlSTP_vc_get_asserts"

(** [vc_get_counterexample stp var] returns [Some x] where [x] is counterexample value for the variable [var] if the previous query is [Invalid], or [None] if the previous query is [Valid]. *)
external vc_get_counterexample : context -> ('a, [<`var]) expr -> ('a, [>`const]) expr option = "OcamlSTP_vc_get_counterexample"


(** {2 Metadata query} *)

(** [bv_width stp expr] returns the bit-width of the bitvector [expr] in the STP context [stp]. *)
external bv_width : context -> ([`bv], [<`expr|`const|`var]) expr -> int = "OcamlSTP_bv_width"

(** [array_value_width stp array] returns the bit-width of the values of [array] in the STP context [stp]. *)
external array_value_width : context -> ([`array], [<`expr|`const|`var]) expr -> int = "OcamlSTP_array_value_width"

(** [array_index_width stp array] returns the bit-width of the index of [array] in the STP context [stp]. *)
external array_index_width : context -> ([`array], [<`expr|`const|`var]) expr -> int = "OcamlSTP_array_index_width"


(** {2 Value conversion} *)

(** [to_bool expr] returns the [bool] value of the constant [expr]. *)
external to_bool : context -> ([`bool], [<`const]) expr -> bool = "OcamlSTP_to_bool"

(** [to_int expr] returns the [int] value of the constant [expr]. *)
external to_int : context -> ([`bv], [<`const]) expr -> int = "OcamlSTP_to_int"

(** [to_int32 expr] returns the [int32] value of the constant [expr]. *)
external to_int32 : context -> ([`bv], [<`const]) expr -> int32 = "OcamlSTP_to_int32"

(** [to_int64 expr] returns the [int64] value of the constant [expr]. *)
external to_int64 : context -> ([`bv], [<`const]) expr -> int64 = "OcamlSTP_to_int64"

(** [to_string expr] returns the string representation of [expr]. *)
external to_string : context -> ('a, 'b) expr -> string = "OcamlSTP_to_string"

