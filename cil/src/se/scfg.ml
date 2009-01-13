(* module of a sub-cfg *)
open Cil

type t = Include of stmt list | Exclude of stmt list
;;

let universe = Exclude ([])
;;