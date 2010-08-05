(** Module for a simple counter. *)

(** Counter type. *)
type t = int ref

(** Make a new counter, optionally giving a starting count (default: 0). *)
let make ?(start=0) () : t = ref start

(** Get the next count in a counter. *)
let next (c : t) = let i = !c in incr c; i
