(** Targets currently include 
 *   - Failure: when __FAILURE() is called
 *   - Coverage: One of {Line, Edge, StmtInfo, Cond}
 *)

(* It's arguable whether this module should be put in OtterCore. But right now it has to. *)
type t = 
    | Failure 
    | Coverage of CoverageData.t

