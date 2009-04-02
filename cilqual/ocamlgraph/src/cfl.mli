(** Check for a path. *)
module Check
    (G : sig
        type t
        type cfl
        module V : Sig.COMPARABLE
        val start_cfl : cfl
        val accept_cfl : cfl -> bool
        val iter_cfl : (V.t -> cfl -> unit) -> t -> V.t -> cfl -> unit
    end) : sig

  type path_checker
  val create : G.t -> path_checker
  val check_path : path_checker -> G.V.t -> G.V.t -> bool
end
