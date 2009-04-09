open Control.Data
open Control.Monad


(* qualifier variable functor *)
module Qual (V : sig include PrintableComparableType val fresh : int -> t end) (C : PrintableComparableType) = struct
    type var = V.t
    type const = C.t
    type qual = Var of var      (* qualifier variables *)
              | Const of const  (* qualifier constants *)
    type t = qual

    let compare x y = if x == y then 0 else match x, y with
        | Var x, Var y -> V.compare x y
        | Const x, Const y -> C.compare x y
        | x, y ->
            let rank = function Var _ -> 0 | Const _ -> 1 in
            Pervasives.compare (rank x) (rank y)
    let hash = function
        | Var v -> V.hash v
        | Const c -> 1 + C.hash c
    let equal x y = if x == y then true else match x, y with
        | Var x, Var y -> V.equal x y
        | Const x, Const y -> C.equal x y
        | _, _ -> false

    let fresh i = Var (V.fresh i)

    let printer ff = function
        | Var v   -> Format.fprintf ff "Var @[(%a)@]" V.printer v
        | Const c -> Format.fprintf ff "Const @[\"%a\"@]" C.printer c
end

(* qualifier constraint graph type *)
module type Constraints = sig
    module Qual : sig
        type var
        type const
        type qual = Var of var | Const of const
        include PrintableComparableType with type t = qual
        val fresh : int -> t
    end
    val const : Qual.const -> Qual.t

    include Ocamlgraph.Sig.P with type V.t = Qual.t

    type path_checker
    val create_path_checker : t -> path_checker
    val check_path : path_checker -> V.t -> V.t -> bool
    val vertex_printer : Format.formatter -> V.t -> unit
    val edge_printer : Format.formatter -> E.t -> unit
    val printer : Format.formatter -> t -> unit
end


module type QualMonad = sig
    include Monad
    module Constraints : Constraints
    open Constraints

    val eq  : Qual.t -> Qual.t -> unit monad
    val leq : Qual.t -> Qual.t -> unit monad
    val lub : Qual.t -> Qual.t -> Qual.t monad
    val glb : Qual.t -> Qual.t -> Qual.t monad

    val const : Qual.const -> Qual.t monad
    val fresh : Qual.t monad
    val annot : Qual.t -> Qual.const list -> Qual.t monad
end


(* list-based monad-transformer to create and manage qualifiers *)
module QualT (Constraints : Constraints) (M : Monad) = struct
    module Constraints = Constraints
    open Constraints
    (* monad stack *)
    module FreshM = FreshT (Constraints.Qual) (M) (* for generating fresh qualifier variables *)
    module StateFreshM = StateT (Constraints) (FreshM) (* for keeping constraints *)
    include StateFreshM
    module Ops = MonadOps (StateFreshM)
    open Ops

    (* lift monad operations *)
    let lift x = StateFreshM.lift (FreshM.lift x)
    let fresh = StateFreshM.lift FreshM.fresh

    (* qualifier constraints *)
    let eq x y =
        modify (fun g -> add_edge (add_edge g x y) y x)
    let leq x y =
        modify (fun g -> add_edge g x y)
    let lub x y = perform (* least upper bound *)
        z <-- fresh;
        leq x z;
        leq y z;
        return z
    let glb x y = perform (* greatest lower bound *)
        z <-- fresh;
        leq z x;
        leq z y;
        return z

    (* qualifier constructors *)
    let const s = return (const s)

    (* annotate qualifier *)
    let annot qv alist = perform
        mapM_ (fun s -> perform s <-- const s; eq qv s) alist;
        return qv
end

