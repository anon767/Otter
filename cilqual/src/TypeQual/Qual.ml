open Control.Data
open Control.Monad
open Control.Graph

open QualGraph


(** qualifier node functor *)
module Qual (Var : QualVar) (Const : QualConst) = struct
    module Var = Var
    module Const = Const
    type t = Var of Var.t      (* qualifier variables *)
           | Const of Const.t  (* qualifier constants *)

    let fresh i = Var (Var.fresh i)

    let projvar = function
        | Var v -> v
        | Const _ -> failwith "TODO: report projvar of Const"

    let compare x y = if x == y then 0 else match x, y with
        | Var x, Var y -> Var.compare x y
        | Const x, Const y -> Const.compare x y
        | x, y ->
            let rank = function Var _ -> 0 | Const _ -> 1 in
            Pervasives.compare (rank x) (rank y)
    let hash = function
        | Var v -> Var.hash v
        | Const c -> 1 + Const.hash c
    let equal x y = if x == y then true else match x, y with
        | Var x, Var y -> Var.equal x y
        | Const x, Const y -> Const.equal x y
        | _, _ -> false

    let printer ff = function
        | Var v   -> Format.fprintf ff "Var @[(%a)@]" Var.printer v
        | Const c -> Format.fprintf ff "Const @[\"%a\"@]" Const.printer c
end


(** qualifier constraint functor *)
module Constraint (C : Constraint) = C


module type QualMonad = sig
    include Monad
    module Qual : Qual
    module QualGraph : sig
        include QualGraphAutomatonType
        module V : VertexType with type t = vertex
        module E : EdgeType with type t = edge and type label = Constraint.t
    end with module Qual = Qual
    val add_edge : ?label:QualGraph.Constraint.t -> Qual.t -> Qual.t -> unit monad

    val fresh : Qual.t monad

    val eq  : Qual.t -> Qual.t -> unit monad
    val leq : Qual.t -> Qual.t -> unit monad
    val lub : Qual.t -> Qual.t -> Qual.t monad
    val glb : Qual.t -> Qual.t -> Qual.t monad

    val annot : Qual.t -> Qual.Const.t -> Qual.t monad
end


(* list-based monad-transformer to create and manage qualifiers *)
module QualT (Q : Qual) (C : Constraint)
        (G : GraphMonad with type Graph.V.t = Q.t and type Graph.E.label = C.t) = struct
    module Qual = Q
    module FreshM = struct (* for generating fresh qualifier variables *)
        include FreshT (Qual) (G)
        (* lift monad operations *)
        let add_edge ?label x y = lift (G.add_edge ?label x y)
    end
    include FreshM

    module QualGraph = struct
        include G.Graph
        module Qual = Qual
        module Constraint = C

        module Automaton = struct
            include Unit
            let start = ()
            let accept () = true
        end
        let fold_forward f g v automaton acc = fold_succ (fun v acc -> f v automaton acc) g v acc
        let fold_backward f g v automaton acc = fold_pred (fun v acc -> f v automaton acc) g v acc
    end

    (* qualifier constraints *)
    let eq x y = perform
        add_edge x y;
        add_edge y x
    let leq x y = perform
        add_edge x y
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

    (* annotate qualifier *)
    let annot qv c = perform
        eq qv (Qual.Const c);
        return qv
end

