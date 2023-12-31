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
        module E : EdgeType with type t = edge and type vertex = vertex and type label = Constraint.t
    end with module Qual = Qual
    val add_edge : ?label:QualGraph.Constraint.t -> Qual.t -> Qual.t -> unit monad

    val fresh : Qual.t monad

    val eq  : Qual.t -> Qual.t -> unit monad
    val leq : Qual.t -> Qual.t -> unit monad
    val lub : Qual.t -> Qual.t -> Qual.t monad
    val glb : Qual.t -> Qual.t -> Qual.t monad

    val annot : Qual.t -> Qual.Const.t -> Qual.t monad
    val has_annot : Qual.t -> bool monad
end


(* list-based monad-transformer to create and manage qualifiers *)
module QualT (Q : Qual) (C : Constraint)
        (G : GraphMonad with type Graph.V.t = Q.t and type Graph.E.label = C.t) = struct
    module Qual = Q
    module FreshM = struct (* for generating fresh qualifier variables *)
        include FreshT (Qual) (G)
        (* lift monad operations *)
        let add_edge ?label x y = lift (G.add_edge ?label x y)
        let succ x = lift (G.succ x)
        let pred x = lift (G.pred x)
    end
    include FreshM
    module Ops = MonadOps (FreshM)
    open Ops

    module QualGraph = struct
        include G.Graph
        module Qual = Qual
        module Constraint = C

        module Automaton = struct
            type t = Start | Walk of edge
            let start = Start
            let accept _ = true
            let compare _ _ = 0
            let printer ff = function
                | Start -> ()
                | Walk e -> E.printer ff e
        end
        let fold_forward f g v automaton acc = fold_succ_e (fun e acc -> f (E.dst e) (Automaton.Walk e) acc) g v acc
        let fold_backward f g v automaton acc = fold_pred_e (fun e acc -> f (E.src e) (Automaton.Walk e) acc) g v acc
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

    let has_annot qv = perform
        let is_constM dir e = match dir e with
            | Qual.Const _ -> return true
            | Qual.Var _ -> return false
        in
        nodes <-- succ qv;
        result <-- existsM (is_constM QualGraph.E.dst) nodes;
        if result then
            return true
        else perform
            nodes <-- pred qv;
            existsM (is_constM QualGraph.E.src) nodes 
end

