open Data
open Monad
open Ocamlgraph

module type VertexLabel = PrintableComparableType
module type EdgeLabel = sig
    include PrintableOrderedType
    val default : t
end


(** Concrete version of Sig.VERTEX *)
module type VertexType = sig
    type t
    val compare : t -> t -> int
    val hash : t -> int
    val equal : t -> t -> bool
    type label = t
    val create : label -> t
    val label : t -> label
end
module type EdgeType = Sig.EDGE


(** Graph signature similar to Sig.P, but without the V and E modules; useful for overriding V and E *)
module type PrintableGraphType = sig
    type t
    type vertex
    type edge
    val is_directed : bool
    val is_empty : t -> bool
    val nb_vertex : t -> int
    val nb_edges : t -> int
    val out_degree : t -> vertex -> int
    val in_degree : t -> vertex -> int
    val mem_vertex : t -> vertex -> bool
    val mem_edge : t -> vertex -> vertex -> bool
    val mem_edge_e : t -> edge -> bool
    val find_edge : t -> vertex -> vertex -> edge
    val succ : t -> vertex -> vertex list
    val pred : t -> vertex -> vertex list
    val succ_e : t -> vertex -> edge list
    val pred_e : t -> vertex -> edge list
    val iter_vertex : (vertex -> unit) -> t -> unit
    val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_edges : (vertex -> vertex -> unit) -> t -> unit
    val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_edges_e : (edge -> unit) -> t -> unit
    val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
    val map_vertex : (vertex -> vertex) -> t -> t
    val iter_succ : (vertex -> unit) -> t -> vertex -> unit
    val iter_pred : (vertex -> unit) -> t -> vertex -> unit
    val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val fold_pred : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_succ_e : (edge -> unit) -> t -> vertex -> unit
    val fold_succ_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_pred_e : (edge -> unit) -> t -> vertex -> unit
    val fold_pred_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val empty : t
    val add_vertex : t -> vertex -> t
    val remove_vertex : t -> vertex -> t
    val add_edge : t -> vertex -> vertex -> t
    val add_edge_e : t -> edge -> t
    val remove_edge : t -> vertex -> vertex -> t
    val remove_edge_e : t -> edge -> t
    val vertex_printer : Format.formatter -> vertex -> unit
    val edge_printer : Format.formatter -> edge -> unit
    val printer : Format.formatter -> t -> unit
end


(** Graph monad signature *)
module type GraphMonad = sig
    module Graph : sig
        module V : VertexType
        module E : EdgeType
        include PrintableGraphType with type vertex = V.t
                                    and type edge = E.t
    end

    include Monad
    val add_edge : ?label:Graph.E.label -> Graph.V.t -> Graph.V.t -> unit monad
end


(** Monad wrapper for Ocamlgraph's persistent, labeled graphs *)
module BidirectionalGraphT (VertexLabel : VertexLabel) (EdgeLabel : EdgeLabel) (M : Monad) = struct
    module Graph = struct
        include Persistent.Digraph.ConcreteBidirectionalLabeled (VertexLabel) (EdgeLabel)

        let vertex_printer = VertexLabel.printer
        let edge_printer ff e = EdgeLabel.printer ff (E.label e)
        let printer ff graph = ignore begin
            let printer ff e = Format.fprintf ff "@[%a@]@ @[<><=%a=@] @[%a@]"
                vertex_printer (E.dst e) edge_printer e vertex_printer (E.src e) in
            fold_edges_e (fun e b -> Format.fprintf ff "%(%)@[<2>%a@]" b printer e; ",@ ") graph ""
        end
    end

    include StateT (Graph) (M)
    let add_edge ?label x y = match label with
        | Some l -> modify (fun g -> Graph.add_edge_e g (Graph.E.create x l y))
        | None -> modify (fun g -> Graph.add_edge g x y)
end


module type Context = EdgeLabel


module type ContextualEdge = sig
    module Context : Context
    module Data : EdgeLabel
    include EdgeLabel with type t = Context.t * Data.t
    val context : t -> Context.t
    val data : t -> Data.t
end


module ContextualEdge (C : Context) (D : EdgeLabel) = struct
    module Context = C
    module Data = D
    type t = Context.t * Data.t
    let context = fst
    let data = snd

    let compare (xc, xe) (yc, ye) = match Context.compare xc yc with
        | 0 -> Data.compare xe ye
        | i -> i
    let default = (Context.default, Data.default)
    let printer ff e = Format.fprintf ff "%a:%a" Context.printer (context e) Data.printer (data e)
end


module ContextualEdgeGraphT (CE : ContextualEdge)
        (G : GraphMonad with type Graph.E.label = CE.t) = struct
    include ReaderT (CE.Context) (G)

    module Graph = struct
        open G.Graph
        module V = V
        module E = struct
            type t = E.t
            let compare = E.compare
            type vertex = E.vertex
            let src = E.src
            let dst = E.dst
            type label = CE.Data.t
            let create x l y = E.create x (CE.Context.default, l) y
            let context e = fst (E.label e)
            let data e = snd (E.label e)
            let label = data
        end
        include (G.Graph : PrintableGraphType with type t = G.Graph.t
                                               and type vertex = G.Graph.vertex
                                               and type edge = G.Graph.edge)
    end

    let add_edge ?label x y = perform
        context <-- ask;
        match label with
            | Some l -> lift (G.add_edge ~label:(context, l) x y)
            | None -> lift (G.add_edge ~label:(context, CE.Data.default) x y)
end

