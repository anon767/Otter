open Control.Data
open Control.Graph
open QualGraph

module Reachability = struct
    module SourceSink (G : sig include QualGraphAutomataType module V : VertexType with type t = vertex end) = struct
        module CflGraph = struct
            include G
            type cfl = Automata.t
            let start_cfl = Automata.start
            let accept_cfl = Automata.accept
            let iter_cfl f g v cfl = fold_forward (fun v cfl () -> f v cfl) g v cfl ()
        end
        module PathChecker = Ocamlgraph.Cfl.Check (CflGraph)
        let create = PathChecker.create
        let check pc x y =
            try PathChecker.check_path pc x y
            with Invalid_argument s -> false
    end
end

