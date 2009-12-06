open Control.Data
open Control.Graph
open Control.UnionFind
open QualGraph

module Reachability = struct
    module SourceSink (G : sig include QualGraphAutomatonType module V : VertexType with type t = vertex end) = struct
        module CflGraph = struct
            include G
            type cfl = Automaton.t
            let start_cfl = Automaton.start
            let accept_cfl = Automaton.accept
            let iter_cfl f g v cfl = fold_forward (fun v cfl () -> f v cfl) g v cfl ()
        end
        module PathChecker = Ocamlgraph.Cfl.Check (CflGraph)
        let create = PathChecker.create
        let check pc x y =
            try PathChecker.check_path pc x y
            with Invalid_argument s -> false
    end
end


module Traversal (G : sig include QualGraphAutomatonType module V : VertexType with type t = vertex end) = struct
    module AutomatonState = struct
        type t = G.Qual.Var.t * G.Automaton.t
        let compare (x1, x2) (y1, y2) = match G.Qual.Var.compare x1 y1 with
            | 0 -> G.Automaton.compare x2 y2
            | i -> i
    end

    module VisitedSet = Set.Make (AutomatonState)

    (* find all adjacent, accepted quals *)
    (* TODO: replace find_adjacent_accept with CFL-reachability *)
    let fold_adjacent_accept fold f node constraints acc =
        let rec fold_adjacent_accept visited acc = function
            | (part, automaton)::partwork ->
                (* traverse adjacents of part *)
                let visited, acc, partwork =
                    fold begin fun v automaton (visited, acc, partwork) -> match v with
                        | G.Qual.Var a when not (VisitedSet.mem (a, automaton) visited) ->
                            let visited = VisitedSet.add (a, automaton) visited in
                            if G.Automaton.accept automaton then
                                (* could have duplicates, but the result is usually added to sets anyway *)
                                (visited, f a acc, partwork)
                            else
                                (* not visited and not accepted yet; so add to work list *)
                                (visited, acc, (v, automaton)::partwork)
                        | _ ->
                            (visited, acc, partwork)
                    end constraints part automaton (visited, acc, partwork)
                in

                (* and recurse *)
                fold_adjacent_accept visited acc partwork

            | [] ->
                (* done *)
                acc
        in
        fold_adjacent_accept VisitedSet.empty acc [(node, G.Automaton.start)]

    let fold_forward_accept f node constraints acc = fold_adjacent_accept G.fold_forward f node constraints acc
    let fold_backward_accept f node constraints acc = fold_adjacent_accept G.fold_backward f node constraints acc
end


(*
upper-closure(a) = {b | a <= b}
upper-closure(S) = \union_{s \in S} upper-closure(s)

Qual-solve(C) =
    for all k \in C do S(k) := Q
    for all q \in Q do S(q) := {q}
    let C' = C
    while C /= \emptyset do
        remove an L <= R from C'
        let S'L = S(L) \intersect lower-closure(S(R))
        let S'R = S(R) \intersect upper_closure(S(L))
        if S'L = \emptyset or S'R = \emptyset then
            return unsatisfiable
        if S(L) /= S'L then
            S(L) := S'L
            Add each L' <= L and L <= R' in C to C'
        if S(R) /= S'R then
            S(R) := S'R
            Add each L' <= R and R <= R' in C to C'
        return S
*)

module DiscreteOrder (G : sig include QualGraphAutomatonType module V : VertexType with type t = vertex end) = struct
    include Traversal (G)

    module ConstSet = Set.Make (G.Qual.Const)
    module VarSet = Set.Make (G.Qual.Var)

    module Solution = struct
        module SolutionMap = Map.Make (G.Qual.Var)
        module Unsolvables = VarSet
        module EquivalenceClasses = UnionFindClassMap (G.Qual.Var)

        type t = {
            consts : ConstSet.t;                 (* the discrete order set *)
            constraints : G.t;                   (* the constraints from which the solution is derived *)
            solution : ConstSet.t SolutionMap.t; (* the map from qualifier variables to their solution *)
            unsolvables : Unsolvables.t;         (* unsolvable qualifier variables *)
            unsolvables_classes : VarSet.t EquivalenceClasses.t;
                                                 (* unsolvables classified by common annotated variables *)
        }

        let empty consts constraints = {
            consts = consts;
            constraints = constraints;
            solution = SolutionMap.empty;
            unsolvables = Unsolvables.empty;
            unsolvables_classes = EquivalenceClasses.empty;
        }

        let consts result = result.consts
        let constraints result = result.constraints
        let unsolvables result = result.unsolvables
        let unsolvables_classes result = result.unsolvables_classes

        let const c = ConstSet.singleton c

        let find v result = SolutionMap.find v result.solution

        let is_unsatisfiable result = not (Unsolvables.is_empty result.unsolvables)

        let get_annotations v result = ConstSet.fold begin fun c a ->
            if G.mem_edge result.constraints (G.Qual.Var v) (G.Qual.Const c)
                    || G.mem_edge result.constraints (G.Qual.Const c) (G.Qual.Var v) then
                c::a
            else
                a
        end result.consts []

        let is_annotated v result = get_annotations v result <> []

        let is_constrained v result = SolutionMap.mem v result.solution

        let is_unsolvable v result = try ConstSet.is_empty (find v result) with Not_found -> false

        let equal_const v c result = try ConstSet.equal (find v result) (const c) with Not_found -> false

        let equate_class x y result =
            (* only bother to update for unsolvables *)
            if not (is_unsolvable x result && is_unsolvable y result) then
                result
            else
                let classes = result.unsolvables_classes in
                let ax = is_annotated x result in
                let x, y = if ax then (y, x) else (x, y) in
                let classes = if ax <> is_annotated y result then
                    (* x is not annotated, y is annotated *)
                    try
                        EquivalenceClasses.add x (VarSet.add y (EquivalenceClasses.find x classes)) classes
                    with Not_found ->
                        EquivalenceClasses.add x (VarSet.singleton y) classes
                else
                    (* x and y are either both annotated or both not annotated *)
                    let combine classes =
                        let x, y, unified, classes = EquivalenceClasses.union x y classes in
                        match unified with
                            | Some (sx, sy) -> EquivalenceClasses.add x (VarSet.union sx sy) classes
                            | None -> classes
                    in try
                        combine classes
                    with Not_found ->
                        let fix u classes = if EquivalenceClasses.mem u classes then classes else
                            EquivalenceClasses.add u (if ax then VarSet.singleton u else VarSet.empty) classes
                        in
                        combine (fix x (fix y classes))
                in
                { result with unsolvables_classes = classes }

        let add var consts result =
            let solution = SolutionMap.add var consts result.solution in
            let unsolvables = if ConstSet.is_empty consts && is_annotated var result then
                Unsolvables.add var result.unsolvables
            else
                result.unsolvables
            in
            { result with solution = solution; unsolvables = unsolvables }

        let printer ff result =
            let set_printer fold elt_printer ff set =
                let pre = format_of_string "" in
                let br = format_of_string ",@ " in
                let set_printer ff = ignore begin fold
                    (fun x b -> Format.fprintf ff "%(%)@[%a@]" b elt_printer x; br) set pre
                end in
                Format.fprintf ff "{ %t@ }" set_printer
            in
            let pre = format_of_string "" in
            let br = format_of_string "@\n" in
            let unsolvables_printer = set_printer Unsolvables.fold G.Qual.Var.printer in
            let const_set_printer = set_printer ConstSet.fold G.Qual.Const.printer in
            let solution_printer ff = ignore begin SolutionMap.fold begin fun v c b ->
                if not (ConstSet.is_empty c) then
                    Format.fprintf ff "%(%)@[<2>%a:@ @[%a@]@]" b G.Qual.Var.printer v const_set_printer c;
                br
            end result.solution pre end in
            Format.fprintf ff "@[<v2>Solution:@\n%t@]@\n" solution_printer;
            Format.fprintf ff "@[<2>Unsolvable:@ %a@]" unsolvables_printer result.unsolvables
    end


    let solve consts g =
        (* turn consts into a set; this will be the initial solution for every qual *)
        let initial_consts = List.fold_left (fun consts c -> ConstSet.add c consts) ConstSet.empty consts in

        (* TODO: the below assumes that the accepting state is the same as the start state *)
        let rec solve solution = function
            | qual::qualwork ->
                let forwards = fold_forward_accept (fun a acc -> a::acc) qual g [] in
                let backwards = fold_backward_accept (fun a acc -> a::acc) qual g [] in

                (* find the solutions for qual, and update equivalence classes if necessary *)
                let solution, qual_consts = match qual with
                    | G.Qual.Var v ->
                        let equate solution x = Solution.equate_class v x solution in
                        let solution = List.fold_left equate solution forwards in
                        let solution = List.fold_left equate solution backwards in
                        (solution, Solution.find v solution) (* should always be present *)
                    | G.Qual.Const c ->
                        (solution, Solution.const c)
                in

                (* update the solutions of adjacents with the intersection *)
                let update_consts (solution, updated) adj =
                    let adj_consts = (try Solution.find adj solution with Not_found -> initial_consts) in

                    (* TODO: this line is the main difference from Qual-solve *)
                    let new_consts = ConstSet.inter qual_consts adj_consts in

                    (* update only if the solution changes *)
                    if ConstSet.equal adj_consts new_consts then
                        (solution, updated)
                    else begin
                        (* if the solution changes, put adj back into the work list *)
                        (Solution.add adj new_consts solution, (G.Qual.Var adj)::updated)
                    end
                in
                let solution, qualwork = List.fold_left update_consts (solution, qualwork) forwards in
                let solution, qualwork = List.fold_left update_consts (solution, qualwork) backwards in

                (* and recurse *)
                solve solution qualwork

            | [] ->
                (* done *)
                solution
        in
        (* filter out consts that are not in the constraint graph *)
        let qual_consts = List.filter (G.mem_vertex g) (List.map (fun c -> G.Qual.Const c) consts) in
        solve (Solution.empty initial_consts g) qual_consts


    module Explanation = struct
        module Path = struct
            type t = G.Qual.Const.t * (G.Qual.t * G.Automaton.t) list * G.Qual.Const.t

            (* TODO: ignore order of nodes in path *)
            let compare (ax, xs, zx as x) (ay, ys, zy as y) = if x == y then 0 else
                match G.Qual.Const.compare ax ay with
                    | 0 ->
                        begin match G.Qual.Const.compare zx zy with
                            | 0 ->
                                let rec compare x y = match x, y with
                                    | (x, xa)::xs, (y, ya)::ys ->
                                        begin match G.Qual.compare x y with
                                            | 0 ->
                                                begin match G.Automaton.compare xa ya with
                                                    | 0 -> compare xs ys
                                                    | i -> i
                                                end
                                            | i -> i
                                        end
                                    | [], [] -> 0
                                    | [], _  -> -1
                                    | _, []  -> 1
                                in
                                compare xs ys
                            | i -> i
                        end
                    | i -> i


            let path_printer ff path = ignore begin List.fold_left begin fun b (qual, automaton) ->
                Format.fprintf ff "%(%)%a %a" b G.Automaton.printer automaton G.Qual.printer qual; "@\n"
            end "" path end

            let printer ff (a, xs, z) =
                Format.fprintf ff "@[%a@ == %a@]@\n%a" G.Qual.Const.printer a G.Qual.Const.printer z path_printer xs
        end
        include Set.Make(Path)

        let printer ff pathset = ignore begin
            fold (fun path b -> Format.fprintf ff "%(%)@[%a@]" b Path.printer path; "@\n@\n") pathset ""
        end
    end


    (* given a solution, explain all unsolvables *)
    let explain solution =
        let consts = Solution.consts solution in
        let constraints = Solution.constraints solution in
        let unsolvables = Solution.unsolvables solution in

        (* breadth-first-search to a sink, and return the remaining frontier *)
        let explain_one (source, frontier) =
            let rec explain_one = function
                | (front, path, visited)::frontier, advanced ->
                    let rec search_front visited newadvanced = function
                        | (G.Qual.Const sink, _)::rest when not (ConstSet.mem sink consts) ->
                            (* ignore sinks that are not in this discrete order *)
                            search_front visited newadvanced rest

                        | (G.Qual.Const sink, automaton)::rest when G.Qual.Const.compare source sink <> 0 ->
                            (* one of the nodes in this front is a sink, so abandon this front and
                             * return this source-to-sink path and the rest of the frontier *)
                            let explanation = (source, List.rev ((G.Qual.Const sink, automaton)::path), sink) in
                            Some (explanation, List.rev_append frontier advanced)

                        | (G.Qual.Var v, automaton)::rest when not (VisitedSet.mem (v, automaton) visited) ->
                            let visited = VisitedSet.add (v, automaton) visited in
                            let path = (G.Qual.Var v, automaton)::path in

                            (* yet to reach sink: tentitavely advance the frontier and search the rest *)
                            let collect v automaton adj = (v, automaton)::adj in
                            let front = G.fold_forward collect constraints (G.Qual.Var v) automaton [] in
                            let front = G.fold_backward collect constraints (G.Qual.Var v) automaton front in

                            search_front visited ((front, path, visited)::newadvanced) rest

                        | (G.Qual.Var _, _)::rest      (* never revisit *)
                        | (G.Qual.Const _, _)::rest -> (* never traverse across annotations *)
                            search_front visited newadvanced rest

                        | [] ->
                            (* none of the nodes in this front is a sink, move on to the next front *)
                            explain_one (frontier, newadvanced)
                    in
                    search_front visited advanced front

                | [], (_::_ as advanced) ->
                    (* done searching this depth, advance to the next depth *)
                    explain_one ((List.rev advanced), [])

                | [], [] ->
                    (* no more nodes to search *)
                    None
            in
            explain_one (frontier, [])
        in


        (* work list of (source qualifier constant, path); the work list is derived via the graph walking
         * automaton to maintain a consistent description of the path *)
        let worklist = ConstSet.fold begin fun source worklist ->
            let collect node automaton (worklist, visited) = match node with
                | G.Qual.Var v when Solution.Unsolvables.mem v unsolvables
                                 && not (VisitedSet.mem (v, automaton) visited) ->
                    let start = [ (node, automaton) ] in
                    let path = [ (G.Qual.Const source, G.Automaton.start) ] in
                    (* list of source terminals -> list of fronts (the frontier) -> list of open nodes *)
                    let worklist = (source, [ (start, path, VisitedSet.empty) ])::worklist in
                    let visited = VisitedSet.add (v, automaton) visited in
                    (worklist, visited)
                | _ ->
                    (worklist, visited)
            in
            let acc = (worklist, VisitedSet.empty) in
            let acc = G.fold_forward collect constraints (G.Qual.Const source) G.Automaton.start acc in
            let acc = G.fold_backward collect constraints (G.Qual.Const source) G.Automaton.start acc in
            fst acc
        end consts [] in


        (* find exactly one explanation for each source *)
        (* TODO: find up to N explanations for each source *)
        List.fold_left begin fun explanations work -> match explain_one work with
            | Some (explanation, _) -> Explanation.add explanation explanations
            | None -> explanations
        end Explanation.empty worklist
end


module Aliasing (G : sig include QualGraphAutomatonType module V : VertexType with type t = vertex end) = struct
    include Traversal (G)
    module VarSet = Set.Make (G.Qual.Var)

    let alias v constraints =
        (* alias := flow-backward* flow-forward* *)
        let rec collect fold solution = function
            | v::worklist ->
                let solution, worklist = fold begin fun u (solution, worklist) ->
                    if VarSet.mem u solution then
                        (solution, worklist)
                    else
                        (VarSet.add u solution, u::worklist)
                end (G.Qual.Var v) constraints (solution, worklist) in
                collect fold solution worklist
            | [] ->
                solution
        in
        if G.mem_vertex constraints (G.Qual.Var v) then
            let solution = collect fold_backward_accept VarSet.empty [ v ] in
            let solution = collect fold_forward_accept solution (v::(VarSet.elements solution)) in
            solution
        else
            VarSet.empty
end

