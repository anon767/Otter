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

module DiscreteOrder (G : sig include QualGraphAutomataType module V : VertexType with type t = vertex end) = struct
    module ConstSet = Set.Make (G.Qual.Const)

    module AutomataState = struct
        type t = G.Qual.Var.t * G.Automata.t
        let compare (x1, x2) (y1, y2) = match G.Qual.Var.compare x1 y1 with
            | 0 -> G.Automata.compare x2 y2
            | i -> i
    end

    module VisitedSet = Set.Make (AutomataState)

    module Solution = struct
        module SolutionMap = Map.Make (G.Qual.Var)
        module Unsolvables = Set.Make (G.Qual.Var)

        type t = {
            consts : ConstSet.t;
            constraints : G.t;
            solution : ConstSet.t SolutionMap.t;
            unsolvables : Unsolvables.t;
        }

        let empty consts constraints = {
            consts = consts;
            constraints = constraints;
            solution = SolutionMap.empty;
            unsolvables = Unsolvables.empty
        }

        let add var consts result =
            let unsolvables = if ConstSet.is_empty consts then
                Unsolvables.add var result.unsolvables
            else
                result.unsolvables
            in
            let solution = SolutionMap.add var consts result.solution in
            { result with solution = solution; unsolvables = unsolvables }

        let const c = ConstSet.singleton c

        let find v result = SolutionMap.find v result.solution

        let is_const v c result =
            try ConstSet.equal (SolutionMap.find v result.solution) (const c) with
                | Not_found -> false

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

        let is_unsatisfiable result = not (Unsolvables.is_empty result.unsolvables)
        let consts result = result.consts
        let constraints result = result.constraints
        let unsolvables result = result.unsolvables
    end


    let solve consts g =
        (* turn consts into a set; this will be the initial solution for every qual *)
        let initial_consts = List.fold_left (fun consts c -> ConstSet.add c consts) ConstSet.empty consts in

        (* TODO: the below assumes that the accepting state is the same as the start state *)
        let rec solve solution = function
            | qual::qualwork ->
                (* find all adjacent, accepted quals *)
                (* TODO: add a cache *)
                let find_adjacent_accept fold =
                    let rec find_adjacent_accept visited accepts = function

                        | (part, automata)::partwork ->
                            (* traverse adjacents of part *)
                            let visited, accepts, partwork =
                                fold begin fun v automata (visited, accepts, partwork) -> match v with
                                    | G.Qual.Var a when not (VisitedSet.mem (a, automata) visited) ->
                                        let visited = VisitedSet.add (a, automata) visited in
                                        if G.Automata.accept automata then
                                            (* could have duplicates, but the result is usually added to sets anyway *)
                                            (visited, a::accepts, partwork)
                                        else
                                            (* not visited and not accepted yet; so add to work list *)
                                            (visited, accepts, (v, automata)::partwork)
                                    | _ ->
                                        (visited, accepts, partwork)
                                end g part automata (visited, accepts, partwork)
                            in

                            (* and recurse *)
                            find_adjacent_accept visited accepts partwork

                        | [] ->
                            (* done *)
                            accepts
                    in
                    find_adjacent_accept VisitedSet.empty [] [(qual, G.Automata.start)]
                in
                let forwards = find_adjacent_accept G.fold_forward in
                let backwards = find_adjacent_accept G.fold_backward in

                (* find the solutions for qual *)
                let qual_consts = match qual with
                    | G.Qual.Var v -> Solution.find v solution (* should always be present *)
                    | G.Qual.Const c -> Solution.const c
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
        (* TODO: implement a "most different first" order; probably using a trie instead of a set *)
        module Path = struct
            type t = (G.Qual.t * G.Automata.t) list
            let rec compare x y = match x, y with
                | (x, xa)::xs, (y, ya)::ys ->
                    begin match G.Qual.compare x y with
                        | 0 ->
                            begin match G.Automata.compare xa ya with
                                | 0 -> compare xs ys
                                | i -> i
                            end
                        | i -> i
                    end
                | [], [] -> 0
                | [], _  -> -1
                | _, []  -> 1

            let printer ff path = ignore begin List.fold_left begin fun b (qual, automata) ->
                Format.fprintf ff "%(%)%a %a" b G.Automata.printer automata G.Qual.printer qual; "@\n"
            end "" path end
        end
        include Set.Make(Path)

        let printer ff pathset = ignore begin
            fold (fun path b -> Format.fprintf ff "%(%)@[%a@]" b Path.printer path; "@\n@\n") pathset ""
        end
    end


    (* given a solution, explain all unsolvables *)
    let explain solution =
        let constraints = Solution.constraints solution in
        let unsolvables = Solution.unsolvables solution in

        let rec explain explanations consts =
            (* pick a const as source *)
            let source = ConstSet.choose consts in
            let consts = ConstSet.remove source consts in

            if not (ConstSet.is_empty consts) then begin

                (* depth-first traversal to sink *)
                let rec find_sink explanations = function
                    | (prefix, direction, visited)::prefixwork ->
                        let part, automata = List.hd prefix in
                        let fold = match direction with
                            | `Forward -> G.fold_forward
                            | `Backward -> G.fold_backward
                        in
                        let explanations, prefixwork =
                            (* explore every successor/predecessor of this part *)
                            fold begin fun v automata (explanations, prefixwork) -> match v with
                                | G.Qual.Const c when ConstSet.mem c consts
                                                   && G.Automata.accept automata ->
                                    (* reached a sink *)
                                    let explanation = List.rev ((v, automata)::prefix) in
                                    (Explanation.add explanation explanations, prefixwork)

                                | G.Qual.Var a when not (VisitedSet.mem (a, automata) visited) ->
                                    let visited = VisitedSet.add (a, automata) visited in
                                    if G.Automata.accept automata then
                                        if Solution.Unsolvables.mem a unsolvables then
                                            (* explore in both directions if we've reached an accepting state *)
                                            let forward  = ((v, automata)::prefix, `Forward, visited) in
                                            let backward = ((v, automata)::prefix, `Backward, visited) in
                                            (explanations, forward::backward::prefixwork)
                                        else
                                            (* don't explore qualifier variables with solutions *)
                                            (explanations, prefixwork)
                                    else
                                        (* traverse until we reach an accepting state *)
                                        let prefix = ((v, automata)::prefix, direction, visited) in
                                        (explanations, prefix::prefixwork)
                                | _ ->
                                    (explanations, prefixwork)
                            end constraints part automata (explanations, prefixwork)
                        in

                        (* recurse *)
                        find_sink explanations prefixwork

                    | [] ->
                        (* done *)
                        explanations
                in
                (* traverse from this source to all other sinks, forwards and backwards *)
                let forwardwork = ([(G.Qual.Const source, G.Automata.start)], `Forward, VisitedSet.empty) in
                let backwardwork = ([(G.Qual.Const source, G.Automata.start)], `Backward, VisitedSet.empty) in
                let explanations = find_sink explanations [ forwardwork; backwardwork ] in

                (* recurse, until there are no more sources *)
                explain explanations consts

            end else
                (* done *)
                explanations
        in

        (* filter out consts that are not in the constraint graph *)
        let consts = ConstSet.filter (fun c -> G.mem_vertex constraints (G.Qual.Const c)) (Solution.consts solution) in
        explain Explanation.empty consts
end

