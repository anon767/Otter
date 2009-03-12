open Control.Monad

open TypeQual.Qual
open TypeQual.QualType


(* setup CilQual monad stack *)
module G =
    Global.InterpreterT
        (Statement.InterpreterT
            (Instruction.InterpreterT
                (Expression.InterpreterT
                    (Environment.InterpreterT
                        (Type.InterpreterT
                            (QualTypeT (Identity)))))))

(* setup Graphviz module *)
module C (G : Constraints with type E.label = QualTypeConstraints.TypeVarLabel.t) = struct
    include G
    open QualTypeConstraints.TypeVarLabel

    (* print forward edges only *)
    let iter_edges_e f = iter_edges_e begin fun e -> match E.label e with
        | Forward _ -> f e
        | Backward _ -> ()
    end

    (* Graphviz attributes *)
    let vertex_name v =
        let quoted = Str.global_replace (Str.regexp "\"") "\\\""
            (vertex_printer Format.str_formatter v; Format.flush_str_formatter ()) in
        "\""^quoted^"\""
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_attributes _ = []
    let default_edge_attributes _ = []
    let edge_attributes _ = []
    let get_subgraph _ = None
end
module D = Graph.Graphviz.Dot (C (G.Constraints))


(* cilqual options *)
let opt_save_dot = ref ""
let opt_statistics = ref false


(* timing helpers *)
let timer = None
let tic s t =
    Format.eprintf "%s...@\n" s;
    match t with
        | None -> Some (Sys.time (), s, [])
        | Some (was, s', l) -> let now = Sys.time () in Some (now, s, (s', now -. was)::l)
let toc = function
    | None -> []
    | Some (was, s', l) -> let now = Sys.time () in (s', now -. was)::l


(* cilqual driver *)
let doit file =
    (* check Cil setup *)
    if !Cil.insertImplicitCasts then begin
        Format.eprintf
            "Error: @[Cil is configured to insert implicit typecasts!@\n\
                    CilQual will not be able to track type qualifiers correctly.@]@.";
        exit 1
    end;

    (* initialize constraint graph *)
    let null = (G.Constraints.const "null") in
    let nonnull = (G.Constraints.const "nonnull") in
    let init_constraints = List.fold_left G.Constraints.add_vertex G.Constraints.empty [null; nonnull] in

    let timing = timer in

    (* generate the interpreter for the file *)
    let timing = tic "Generating interpreter monad" timing in
    let expM = G.interpret_file file in

    (* run the interpreter *)
    let timing = tic "Running interpreter monad" timing in
    let ((((), _), constraints), _) = G.run expM G.emptyEnv init_constraints 0 in

    (* determine if there is a path between $null and $nonnull *)
    let timing = tic "Resolving constraints" timing in
    let path_checker = G.Constraints.create_path_checker constraints in
    let has_error = (G.Constraints.check_path path_checker null nonnull)
                    or (G.Constraints.check_path path_checker nonnull null)
    in

    let timing = toc timing in

    (* save the constraint graph, if requested *)
    if !opt_save_dot <> "" then begin
        let dot_file = open_out !opt_save_dot in
        D.output_graph dot_file constraints;
        close_out dot_file
    end;

    (* print statistics, if requested *)
    if !opt_statistics then begin
        Format.eprintf "@[<v2>Timing:@\n%a@]@\n" begin fun ff ts ->
            ignore (List.fold_left (fun b (s, t) -> Format.fprintf ff "%(%)@[%-30s: %7.3fs@]" b s t; "@\n") "" ts)
        end (List.rev timing);
        Format.eprintf "@[<v2>Constraint graph:@\n\
                          Vertex count: %7d@\n\
                          Edge count  : %7d (halved to account for backward edges)@]@\n"
            (G.Constraints.nb_vertex constraints)
            (G.Constraints.nb_edges constraints / 2);
        let gc = Gc.quick_stat () in
        Format.eprintf "@[<v2>Memory:@\n\
                          Allocated: %10.2fMiB@\n\
                          Maximum  : %10.2fMiB@]@\n"
            (float_of_int (Sys.word_size / 8) *.
                (gc.Gc.minor_words +. gc.Gc.major_words -. gc.Gc.promoted_words) /. 1024. /. 1024.)
            ((float_of_int (Sys.word_size / 8)) *. (float_of_int gc.Gc.top_heap_words) /. 1024. /. 1024.)
    end;

    (* finally, report the error *)
    if has_error then begin
        Format.eprintf "Error: path found between $null and $nonnull!@.";
        exit 1
    end else begin
        Format.eprintf "Safe: no path found between $null and $nonnull.@."
    end


(* Cil feature description *)
let description = {
    Cil.fd_name = "cilqual";
    Cil.fd_enabled = ref false;
    Cil.fd_description = "check type qualifier";
    Cil.fd_extraopt = [
        ("--cilqual-save-dot", Arg.Set_string(opt_save_dot),
            "<file> Save the constraint graph in Graphviz DOT format");
        ("--cilqual-statistics", Arg.Set(opt_statistics),
            " Print statistics about the analysis");
    ];
    Cil.fd_post_check = false;
    Cil.fd_doit = doit
}

