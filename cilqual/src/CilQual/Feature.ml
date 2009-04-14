open Control.Monad
open Control.Graph

(* setup CilQual interpreter monad stack *)
module G =
    Global.InterpreterT
        (Statement.InterpreterT
            (Instruction.InterpreterT
                (Expression.InterpreterT
                    (Environment.InterpreterT
                        (Type.InterpreterT
                            (Config.InterpreterT
                                (CilQualType.CilQualTypeT (Identity))))))))

(* setup CilQual solver *)
module SourceSink = TypeQual.QualSolver.Reachability.SourceSink (G.QualGraph)


(* setup Graphviz module *)
module C (Graph : sig include PrintableGraphType module V : VertexType module E : EdgeType end) = struct
    include Graph

    let quote printer x =
        let quoted = Format.fprintf Format.str_formatter "%a" printer x; Format.flush_str_formatter () in
        (Str.global_replace (Str.regexp "\"") "\\\"" quoted)

    (* Graphviz attributes *)
    let vertex_name v = "\""^(quote vertex_printer v)^"\""
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_attributes v = [ `Label (quote vertex_printer v) ]
    let default_edge_attributes _ = []
    let edge_attributes e = [ `Label (quote edge_printer e) ]
    let get_subgraph _ = None
end
module D = Ocamlgraph.Graphviz.Dot (C (G.QualGraph))


(* cilqual options *)
let opt_save_dot = ref ""
let opt_statistics = ref false


(* timing helpers *)
let timer = None
let tic s t =
    Format.eprintf "%s...@." s;
    match t with
        | None -> Some (Sys.time (), s, [])
        | Some (was, s', l) -> let now = Sys.time () in Some (now, s, (s', now -. was)::l)
let toc = function
    | None -> []
    | Some (was, s', l) -> let now = Sys.time () in (s', now -. was)::l


(* It's unclear to me how Cil handles interaction between features, especially since makeCFGFeature destructively
 * modifies the file. *)

(* call to configure Cil as required for CilQual *)
let init_cil () = Config.init_cil ()

(* prepare file for analysis by assigning globally unique vids to varinfo, and breaking up Cil.Instr to have at
 * most one Cil.Call, placed at the end *)
let prepare_file = Cilly.makeCFGFeature.Cil.fd_doit


(* cilqual driver *)
let doit file =
    (* prepare file for analysis *)
    prepare_file file;

    let timing = timer in

    (* generate the interpreter for the file *)
    let timing = tic "Generating interpreter monad" timing in
    let expM = G.interpret_file file in

    (* run the interpreter *)
    let timing = tic "Running interpreter monad" timing in
    let ((((), env), fresh), constraints) = G.run expM G.emptyEnv 0 G.emptyContext G.QualGraph.empty in

    (* determine if there is a path between $null and $nonnull *)
    let timing = tic "Resolving constraints" timing in

    let solver = SourceSink.create constraints in
    let null = G.QualGraph.Qual.Const "null" in
    let nonnull = G.QualGraph.Qual.Const "nonnull" in
    let has_error = SourceSink.check solver null nonnull || SourceSink.check solver nonnull null in

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
                          Edge count  : %7d@]@\n"
            (G.QualGraph.nb_vertex constraints)
            (G.QualGraph.nb_edges constraints);
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

