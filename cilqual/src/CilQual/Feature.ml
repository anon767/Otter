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
    (CilQualType.CilQualTypeT (Environment.CilFieldOrVar) (CilQualType.Context)
    (Identity)))))))

(* setup CilQual solver *)
module DiscreteSolver = TypeQual.QualSolver.DiscreteOrder (G.QualGraph)


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


(* timing helpers *)
let timer = None
let tic s = function
    | None ->
        Format.eprintf "%s...@?" s;
        Some (Sys.time (), s, [])
    | Some (was, s', l) ->
        let now = Sys.time () in
        let elapsed = now -. was in
        Format.eprintf "(%7.3fs)@.%s...@?" elapsed s;
        Some (now, s, (s', elapsed)::l)
let toc = function
    | None -> []
    | Some (was, s', l) ->
        let now = Sys.time () in
        let elapsed = now -. was in
        Format.eprintf "(%7.3fs)@." elapsed;
        (s', now -. was)::l


(* It's unclear to me how Cil handles interaction between features, especially since makeCFGFeature destructively
 * modifies the file. *)

(* call to configure Cil as required for CilQual *)
let init_cil () = Config.init_cil ()

(* prepare file for analysis *)
let prepare_file file =
    (* remove unused declarations *)
    Rmtmps.removeUnusedTemps file;
    (* assign globally unique vids to varinfo, and break up Cil.Instr to have at most one Cil.Call,
     * placed at the end *)
    Cilly.makeCFGFeature.Cil.fd_doit file


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
    let (((((), constraints), _), _), _) =
        G.run expM (((((), G.QualGraph.empty), (G.fileContext file)), 0), G.emptyEnv) in

    (* save the constraint graph, if requested *)
    let timing = if !opt_save_dot = "" then timing else begin
        let timing = tic "Saving constraint graph" timing in
        let dot_file = open_out !opt_save_dot in
        D.output_graph dot_file constraints;
        close_out dot_file;
        timing
    end in

    (* determine if there is a path between $null and $nonnull *)
    let timing = tic "Solving constraints" timing in

    let consts = [ "null"; "nonnull" ] in
    let solution = DiscreteSolver.solve consts constraints in
    let has_error = DiscreteSolver.Solution.is_unsatisfiable solution in

    let timing, explanation = if not has_error then (toc timing, DiscreteSolver.Explanation.empty) else begin
        let timing = tic "Explaining solution" timing in
        let explanation = DiscreteSolver.explain solution in

        let timing = toc timing in
        Format.eprintf "@[%a@]@\n" DiscreteSolver.Explanation.printer explanation;
        (timing, explanation)
    end in

    (* print statistics, if requested *)
    if !Errormsg.verboseFlag || !Cilutil.printStats then begin
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
        Format.eprintf "Error: %d acyclic path(s) found between $null and $nonnull!@."
            (DiscreteSolver.Explanation.cardinal explanation);
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
    ];
    Cil.fd_post_check = false;
    Cil.fd_doit = doit
}

