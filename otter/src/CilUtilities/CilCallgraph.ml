(** Find the callers and callees of functions. *)


(**/**) (* various helper modules *)

module Callgraph = Ocamlgraph.Persistent.Digraph.ConcreteBidirectional (CilData.CilFundec)
module FundecSet = Set.Make (CilData.CilFundec)

module DotCallgraph = Ocamlgraph.Graphviz.Dot (struct
    include Callgraph
    let quote v = Str.global_replace (Str.regexp "\"") "\\\"" v.Cil.svar.Cil.vname

    (* Graphviz attributes *)
    let vertex_name v = "\""^(quote v)^"\""
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_attributes v = [ `Label (quote v) ]
    let default_edge_attributes _ = []
    let edge_attributes _ = []
    let get_subgraph _ = None
end)

let output_dot_file = ref "cilcallgraph.dot"

(**/**)


(** Resolve a {!Cil.exp} to a list of {!Cil.fundec}, using {!CilPtranal.points_to_fundec} to resolve function pointers. *)
let resolve_exp_to_fundecs file = function
    | Cil.Lval (Cil.Var fn, Cil.NoOffset) -> (try [ FindCil.fundec_by_varinfo file fn ] with Not_found -> [])
    | Cil.Lval (Cil.Mem ptrexp, Cil.NoOffset) -> CilPtranal.points_to_fundec file ptrexp
    | _ -> failwith "Does Cil generate other variations of function call expressions?"


(** Compute the callgraph of a file. *)
let compute_callgraph =
    let memotable = Hashtbl.create 0 in
    fun file ->
        try
            Hashtbl.find memotable file
        with Not_found ->
            let callgraph = ref Callgraph.empty in
            Cil.visitCilFileSameGlobals begin object
                inherit Cil.nopCilVisitor
                method vfunc fundec =
                    callgraph := Callgraph.add_vertex !callgraph fundec;
                    ignore begin Cil.visitCilFunction begin object
                        inherit Cil.nopCilVisitor
                        method vinst = function
                            | Cil.Call (_, fexp, _, _) ->
                                let targets = resolve_exp_to_fundecs file fexp in
                                List.iter (fun target -> callgraph := Callgraph.add_edge !callgraph fundec target) targets;
                                Cil.SkipChildren
                            | _ ->
                                Cil.SkipChildren
                    end end fundec end;
                    Cil.SkipChildren
            end end file;
            Hashtbl.add memotable file !callgraph;
            !callgraph


(** Find the {!Cil.fundecs} of callers of a function. *)
let find_callers file fundec = Callgraph.pred (compute_callgraph file) fundec


(** Find the {!Cil.fundecs} of callees of a function. *)
let find_callees file fundec = Callgraph.succ (compute_callgraph file) fundec


(**/**) (* helper function for transitive queries *)
let find_transitive_worker dir file fundec =
    let callgraph = compute_callgraph file in
    let rec find_transitive_worker result = function
        | fundec::worklist when not (FundecSet.mem fundec result) ->
            find_transitive_worker (FundecSet.add fundec result) (List.rev_append (dir callgraph fundec) worklist)
        | _::worklist ->
            find_transitive_worker result worklist
        | [] ->
            FundecSet.elements result
    in
    find_transitive_worker FundecSet.empty (dir callgraph fundec)
(**/**)


(** Find the {!Cil.fundecs} of transitive callers of a function. *)
let find_transitive_callers =
    find_transitive_worker Callgraph.pred


(** Find the {!Cil.fundecs} of transitive callees of a function. *)
let find_transitive_callees =
    find_transitive_worker Callgraph.succ


(** Save the callgraph of a file to a GraphViz dot file. *)
let save_to_dot file dot_file =
    let dot_out = open_out dot_file in
    let callgraph = compute_callgraph file in
    try
        DotCallgraph.output_graph dot_out callgraph;
        close_out dot_out
    with e ->
        close_out dot_out;
        raise e

(** Cil feature description. *)
let feature : Cil.featureDescr = {
    Cil.fd_enabled = ref false;
    Cil.fd_name = "cilcallgraph";
    Cil.fd_description = "saving the callgraph of the program to a GraphViz dot file";
    Cil.fd_extraopt = [
        ("--cilcallgraph-dot-file",
        Arg.Set_string output_dot_file,
        "<filename> The file to which to write the callgraph (default is " ^ !output_dot_file ^ ")")
    ];
    Cil.fd_doit = (fun file -> save_to_dot file !output_dot_file);
    Cil.fd_post_check = false;
}
