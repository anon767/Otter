

(* helpers *)
let failwith format = Format.ksprintf Pervasives.failwith format


(* command line options *)
let arg_output_file = ref ""
let arg_input_files = ref []
let arg_for_pack = ref "<none>"
let arg_include_directories = ref []
let arg_hide_warnings = ref false


(* graph style *)
let style : ('a, 'b, 'c) format =
    "\
    graph [ style = \"filled, rounded\", colorscheme = \"purples9\" ]@\n\
    node [ style = \"filled\", colorscheme = \"purples9\" ]\
    "
let initialcolor = 3
let lastcolor = 9


(* packs/for-packs *)
type pack_content =
    | Pack of string * pack_content list (* pack name and contents *)
    | Module of string * string list (* module name and dependencies *)

type pack = string * pack_content (* for-pack name and content *)

let annot_extension = ".annot"
let pack_extension = ".dotpack"

let concat_name prefix name = match prefix, name with
    | "", name -> name
    | prefix, "" -> prefix
    | prefix, name -> prefix ^ "." ^ name

let pack_name = function
    | _, Pack (name, _)
    | _, Module (name, _) -> name

let pack_fqn = function
    | prefix, Pack (name, _)
    | prefix, Module (name, _) -> concat_name prefix name

let pack_name_of_file file = String.capitalize (Filename.chop_extension (Filename.basename file))


(* write a pack to a file *)
let write_pack pack_path pack =
    let pack_out = open_out pack_path in
    try
        Marshal.to_channel pack_out pack [];
        close_out pack_out
    with e ->
        close_out pack_out;
        raise e


let add_pack, read_pack, load_pack =
    (* memoize the results *)
    let memotable = Hashtbl.create 0 in

    (* add a pack to the memotable *)
    let add_pack pack =
        let name = pack_name pack in
        match try Hashtbl.find memotable name with Not_found -> None with
            | None -> Hashtbl.add memotable name (Some pack)
            | Some _ -> failwith "Module %s already exists" name
    in

    (* read a pack from a file *)
    let read_pack pack_path =
        let pack_in = open_in pack_path in
        try
            let pack = (Marshal.from_channel pack_in : pack) in
            close_in pack_in;
            add_pack pack;
            pack
        with e ->
            close_in pack_in;
            raise e
    in

    (* load a pack that was previously added or by searching the include directories *)
    let load_pack module_name =
        let module_name = String.capitalize module_name in
        try
            Hashtbl.find memotable module_name
        with Not_found ->
            let rec load_pack = function
                | dir::rest ->
                    let result_opt =
                        (* try both lower- and upper-case files *)
                        let path = Filename.concat dir ((String.uncapitalize module_name) ^ pack_extension) in
                        try Some (path, read_pack path) with Sys_error _ ->
                            let path = Filename.concat dir (module_name ^ pack_extension) in
                            try Some (path, read_pack path) with Sys_error _ ->
                                None
                    in
                    begin match result_opt with
                        | Some (_, pack) when pack_name pack = module_name ->
                            let pack_opt = Some pack in
                            Hashtbl.add memotable module_name pack_opt;
                            pack_opt
                        | Some (path, pack) ->
                            if not !arg_hide_warnings then Format.eprintf "%s does not contain module %s@." path module_name;
                            load_pack rest
                        | None ->
                            load_pack rest
                    end
                | [] ->
                    Hashtbl.add memotable module_name None;
                    None
            in
            load_pack (""::(List.rev !arg_include_directories))
    in
    (add_pack, read_pack, load_pack)


(* simple graph implementation *)
module NodeSet = Set.Make (String)
module Graph = struct
    module Adjacency = struct
        include Map.Make (String)
        let mem key value map = NodeSet.mem value (find key map)
        let find key map = try find key map with Not_found -> NodeSet.empty
        let remove key value map =
            let values = NodeSet.remove value (find key map) in
            if NodeSet.is_empty values then remove key map else add key values map
        let add key value map = add key (NodeSet.add value (find key map)) map
    end
    let empty = (Adjacency.empty, Adjacency.empty)
    let mem_edge src dest (forward, backward) = Adjacency.mem src dest forward || Adjacency.mem dest src backward
    let remove_edge src dest (forward, backward) = (Adjacency.remove src dest forward, Adjacency.remove dest src backward)
    let add_edge src dest (forward, backward) = (Adjacency.add src dest forward, Adjacency.add dest src backward)
    let fold_edges f (forward, _) = Adjacency.fold (fun key -> NodeSet.fold (f key)) forward
    let fold_nodes f (forward, _) = Adjacency.fold (fun key value -> f key) forward
    let fold_successors f src (forward, _) = NodeSet.fold f (Adjacency.find src forward)
    let fold_predecessors f dest (_, backward) = NodeSet.fold f (Adjacency.find dest backward)
    let transitive_reduction graph =
        (* depth-first search based algorithm for DAG *)
        let rec descend marked node successor graph =
            if not (NodeSet.mem successor marked) && mem_edge node successor graph then
                let graph = fold_predecessors begin fun predecessor graph ->
                    if NodeSet.mem predecessor marked then
                        remove_edge predecessor successor graph
                    else
                        graph
                end successor graph graph in
                fold_successors (descend (NodeSet.add node marked) successor) successor graph graph
            else
                graph
        in
        fold_edges (descend NodeSet.empty) graph graph
end


(* resolve all dependencies of a module to fully-qualified names *)
let resolve_dependencies annot_file =
    (* read the .annot file for the module and look for ext_ref *)
    let find_dependencies annot_file =
        let module_ref_regexp = Str.regexp "[ 	]+\\([A-Z][A-Za-z0-9'_.() 	]+\\)\\.\\(([ 	]+\\)?[^A-Z]" in
        let annot_in = open_in annot_file in
        let rec find_dependencies deps =
            try
                let annot_line = input_line annot_in in
                let deps = if annot_line.[0] = '"' then (* lines starting with '"' are file markers *)
                    deps
                else
                    let rec scan start deps =
                        try
                            let _ = Str.search_forward module_ref_regexp annot_line start in
                            scan (Str.match_end ()) (NodeSet.add (Str.matched_group 1 annot_line) deps)
                        with Not_found ->
                            deps
                    in
                    scan 0 deps
                in
                find_dependencies deps
            with End_of_file ->
                deps
        in
        let dependencies = find_dependencies NodeSet.empty in
        close_in annot_in;
        NodeSet.elements dependencies
    in

    (* for each ext_ref found, try to load its first component and resolve to its fully-qualified name, otherwise drop it *)
    let dot_regexp = Str.regexp "[ 	().]+" in
    List.fold_left begin fun dependencies dep ->
        let dep = Str.split dot_regexp dep in
        match load_pack (List.hd dep) with
            | Some (dep_for_pack, dep_pack) ->
                let rec open_modules prefix dep dependencies pack = match dep, pack with
                    | dep_name::rest, Pack (pack_name, pack_list) when dep_name = pack_name ->
                        List.fold_left (open_modules (concat_name prefix pack_name) rest) dependencies pack_list
                    | dep_name::[], Module (module_name, _) when dep_name = module_name ->
                        (concat_name prefix module_name)::dependencies
                    | _ ->
                        dependencies
                in
                open_modules dep_for_pack dep dependencies dep_pack
            | None ->
                dependencies
    end [] (find_dependencies annot_file)


let nodes_printer ff pack_list =
    (* print modules in hierarchical clusters *)
    let rec nodes_printer color ff pack_list =
        ignore begin List.fold_left begin fun sep pack ->
            Format.fprintf ff sep;
            let fqn = pack_fqn pack in
            begin match snd pack with
                | Pack (pack_name, pack_list) ->
                    Format.fprintf ff
                        "\
                        subgraph \"cluster_%s\" {@\n\
                        \  label = \"%s\"@\n\
                        \  color = \"%d\"@\n\
                        \  @[%a@]@\n\
                        }\
                        "
                        pack_name
                        pack_name
                        color
                        (nodes_printer (min (color + 1) lastcolor)) (List.map (fun p -> (fqn, p)) pack_list);

                | Module (module_name, module_deps) ->
                    Format.fprintf ff "\"%s\" [ label = \"%s\", color = \"%d\" ]" fqn module_name color
            end;
            "@\n"
        end "" pack_list end
    in
    nodes_printer initialcolor ff pack_list


let edges_printer ff pack_list =
    (* collect the edges *)
    let rec find_adjacencies graph pack_list = List.fold_left begin fun graph pack ->
        let fqn = pack_fqn pack in
        match snd pack with
            | Pack (pack_name, pack_list) ->
                find_adjacencies graph (List.map (fun p -> (fqn, p)) pack_list)
            | Module (module_name, module_deps) ->
                List.fold_left (fun graph dep -> Graph.add_edge fqn dep graph) graph module_deps
    end graph pack_list in
    let graph = find_adjacencies Graph.empty pack_list in

    (* compute the transitive reduction *)
    let graph = Graph.transitive_reduction graph in

    (* print the edges *)
    ignore begin Graph.fold_nodes begin fun node sep ->
        Format.fprintf ff "%(%)@[<hv>\"%s\" -> {@;<1 2>@[%t@]@ }@]" sep node
            begin fun ff -> ignore begin Graph.fold_successors begin fun successor sep ->
                Format.fprintf ff "%(%)\"%s\"" sep successor;
                ";@ "
            end node graph "" end end;
        "@\n"
    end graph "" end


(* write the module dependencies graph to a GraphViz dot file *)
let write_dot dot_path pack_name pack_list =
    let file_out = open_out dot_path in
    let ff = Format.formatter_of_out_channel file_out in

    Format.fprintf ff
        "\
        digraph \"%s\" {@\n\
        \  @[%(%)@]@\n\
        \  @[%a@]@\n\
        \  @[%a@]@\n\
        }@.\
        "
        pack_name
        style
        nodes_printer pack_list
        edges_printer pack_list;

    close_out file_out


let options = [
    ("-o", Arg.Set_string arg_output_file, "<file> Place the output into <file>");
    ("-I", Arg.String (fun dir -> arg_include_directories := dir::!arg_include_directories), "<dir> Add <dir> to the list of include directories");
    ("-for-pack", Arg.Set_string arg_for_pack, "<module> Prepare a file to be 'packed' into <module>");
    ("-hide-warnings", Arg.Set arg_hide_warnings, " Do not print warnings");
]


let _ =
    let exe_name = Filename.basename Sys.argv.(0) in
    try
        Arg.parse options
            (fun file -> arg_input_files := file::!arg_input_files)
            ("Module dependency graph generator for Ocaml\n"
                ^ exe_name ^ " [options] -o <output file> <input files>");

        if !arg_output_file = "" then failwith "No output file given";
        let pack_name = pack_name_of_file !arg_output_file in

        (* divide the input files into annot files and pack files, ignoring other files *)
        let annot_files, pack_files = List.fold_left begin fun (annot_files, pack_files) input_file ->
            if Filename.check_suffix input_file annot_extension then
                (input_file::annot_files, pack_files)
            else if Filename.check_suffix input_file pack_extension then
                (annot_files, input_file::pack_files)
            else begin
                if not !arg_hide_warnings then Format.eprintf "%s is not a %s or a %s file@." annot_extension pack_extension input_file;
                (annot_files, pack_files)
            end
        end ([], []) !arg_input_files in

        (* process the input *)
        match !arg_for_pack, pack_files with
            | "<none>", pack_files ->
                (* generate the final dot file *)

                let pack_list = List.rev_map read_pack pack_files in
                let annot_list = List.rev_map begin fun annot_file ->
                    let pack = ("", Module (pack_name_of_file annot_file, resolve_dependencies annot_file)) in
                    add_pack pack;
                    pack
                end annot_files in

                (* write the dot file *)
                write_dot !arg_output_file pack_name (pack_list @ annot_list)

            | for_pack, [] ->
                (* generate a pack file for a module *)
                begin match annot_files with
                    | [ annot_file ] ->
                        (* write the pack *)
                        let pack = Module (pack_name_of_file annot_file, resolve_dependencies annot_file) in
                        write_pack !arg_output_file (for_pack, pack)

                    | _ ->
                        failwith "Exactly one %s file should be specified when option -for-pack is used" annot_extension
                end

            | for_pack, pack_files ->
                (* pack multiple packs *)
                begin match annot_files with
                    | [] ->
                        (* make sure that all the for_packs match *)
                        let pack_list = List.rev_map begin fun pack_file ->
                            let pack_for_pack, pack = read_pack pack_file in
                            let expected_pack_for_pack = concat_name for_pack pack_name in
                            if pack_for_pack <> expected_pack_for_pack then
                                failwith "%s was not generated with -for-pack %s" pack_file expected_pack_for_pack;
                            pack
                        end pack_files in

                        (* write the pack *)
                        write_pack !arg_output_file (for_pack, Pack (pack_name, pack_list))

                    | _ ->
                        failwith "No %s files should be specified when option -pack is used" annot_extension
                end

    with Failure msg ->
        Format.eprintf "%s: %s@." exe_name msg;
        exit 1
