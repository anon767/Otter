
open Ocamlbuild_plugin
open Ocamlbuild_pack
open Command (* no longer needed for OCaml >= 3.10.2 *)

(* find ocamlbuild tools *)
let myocamlbuild_tool tool = Filename.concat "myocamlbuild.tools" tool

(**
    {1 OCamlFind}

    Adapted from O'Caml Batteries:
    http://git.ocamlcore.org/cgi-bin/gitweb.cgi?p=batteries/batteries.git;a=blob_plain;f=myocamlbuild.ml;hb=HEAD
*)

module OCamlFind = struct
    (* these functions are not really officially exported *)
    let run_and_read      = Ocamlbuild_pack.My_unix.run_and_read
    let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings

    (* this lists all supported packages *)
    let find_packages () =
        blank_sep_strings &
            Lexing.from_string &
            run_and_read "ocamlfind list | cut -d' ' -f1"

    (* this is supposed to list available syntaxes, but I don't know how to do it. *)
    let find_syntaxes () = ["camlp4o"; "camlp4r"]

    (* ocamlfind command *)
    let ocamlfind x = S[A"ocamlfind"; x]

    let before_options () =
        (* by using Before_options, command line options have higher priority *)
        (* on the contrary using After_options will guarantee higher priority *)

        (* override default commands by ocamlfind ones *)
        Options.ocamlc     := ocamlfind & A"ocamlc";
        Options.ocamlopt   := ocamlfind & A"ocamlopt";
        Options.ocamldep   := ocamlfind & A"ocamldep";
        Options.ocamldoc   := ocamlfind & A"ocamldoc";
        Options.ocamlmktop := ocamlfind & A"ocamlmktop"

    let after_rules () =
        (* When one link an OCaml library/binary/package, one should use -linkpkg *)
        flag ["ocaml"; "link"; "byte"] & A"-linkpkg";
        flag ["ocaml"; "link"; "native"; "program"] & A"-linkpkg";

        (* For each ocamlfind package one inject the -package option when
         * compiling, computing dependencies, generating documentation and
         * linking. *)
        List.iter begin fun pkg ->
            flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
            flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
            flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
            flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
        end (find_packages ());

        (* Like -package but for extensions syntax. Morover -syntax is useless
         * when linking. *)
        List.iter begin fun syntax ->
            flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
            flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
            flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
        end (find_syntaxes ());

        (* The default "thread" tag is not compatible with ocamlfind.
           Indeed, the default rules add the "threads.cma" or "threads.cmxa"
           options when using this tag. When using the "-linkpkg" option with
           ocamlfind, this module will then be added twice on the command line.

           To solve this, one approach is to add the "-thread" option when using
           the "threads" package using the previous plugin.
         *)
        flag ["ocaml"; "pkg_threads"; "compile"] (A "-thread");
        flag ["ocaml"; "pkg_threads"; "link"]    (A "-thread");
        flag ["ocaml"; "pkg_threads"; "doc"]     (S[A "-I"; A "+threads"]);
end

(**
    {1 OCamlDoc}
*)

module OCamlDoc = struct
    let after_rules () =
        (* Provide flags for ocamldoc *)
        flag ["ocaml"; "doc"; "quiet"] (A "-hide-warnings");
        flag ["ocaml"; "doc"; "colorize_code"] (A "-colorize-code");

        copy_rule "ocamldoc: mlpack -> odocl" "%.mlpack" "%.odocl";
end

(**
    {1 Custom GraphViz generator for OCamlDoc that can handle module packs}
*)

module OCamlDoc_DotPack = struct
    let after_rules () =
        (* add myocamlbuild.tools/dotpack.native as dependency when tags ocaml, doc and dotpack/dot are enabled *)
        let dotpack_native = myocamlbuild_tool "dotpack.native" in
        dep ["ocaml"; "dotpack"] [ dotpack_native ];
        dep ["ocaml"; "dot"] [ dotpack_native ];

        (* also generate annot file when compiling ocaml source files *)
        tag_any ["annot"];


        (* stupid hack to find the -for-pack flag by searching the ocamlopt command line, since Ocamlbuild 3.11.2 did
           not provide any way to find it otherwise. Ocamlbuild 3.12.0 does provide a way (parameterized tags), but
           keep this for compatibility reasons
         *)
        let for_pack_flag_of env =
            let rec for_pack_flag_of = function
                | S (A "-for-pack"::A for_pack::_) -> S [A "-for-pack"; A for_pack]
                | S (A "-for-pack"::S list::rest) -> for_pack_flag_of (S (A "-for-pack"::(list @ rest)))
                | S (S list::rest) -> for_pack_flag_of (S (list @ rest))
                | S (_::rest) -> for_pack_flag_of (S rest)
                | _ -> S [A "-for-pack"; A ""]
            in
            match Ocaml_compiler.ocamlopt_c (tags_of_pathname (env "%.cmi")) (env "%.mli") (env "%.cmi") with
                | Cmd cmd -> for_pack_flag_of (Command.reduce cmd)
                | _ -> failwith ("Unexpected ocamlopt command for " ^ (env "%.cmi"))
        in


        (* compile ocaml sources into a dotpack file *)
        let dotpack_source sources env build =
            let sources = List.map env sources in
            let annot = env "%.annot" in
            let dotpack = env "%.dotpack" in
            let tags = (tags_of_pathname dotpack)++"ocaml" in

            (* process all dependencies of an ocaml source file *)
            List.iter begin fun source ->
                Ocaml_compiler.prepare_compile build source;
                (* attempt to compile dotpack files for all dependencies *)
                let include_dirs = Pathname.include_dirs_of (Pathname.dirname source) in
                let to_build = List.map begin fun (_, x) ->
                    expand_module include_dirs x ["dotpack"]
                end (Ocaml_utils.path_dependencies_of source) in
                ignore (build to_build)
            end sources;

            Cmd (S [Px dotpack_native;
                T(tags++"dotpack");
                Ocaml_utils.ocaml_include_flags (List.hd sources);
                for_pack_flag_of env;
                A "-o"; Px dotpack;
                P annot])
        in

        (* rather than depending on %.annot, depend on %.cmi which will indirectly generate %.annot,
           since there is no built-in rule for %.annot
         *)
        rule "dotpack: ml & mli -> dotpack"
            ~prod:"%.dotpack"
            ~deps:["%.ml"; "%.mli"; "%.cmi"]
            (dotpack_source ["%.ml"; "%.mli"]);

        rule "dotpack: mli -> dotpack"
            ~prod:"%.dotpack"
            ~deps:["%.mli"; "%.cmi"]
            (dotpack_source ["%.mli"]);

        rule "dotpack: ml -> dotpack"
            ~prod:"%.dotpack"
            ~deps:["%.ml"; "%.cmi"]
            (dotpack_source ["%.ml"]);


        (* compile mlpack/docpack files into a single dot/dotpack file *)
        let dotpack_pack make_dotpack pack env build =
            let pack = env pack in
            let out = if make_dotpack then env "%.dotpack" else env "%.dot" in
            let tag = if make_dotpack then "dotpack" else "dot" in
            let flags = if make_dotpack then for_pack_flag_of env else N in

            (* process all modules listed in the pack file *)
            let contents = string_list_of_file pack in
            let include_dirs = Pathname.include_dirs_of (Pathname.dirname pack) in
            let to_build = List.map begin fun module_name ->
                expand_module include_dirs module_name ["dotpack"]
            end contents in
            let modules = List.map Outcome.good (build to_build) in

            Cmd (S [Px dotpack_native;
                T ((tags_of_pathname out)++"ocaml"++tag);
                flags;
                A "-o"; Px out;
                S (List.map (fun m -> P m) modules)])
        in

        rule "dotpack: mlpack -> dotpack"
            ~prod:"%.dotpack"
            ~dep:"%.mlpack"
            (dotpack_pack true "%.mlpack");

        rule "dotpack: mlpack -> dot"
            ~prod:"%.dot"
            ~dep:"%.mlpack"
            (dotpack_pack false "%.mlpack");

        rule "dotpack: docpack -> dotpack"
            ~prod:"%.dotpack"
            ~dep:"%.docpack"
            (dotpack_pack true "%.docpack");

        rule "dotpack: docpack -> dot"
            ~prod:"%.dot"
            ~dep:"%.docpack"
            (dotpack_pack false "%.docpack");
end

(**
    {1 Patches to fix up misbehaviors in Ocamlbuild (as of version 3.11.2)}
*)

module Ocamlbuild_patches = struct
    let before_rules () =
        (* override the -libs option processing, since it's broken in ocamlbuild.
           related to: http://caml.inria.fr/mantis/view.php?id=4943
         *)
        flag ["ocaml"; "link"; "byte"] begin
            S (List.map (fun x -> A (x^".cma")) !Options.ocaml_libs)
        end;
        flag ["ocaml"; "link"; "native"; "program"] begin
            (* only link libraries for programs, not libraries *)
            S (List.map (fun x -> A (x^".cmxa")) !Options.ocaml_libs)
        end;
        Options.ocaml_libs := [];

        (* Workaround to add -I include directories to ocamldoc, taking it
           from the linker flags.
         *)
        let rec scan_include_dirs spec = function
            | "-I"::dir::tail -> scan_include_dirs (S[spec; A "-I"; P dir]) tail
            | _::tail -> scan_include_dirs spec tail
            | [] -> spec
        in
        flag ["ocaml"; "doc"] (scan_include_dirs N !Options.ocaml_lflags)


    let after_rules () =
        (* Workaround for "The implementation (obtained by packing) does not match the interface" errors, which seems
             to be due to the presence of the fake mli file.
        *)
        (* copied from ocaml_specific.ml and ocaml_compiler.ml *)
        let ocamlopt_p tags deps out =
            (* from Ocaml_compiler.ml:
                    Cmd(S[A"touch"; P mli; Sh" ; if "; cmd; Sh" ; then "; rm; Sh" ; else "; rm; Sh" ; exit 1; fi"])
                extract just the cmd part
            *)
            match Ocaml_compiler.ocamlopt_p tags deps out with
                | Cmd(S[_; _; _; cmd; _; _; _; _; _]) -> Cmd cmd
                | _ -> failwith "Unknown ocamlopt_p"
        in
        let link_from_file link modules_file cmX env build =
            let modules_file = env modules_file in
            let contents_list = string_list_of_file modules_file in
            link contents_list cmX env build
        in
        let native_pack_modules x =
            Ocaml_compiler.pack_modules [("cmx",["cmi"; !Options.ext_obj]); ("cmi",[])] "cmx" "cmxa" !Options.ext_lib ocamlopt_p
                (fun tags -> tags++"ocaml"++"pack"++"native") x
        in
        let native_pack_mlpack = link_from_file native_pack_modules in

        let native_profile_pack_modules x =
            Ocaml_compiler.pack_modules [("p.cmx",["cmi"; "p" -.- !Options.ext_obj]); ("cmi",[])] "p.cmx" "p.cmxa"
                ("p" -.- !Options.ext_lib) ocamlopt_p
                (fun tags -> tags++"ocaml"++"pack"++"native"++"profile") x
        in
        let native_profile_pack_mlpack = link_from_file native_profile_pack_modules in

        (* Insert the override rules just before the built-in rules, to maintain the rule resolution order.

           For some reason, ocamlbuild generates wildly different inputs for the dependency analysis of %.cmi,
           %.cmo and %cmx files, which occasionally leads to dependency results with slightly different ordering that
           causes "make inconsistent assumptions over interface" errors.

           I haven't figured out exactly, but it seems, by accident, the order of built-in rules masks this error in
           in that when such a conflict occurs, the respective rules will be retriggered, causing the %.cm[ox] file
           to be (unnecessarily) recompiled, thus hiding the conflict with the %.cmi file for the rest of the build.

           FIXME: this means that the %.cmi file will always be inconsistent with either the %.cmo or the %.cmx file,
           which is a problem if the %.cmi file is to be distributed in a library.
         *)
        rule "(override) ocaml: mlpack & cmi & cmx* & o* -> cmx & o"
            ~insert:(`before "ocaml: mlpack & cmi & cmx* & o* -> cmx & o")
            ~tags:["ocaml"; "native"]
            ~prods:["%.cmx"; "%" -.- !Options.ext_obj; "%.cmi"]
            ~deps:["%.mlpack"]
            (native_pack_mlpack "%.mlpack" "%.cmx");

        rule "(override) ocaml: mlpack & cmi & p.cmx* & p.o* -> p.cmx & p.o"
            ~insert:(`before "ocaml: mlpack & cmi & p.cmx* & p.o* -> p.cmx & p.o")
            ~tags:["ocaml"; "profile"; "native"]
            ~prods:["%.p.cmx"; "%.p" -.- !Options.ext_obj; "%.cmi"]
            ~deps:["%.mlpack"]
            (native_profile_pack_mlpack "%.mlpack" "%.p.cmx")
end


let _ = dispatch begin function
    | Before_options ->
        OCamlFind.before_options ()
    | Before_rules ->
        Ocamlbuild_patches.before_rules ()
    | After_rules ->
        OCamlFind.after_rules ();
        OCamlDoc.after_rules ();
        OCamlDoc_DotPack.after_rules ();
        Ocamlbuild_patches.after_rules ()
    | _ ->
        ()
end
