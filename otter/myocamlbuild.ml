
(* adapted from O'Caml Batteries:
 * http://git.ocamlcore.org/cgi-bin/gitweb.cgi?p=batteries/batteries.git;a=blob_plain;f=myocamlbuild.ml;hb=HEAD
 *)

open Ocamlbuild_plugin
open Command (* no longer needed for OCaml >= 3.10.2 *)

(**
   {1 OCamlFind}
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
        flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
        flag ["ocaml"; "pkg_threads"; "link"]    (S[A "-thread"]);
        flag ["ocaml"; "pkg_threads"; "doc"]     (S[A "-I"; A "+threads"]);

        (* Provide flags for ocamldoc *)
        flag ["ocaml"; "doc"; "quiet"]          (S[A "-hide-warnings"]);
        flag ["ocaml"; "doc"; "dot_reduce"]     (S[A "-dot-reduce"]);
        flag ["ocaml"; "doc"; "colorize_code"]  (S[A "-colorize-code"]);
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
        flag ["ocaml"; "doc"] (scan_include_dirs N !Options.ocaml_lflags);
end


let _ = dispatch begin function
    | Before_options ->
        OCamlFind.before_options ()
    | Before_rules ->
        Ocamlbuild_patches.before_rules ()
    | After_rules ->
        OCamlFind.after_rules ()
    | _ ->
        ()
end
