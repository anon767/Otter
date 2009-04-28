open Str
open MyOUnit

let preprocess str =
    (* convert NULL to ((void * $(null))0) *)
    let preprocess_null =
        global_replace (regexp "\\bNULL\\b")
                       ("((void * $(null))0)")
    in
    (* convert $(foo) to __attribute__((cilqual(foo))) *)
    let preprocess_cilqual =
        global_replace (regexp "\\([ \t\r\n(]\\)\\$(\\([_a-zA-Z0-9]+\\))")
                       ("\\1__attribute__(("^CilQual.Config.annot_attribute_string^"(\\2)))")
    in
    (* convert MIX(foo) to __attribute__((cilqual(foo))) *)
    let preprocess_mix =
        global_replace (regexp "\\([ \t\r\n(]\\)MIX(\\([_a-zA-Z0-9]+\\))")
                       ("\\1__attribute__(("^Mix.Config.annot_attribute_string^"(\\2)))")
    in
    preprocess_mix (preprocess_cilqual (preprocess_null str))

let constraints_printer ff solution =
    Mix.TypedBlock.G.QualGraph.printer ff (Mix.TypedBlock.DiscreteSolver.Solution.constraints solution)

let assert_discrete_satisfiable solution =
    if Mix.TypedBlock.DiscreteSolver.Solution.is_unsatisfiable solution then
        assert_failure "@[<v2>Should be satisfiable:@\n%a@]" Mix.TypedBlock.DiscreteSolver.Solution.printer solution

let assert_discrete_unsatisfiable solution =
    if not (Mix.TypedBlock.DiscreteSolver.Solution.is_unsatisfiable solution) then
        assert_failure "@[<v2>Should be unsatisfiable:@\n%a@]" Mix.TypedBlock.DiscreteSolver.Solution.printer solution

