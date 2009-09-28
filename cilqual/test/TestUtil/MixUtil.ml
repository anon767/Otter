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

let strip_location_visitor = object
    inherit Cil.nopCilVisitor
    method vvdec v =
        v.Cil.vdecl <- Cil.locUnknown;
        Cil.SkipChildren
    method vglob g =
        let vcomp c = List.iter (fun f -> f.Cil.floc <- Cil.locUnknown) c.Cil.cfields; c in
        match g with
            | Cil.GType (typ, loc) -> Cil.ChangeTo [ Cil.GType (typ, Cil.locUnknown) ]
            | Cil.GCompTag (comp, loc) -> Cil.ChangeTo [ Cil.GCompTag (vcomp comp, Cil.locUnknown) ]
            | Cil.GCompTagDecl (comp, loc) -> Cil.ChangeTo [ Cil.GCompTagDecl (comp, Cil.locUnknown) ]
            | _ -> Cil.DoChildren
    method vinst i =
        let i = match i with
            | Cil.Set (lval, exp, loc) -> Cil.Set (lval, exp, Cil.locUnknown)
            | Cil.Call (lval, fn, args, loc) -> Cil.Call (lval, fn, args, Cil.locUnknown)
            | Cil.Asm (attrs, template, ci, dj, rk, loc) -> Cil.Asm (attrs, template, ci, dj, rk, Cil.locUnknown)
        in
        Cil.ChangeTo [ i ]
end

let constraints_printer ff solution =
    Mix.TypedBlock.G.QualGraph.printer ff (Mix.TypedBlock.DiscreteSolver.Solution.constraints solution)

let assert_discrete_satisfiable solution =
    if Mix.TypedBlock.DiscreteSolver.Solution.is_unsatisfiable solution then
        assert_failure "@[<v2>Should be satisfiable:@\n%a@]" Mix.TypedBlock.DiscreteSolver.Solution.printer solution

let assert_discrete_unsatisfiable solution =
    if not (Mix.TypedBlock.DiscreteSolver.Solution.is_unsatisfiable solution) then
        assert_failure "@[<v2>Should be unsatisfiable:@\n%a@]" Mix.TypedBlock.DiscreteSolver.Solution.printer solution

