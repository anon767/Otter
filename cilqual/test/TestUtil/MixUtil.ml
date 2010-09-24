open Str
open MyOUnit

open OtterCore


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
    (* convert MIX(foo) to __attribute__((mix(foo))) *)
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
            | Cil.GType (typ, _) -> Cil.ChangeTo [ Cil.GType (typ, Cil.locUnknown) ]
            | Cil.GCompTag (comp, _) -> Cil.ChangeTo [ Cil.GCompTag (vcomp comp, Cil.locUnknown) ]
            | Cil.GCompTagDecl (comp, _) -> Cil.ChangeTo [ Cil.GCompTagDecl (comp, Cil.locUnknown) ]
            | _ -> Cil.DoChildren
    method vstmt s =
        s.Cil.skind <- begin match s.Cil.skind with
            | Cil.Return (exp, _) -> Cil.Return (exp, Cil.locUnknown)
            | Cil.Goto (stmt, _) -> Cil.Goto (stmt, Cil.locUnknown)
            | Cil.Break _ -> Cil.Break Cil.locUnknown
            | Cil.Continue _ -> Cil.Continue Cil.locUnknown
            | Cil.If (exp, t, f, _) -> Cil.If (exp, t, f, Cil.locUnknown)
            | Cil.Switch (exp, block, stmts, _) -> Cil.Switch (exp, block, stmts, Cil.locUnknown)
            | Cil.Loop (block, _, continue, break) -> Cil.Loop (block, Cil.locUnknown, continue, break)
            | Cil.TryFinally (t, f, _) -> Cil.TryFinally (t, f, Cil.locUnknown)
            | Cil.TryExcept (t, e, f, _) -> Cil.TryExcept (t, e, f, Cil.locUnknown)
            | k -> k
        end;
        Cil.DoChildren
    method vinst i =
        let i = match i with
            | Cil.Set (lval, exp, _) -> Cil.Set (lval, exp, Cil.locUnknown)
            | Cil.Call (lval, fn, args, _) -> Cil.Call (lval, fn, args, Cil.locUnknown)
            | Cil.Asm (attrs, template, ci, dj, rk, _) -> Cil.Asm (attrs, template, ci, dj, rk, Cil.locUnknown)
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

let assert_has_block_errors expected_count block_errors =
    let actual_count = List.length block_errors in
    if actual_count <> expected_count then begin
        let printer ff block_errors = ignore begin List.iter begin fun (s, l, b) ->
            Format.fprintf ff "@[%s:%d: %s@]@\n" l.Cil.file l.Cil.line s;
        end block_errors end in
        assert_failure
            "@[@[<2>expected: %d block errors@]@ @[<2> but got:@ %d block errors@]@\n@[<2>Block errors:@\n%a@]@]"
            expected_count actual_count printer block_errors
    end

let assert_no_block_errors block_errors = assert_has_block_errors 0 block_errors

let assert_has_abandoned expected_count results =
    (* count jobs that were abandoned *)
    let abandoned = List.fold_left begin fun abandoned result -> match result with
        | Job.Abandoned (s, loc, _), block_errors -> (s, loc, block_errors)::abandoned
        | _, _ -> abandoned
    end [] results in
    let actual_count = List.length abandoned in
    if actual_count <> expected_count then begin
        let printer ff abandoned = ignore begin List.iter begin fun (s, l, b) ->
            Format.fprintf ff "@[%s:%d: %a@]@\n" l.Cil.file l.Cil.line Report.abandoned_reason s;
        end abandoned end in
        assert_failure
            "@[@[<2>expected: %d abandoned@]@ @[<2> but got:@ %d abandoned@]@\n@[<2>Abandoned paths:@\n%a@]@]"
            expected_count actual_count printer abandoned
    end

let assert_no_abandoned results = assert_has_abandoned 0 results

