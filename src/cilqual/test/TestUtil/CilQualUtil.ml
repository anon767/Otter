open Str
open Control.Monad
open CilQual.Environment.CilFieldOrVarOrCast


module DummyContext = struct
    include CilQual.CilQualType.Context
    let printer ff x = ()
end


module SetupType (T : CilQual.Type.InterpreterMonad) = struct
    open T
    module Ops = MonadOps (T)
    open Ops

    let preprocess =
        (* convert $foo to __attribute__((cilqual(foo))) *)
        global_replace (regexp "\\([ \t\r\n(]\\)\\$\\([_a-zA-Z0-9]+\\)")
                       ("\\1__attribute__(("^CilQual.Config.annot_attribute_string^"(\\2)))")

    (* helper to patch some limitations of Formatcil *)
    let patchtype =
        Cil.visitCilType begin object
            inherit Cil.nopCilVisitor
            method vtype t = match t with
                | Cil.TFun (r, Some [ (s, Cil.TVoid _, _) ], b, a) ->
                    Cil.ChangeDoChildrenPost (Cil.TFun (r, Some [], b, a), fun x -> x)
                | _ ->
                    Cil.DoChildren
        end end

    let create_type typestr = patchtype (Formatcil.cType (preprocess typestr) [])

    let adapt_cil_printer printer ff x =
        let code = Pretty.sprint ~width:120 (printer () x) in
        let code = global_replace (regexp "^[ \t]*\n") "" code in
        let lines = split (regexp "\n") code in
        ignore (List.fold_left (fun b e -> Format.fprintf ff "%(%)%s" b e; "@\n") "" lines)

    let type_printer printer ff t = Format.fprintf ff "@[%a@]" (adapt_cil_printer printer#pType) t
end


module Setup (E : CilQual.Environment.InterpreterMonad) = struct
    include SetupType (E)
    open E

    let create_env typedecls vardecls =
        (* setup structs/unions *)
        let formatcilenv = List.fold_left begin fun env decl ->
            let isStruct, (name, fieldstr) = match decl with `Struct d -> true, d | `Union d -> false, d in
            let fields = begin fun comp ->
                let env = (name, Cil.Fc comp)::env in
                List.map begin fun (fname, ftype) ->
                    let typ = patchtype (Formatcil.cType (preprocess ftype) env) in
                    (fname, typ, None, [], Cil.locUnknown)
                end fieldstr
            end in
            let comp = Cil.mkCompInfo isStruct name fields [] in
            (name, Cil.Fc comp)::env
        end [] typedecls in

        (* create Cil variables *)
        let cilenv =
            let mkvar (name, typstr) =
                let typ = patchtype (Formatcil.cType (preprocess typstr) formatcilenv) in
                (name, Cil.makeGlobalVar name typ)
            in
            List.map mkvar vardecls
        in

        (* setup Cil environment *)
        let formatcilenv = List.map (fun (name, var) -> (name, Cil.Fv var)) cilenv in

        (cilenv, formatcilenv)

    let lookup_env x (cilenv, env) =
        let v = List.assoc x cilenv in
        Env.find (CilVar v) env

    let env_printer printer ff env = ignore begin
        List.fold_left (fun b (_, v) -> Format.fprintf ff "%(%)@[%a@]" b (adapt_cil_printer printer#pVDecl) v; ",@ ") "" env
    end

    let file_printer printer ff file = ignore begin
        let globalprinter = (adapt_cil_printer printer#pGlobal) in
        List.fold_left (fun b g -> Format.fprintf ff "%(%)@[%a@]" b globalprinter g; "@\n") "" file.Cil.globals
    end

    let cilqual_env_printer ff env = ignore begin
        let kvprinter ff (k, v) = match k with
            | CilVar vi -> Format.fprintf ff "@[%s:%d@] => @[%a@]" vi.Cil.vname vi.Cil.vid E.QualType.printer v
            | _ -> ()
        in
        Env.fold (fun k v b -> Format.fprintf ff "%(%)@[%a@]" b kvprinter (k, v); "@\n") env ""
    end
end

