open Control.Monad
open TypeQual.QualType


module type InterpreterMonad = sig
    include Statement.InterpreterMonad
    val declare : Cil.varinfo -> QualType.t monad
    val interpret_global : Cil.global -> unit monad
end


module InterpreterT (S : Statement.InterpreterMonad) = struct
    include S
    module Ops = MonadOps (S)
    open Ops

    let lift x = x

    let interpret_init_var v init =
        let rec interpret_init_lval l = function
            | Cil.SingleInit e -> perform
                qtv <-- interpret_exp e;
                qtl <-- access_rval l;
                assign qtl qtv;
                return ()
            | Cil.CompoundInit (_, initlist) ->
                mapM_ begin fun (offset, init) ->
                    interpret_init_lval (Cil.addOffsetLval offset l) init
                end initlist
        in
        interpret_init_lval (Cil.var v) init

    let interpret_function { Cil.svar=f; Cil.sformals=formals; Cil.sbody=body } = perform
        qt <-- lookup_var f;
        qtr <-- retval qt;
        qta <-- args qt;
        qtv <-- interpret_block body;
        zipWithM_ (fun v a -> assign_lval (Cil.var v) a) formals qta;
        assign qtr qtv;
        return ()

    let interpret_global = function
        | Cil.GVar (v, { Cil.init=Some init }, loc) ->
            interpret_init_var v init

        | Cil.GFun (f, loc) ->
            interpret_function f

        | Cil.GPragma (attr, loc) ->
            return () (*failwith "TODO: read partial-order configuration"*)

        | Cil.GAsm (_, loc) ->
            return () (*failwith "TODO: do something with asm"*)

        (* nothing to do with variable declarations/definitions with no initializers/typedef/struct/enum, since the
         * same varinfo/compinfo/enuminfo is available at use sites, and the environment is initialized lazily *)
        | Cil.GVarDecl (_, loc)
        | Cil.GVar (_, { Cil.init=None }, loc)
        | Cil.GType (_, loc)
        | Cil.GCompTagDecl (_, loc)
        | Cil.GCompTag (_, loc)
        | Cil.GEnumTagDecl (_, loc)
        | Cil.GEnumTag (_, loc) ->
            return ()
        | Cil.GText _ ->
            return ()


    let interpret_file { Cil.globals=globals; Cil.globinit=init } = perform
        mapM_ interpret_global globals;
        match init with
            | Some f -> interpret_function f
            | None -> return ()
end

