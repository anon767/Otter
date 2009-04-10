open Control.Monad
open TypeQual.QualType
open CilData


module type InterpreterMonad = sig
    include Config.InterpreterMonad

    val annot_attr : QualType.t -> Cil.attributes -> QualType.t monad
    val embed_lval : Cil.typ -> QualType.t monad
    val embed_rval : Cil.typ -> QualType.t monad
end


(* interpreter for Cil.exp types *)
module InterpreterT (C : Config.InterpreterMonad) = struct
    include C
    module Ops = MonadOps (C)
    open Ops

    (* annotated with qualifiers from attributes *)
    let annot_attr qt attrlist = perform
        quals <-- parse_annot attrlist;
        annot qt quals

    (* Translation from C types to CilQual types:
     *      Base/Fn         Ref Base/Fn     Ref Ref Base/Fn
     *      (Literal)       (Variable)      (Pointer)           Types       Notes
     * =========================================================================================================
     *      1               int             int *               numeric     one extra Ref for variables
     *      T fn()          T ( *fn)()      T ( **fn)()         functions
     *      {t}             T[]             ( *T)[]             arrays      {t} valid only as initializer
     *      {{t},{t}}       T[1][]          ( *T)[1][]          2-d arrays  ditto
     * =========================================================================================================
     *      (T)*x           (T* ) x         (T** )&x            T *x
     *      (T) x           (T* )&x                             T x
     * =========================================================================================================
     *      (T ( * )())*x   (T ( ** )()) x  (T ( *** )())&x     T ( **x)()  one less Ref for casts
     *      (T ( * )()) x   (T ( ** )())&x                      T ( *x)()
     *      (T ( * )())&x                                       T x()
     *      (T ( * )()) x                                       T x()       x automatically coerced to &x
     * =========================================================================================================
     *      (T[4])*x       (T( * )[4]) x    (T( ** )[4])&x      T ( *x)[4]  (T[4]) isn't a valid cast;
     *      (T[4]) x       (T( * )[4])&x                        T x[4]      (T( * )[4]) is pointer to array
     *                                                                      s.t. sizeof( *x)==sizeof(T)*4
     *      (T)*(x+1)      (T* )(x+1)                           T x[4]      x automatically coerced to &x[0]
     *                     (T* )&x[1]                           T x[4]
     *)

    (* convert types to fresh qualified types.
     * Construction of qualifier variable is in the QualType monad instead of the QualTypeMonad monad, so conversion
     * needs to be done in two stages: first, construct QualType.t, then annotate the qualifier variables.
     *)
    let embed t = perform
        let module QualTypeOps = MonadOps (QualType) in
        let rec embed_ts = function
            | Cil.TSArray (typ, _, attrlist) -> perform with module QualType in
                (* treat n-d arrays as a single cell;
                 * fortunately, Cil makes conversion to pointer explicit with Cil.StartOf *)
                embed_ts typ
            | Cil.TSPtr (pointsTo, attrlist) -> perform with module QualType in
                qtarget <-- embed_ts pointsTo;
                QualType.ref qtarget
            | Cil.TSFun (r, a, is_vararg, attrlist) -> perform with module QualType in
                qtr <-- embed_ts r;
                (* add one void * parameter for vararg *)
                qta <-- QualTypeOps.mapM embed_ts (if is_vararg then a @ [Cil.typeSig Cil.voidPtrType] else a);
                QualType.fn qtr qta
            | Cil.TSComp _ (* structs are field-based, unions are untyped *)
            | Cil.TSEnum _
            | Cil.TSBase _ -> perform with module QualType in
                QualType.base
        in
        let rec annot_qt qt = function
            | Cil.TSArray (typ, _, attrlist) -> perform
                annot_qt qt typ;
                annot_attr qt attrlist
            | Cil.TSPtr (pointsTo, attrlist) -> perform
                qtarget <-- deref qt;
                annot_qt qtarget pointsTo;
                annot_attr qt attrlist
            | Cil.TSFun (r, a, is_vararg, attrlist) -> perform
                qtr <-- retval qt;
                annot_qt qtr r;
                qta <-- args qt;
                zipWithM_ annot_qt qta a;
                annot_attr qt attrlist
            | Cil.TSComp (_, _, attrlist)
            | Cil.TSEnum (_, attrlist) -> perform
                annot_attr qt attrlist
            | Cil.TSBase _ as t -> perform
                annot_attr qt (Cil.typeSigAttrs t)
        in
        let ts = Cil.typeSig t in
        qt <-- create (embed_ts ts);
        inContext (fun _ -> emptyContext) (annot_qt qt ts) (* annotate qualifiers ignoring context *)

    let embed_lval t = perform
        if CilType.is_or_points_to_function t
            then embed t               (* C'ism: function pointers are already lvals, so do not need an extra ref *)
            else embed (CilType.ref t) (* but other variables need an extra ref to become lvals *)

    let embed_rval t = perform
        if CilType.is_or_points_to_function t
            then embed (CilType.deref t) (* C'ism: deref cast to function pointers to become rvals *)
            else embed t                 (* but casts to other types are already rvals *)
end

