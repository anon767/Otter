open Control.Monad
open TypeQual.QualType
open Config
open CilData


module type InterpreterMonad = sig
    include CilUnionQualType.CilUnionQualTypeMonad

    val annot_attr : QualType.t -> Cil.attributes -> QualType.t monad
    val embed_lval : QualType.Var.Embed.t -> Cil.typ -> QualType.t monad
    val embed_rval : Cil.typ -> QualType.t monad
end


(* interpreter for Cil.exp types *)
module InterpreterT (C : CilUnionQualType.CilUnionQualTypeMonad) = struct
    include C
    module Ops = MonadOps (C)
    open Ops

    (* annotated with qualifiers from attributes *)
    let annot_attr qt attrlist = perform
        mapM_ (annot qt) (parse_annot attrlist);
        return qt

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
    let embed_type t =
        let module QualTypeOps = MonadOps (QualType) in
        let rec embed_type = function
            | Cil.TSArray (typ, _, _) -> perform with module QualType in
                (* treat n-d arrays as a single cell;
                 * fortunately, Cil makes conversion to pointer explicit with Cil.StartOf *)
                embed_type typ
            | Cil.TSPtr (Cil.TSBase t, _) when Cil.isVoidType t -> perform with module QualType in
                QualType.base
            | Cil.TSPtr (pointsTo, _) -> perform with module QualType in
                qtarget <-- embed_type pointsTo;
                QualType.ref qtarget
            | Cil.TSFun (r, a, is_vararg, _) -> perform with module QualType in
                qtr <-- embed_type r;
                qta <-- QualTypeOps.mapM embed_type a;
                if is_vararg then perform with module QualType in
                    (* add one variable to represent varargs *)
                    qtva <-- QualType.base;
                    QualType.fn qtr (qta @ [ qtva ])
                else
                    QualType.fn qtr qta
            | Cil.TSComp _ (* structs are field-based, unions are untyped *)
            | Cil.TSEnum _
            | Cil.TSBase _ -> perform with module QualType in
                QualType.base
        in
        embed_type (Cil.typeSig t)

    let annot_qt qt t = perform
        let rec annot_qt qt = function
            | Cil.TSArray (typ, _, attrlist) -> perform
                annot_qt qt typ;
                annot_attr qt attrlist
            | Cil.TSPtr (Cil.TSBase t, attrlist) when Cil.isVoidType t -> perform
                (* TODO: warn about qualifiers in t *)
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
            (* TODO: also add attributes from compinfo *)
            | Cil.TSComp (_, _, attrlist)
            | Cil.TSEnum (_, attrlist) -> perform
                annot_attr qt attrlist
            | Cil.TSBase _ as t -> perform
                annot_attr qt (Cil.typeSigAttrs t)
        in
        annot_qt qt (Cil.typeSig t)

    let embed_lval v t = perform
        let t = if CilType.is_or_points_to_function t
            then t               (* C'ism: function pointers are already lvals, so do not need an extra ref *)
            else (CilType.ref t) (* but other variables need an extra ref to become lvals *)
        in
        qt <-- embed v (embed_type t);
        annot_qt qt t

    let embed_rval t = perform
        let t = if CilType.is_or_points_to_function t
            then (CilType.deref t) (* C'ism: deref cast to function pointers to become rvals *)
            else t                 (* but casts to other types are already rvals *)
        in
        qt <-- fresh (embed_type t);
        annot_qt qt t
end

