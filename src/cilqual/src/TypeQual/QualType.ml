open Control.Data
open Control.Monad
open Control.Graph

open QualGraph
open Qual


module type TypedQualVar = sig
    module Embed : PrintableComparableType
    type t = Fresh of int
           | Embed of Embed.t
           | Deref of t
           | FnRet of t
           | FnArg of int * t
    val fresh : int -> t
    val compare : t -> t -> int
    val hash : t -> int
    val equal : t -> t -> bool
    val subst : t -> t -> t -> t
    val printer : Format.formatter -> t -> unit
end


module type TypedConstraint = sig
    module TypedQualVar : TypedQualVar
    type t = Default
           | Typed of TypedQualVar.t
    val compare : t -> t -> int
    val default : t
    val printer : Format.formatter -> t -> unit
end


(** qualifier variables encoded by reversed-types *)
module TypedQualVar (Embed : PrintableComparableType) = struct
    module Embed = Embed

    type t = Fresh of int
           | Embed of Embed.t
           | Deref of t
           | FnRet of t
           | FnArg of int * t

    let fresh i = Fresh i

    let rec compare x y = if x == y then 0 else match x, y with
        | Fresh x, Fresh y ->
            Pervasives.compare x y
        | Embed x, Embed y ->
            Embed.compare x y
        | Deref x, Deref y
        | FnRet x, FnRet y ->
            compare x y
        | FnArg (ix, x), FnArg (iy, y) ->
            begin match Pervasives.compare ix iy with
                | 0 -> compare x y
                | i -> i
            end
        | x, y ->
            let rank = function Fresh _ -> 0 | Embed _ -> 1 | Deref _ -> 2 | FnRet _ -> 3 | FnArg _ -> 4 in
            Pervasives.compare (rank x) (rank y)

    let rec hash = function
        | Fresh i -> 31 * i
        | Embed v -> 1 + (31 * Embed.hash v)
        | Deref v -> 2 + (31 * hash v)
        | FnRet v -> 3 + (31 * hash v)
        | FnArg (i, v) -> 4 + i + (31 * hash v)

    let equal x y = (compare x y) = 0

    let subst x y v =
        let rec subst v = if equal v x then y else match v with
            | Fresh _
            | Embed _ as v -> v
            | Deref v      -> Deref (subst v)
            | FnRet v      -> FnRet (subst v)
            | FnArg (i, v) -> FnArg (i, subst v)
        in subst v

    let rec printer ff = function
        | Fresh i      -> Format.fprintf ff "Fresh %d" i
        | Embed v      -> Format.fprintf ff "Embed @[%a@]" Embed.printer v
        | Deref v      -> Format.fprintf ff "Deref @[(%a)@]" printer v
        | FnRet v      -> Format.fprintf ff "FnRet @[(%a)@]" printer v
        | FnArg (i, v) -> Format.fprintf ff "FnArg:%d @[(%a)@]" i printer v
end


(** qualifier constraint for TypedQualVar *)
module TypedConstraint (TypedQualVar : TypedQualVar) = struct
    module TypedQualVar = TypedQualVar
    open TypedQualVar

    type t = Default
           | Typed of TypedQualVar.t

    let compare x y = if x == y then 0 else match x, y with
        | Typed x, Typed y ->
            TypedQualVar.compare x y
        | x, y ->
            let rank = function Default -> 0 | Typed _ -> 1 in
            Pervasives.compare (rank x) (rank y)
    let default = Default

    let printer ff = function
        | Typed (Deref _)      -> Format.fprintf ff "Deref"
        | Typed (FnRet _)      -> Format.fprintf ff "FnRet"
        | Typed (FnArg (i, _)) -> Format.fprintf ff "FnArg:%d" i
        | Default -> Format.fprintf ff "="
        | _ -> ()
end


module type QualTypeMonad = sig
    include Monad

    (** qualified-type: types embedded with qualifier variables *)
    module QualType : sig
        module Var : TypedQualVar
        module Qual : Qual with type Var.t = Var.t
        type t = Ref of Qual.t * t         (* reference types *)
               | Fn of Qual.t * t * t list (* function types *)
               | Base of Qual.t            (* base types *)
               | Empty                     (* empty/constant types (identity) *)

        include Monad
        val ref  : t -> t monad
        val fn   : t -> t list -> t monad
        val base : t monad
        val printer : Format.formatter -> t -> unit
    end
    module QualGraph : sig
        include QualGraphAutomatonType
        module V : VertexType with type t = vertex
        module E : EdgeType with type t = edge and type vertex = vertex and type label = Constraint.t
    end with module Qual = QualType.Qual

    val embed  : QualType.Var.Embed.t -> QualType.t QualType.monad -> QualType.t monad
    val fresh  : QualType.t QualType.monad -> QualType.t monad
    val empty  : QualType.t monad

    val assign : QualType.t -> QualType.t -> unit monad
    val join   : QualType.t -> QualType.t -> QualType.t monad
    val meet   : QualType.t -> QualType.t -> QualType.t monad

    val annot  : QualType.t -> QualType.Qual.Const.t -> QualType.t monad
    val deref  : QualType.t -> QualType.t monad
    val app    : QualType.t -> QualType.t list -> QualType.t monad
    val retval : QualType.t -> QualType.t monad
    val args   : QualType.t -> QualType.t list monad
end


module QualTypeT (TypedQualVar : TypedQualVar)
                 (TypedConstraint : TypedConstraint with module TypedQualVar = TypedQualVar)
                 (QM : QualMonad with type Qual.Var.t = TypedQualVar.t
                                  and type QualGraph.Constraint.t = TypedConstraint.t) = struct
    (* monad stack *)
    include (QM : Monad with type 'a monad = 'a QM.monad and type 'a param = 'a QM.param)
    open QM
    module Ops = MonadOps (QM)
    open Ops

    let lift x = x

    module QualType = struct
        module Var = TypedQualVar
        open Var
        module Qual = QM.Qual
        type t = Ref of Qual.t * t         (* reference types *)
               | Fn of Qual.t * t * t list (* function types *)
               | Base of Qual.t            (* base types *)
               | Empty                     (* empty/constant types (identity) *)

        include ReaderT (Qual) (Identity)

        let deref v   = Qual.Var (Deref (Qual.projvar v))
        let fnRet v   = Qual.Var (FnRet (Qual.projvar v))
        let fnArg i v = Qual.Var (FnArg (i, Qual.projvar v))

        (* explicit qualified-type constructors *)
        let subst_qt x y qt =
            let subst_qv v = Qual.Var (subst (Qual.projvar x) (Qual.projvar y) (Qual.projvar v)) in
            let rec subst_qt = function
                | Ref (qv, qt)      -> Ref (subst_qv qv, subst_qt qt)
                | Fn (qv, qtr, qtp) -> Fn (subst_qv qv, subst_qt qtr, List.map subst_qt qtp)
                | Base qv           -> Base (subst_qv qv)
                | Empty             -> failwith "TODO: report invalid use of QualType constructor"
            in subst_qt qt
        let ref qt = perform
            let rec shift_qt = function
                | Ref (qv, qt)   -> Ref (qv, shift_qt qt)
                | Fn (qv, _, _)
                | Base qv as qt  -> Ref (qv, subst_qt qv (deref qv) qt)
                | Empty          -> failwith "TODO: report invalid use of QualType constructor"
            in
            return (shift_qt qt)
        let fn qtr qtp = perform
            qv <-- ask;
            let qtr = subst_qt qv (fnRet qv) qtr in
            let _, qtp = List.fold_left begin fun (i, qtp) a ->
                let qt = subst_qt qv (fnArg i qv) a in
                (i + 1, qt::qtp)
            end (0, []) qtp in
            return (Fn (qv, qtr, List.rev qtp))
        let base = perform
            qv <-- ask;
            return (Base qv)

        let rec printer ff = function
            | Ref (q, qt) ->
                Format.fprintf ff "Ref @[(@[%a,@ %a@]@,)@]" Qual.printer q printer qt
            | Fn  (q, rt, pt) ->
                let list_printer ff args = ignore begin
                    List.fold_left (fun b a -> Format.fprintf ff "%(%)@[%a@]" b printer a; ",@ ") "" args
                end in
                Format.fprintf ff "Fn @[(@[<v>%a,@ %a,@ [@[<v>%a@]]@]@,)@]" Qual.printer q printer rt list_printer pt
            | Base q ->
                Format.fprintf ff "Base @[(@[%a@]@,)@]" Qual.printer q
            | Empty ->
                Format.fprintf ff "Empty"
        let create qt qv = let qt, _ = run qt ((), qv) in qt
    end

    module QualGraph = struct
        include (QualGraph : QualGraphType with type t = QualGraph.t
                                            and type edge = QualGraph.edge
                                            and module Qual = QualGraph.Qual
                                            and module Constraint = QualGraph.Constraint)
        open TypedQualVar
        open TypedConstraint
        module V = QualGraph.V
        module E = QualGraph.E


        (* Type extension edges:
         *      var:    x -----> y      var:    x -----> y      var:    x -----> y
         *              ^        ^              ^        ^              ^        ^
         *              |        |              |        |              |        |
         *      deref: *x <====> *y     fnRet: rx =====> ry     fnArg: ax <===== ay
         *      (non-variant)           (co-variant)            (contra-variant)
         *)
        module Automaton = struct
            (* carry a little more information for printing explanations *)
            type explain = Push of edge | Pop of edge | Walk of edge | Start
            type t = explain * TypedQualVar.t list
            let compare (_, xs) (_, ys) =
                let rec compare xs ys = if xs == ys then 0 else match xs, ys with
                    | x::xs, y::ys ->
                        begin match x, y with
                            | Deref _, Deref _
                            | FnRet _, FnRet _ ->
                                compare xs ys
                            | FnArg (x, _), FnArg (y, _) ->
                                begin match Pervasives.compare x y with
                                    | 0 -> compare xs ys
                                    | i -> i
                                end
                            | x, y ->
                                let rank = function Deref _ -> 0 | FnRet _ -> 1 | FnArg _ -> 2
                                                  | Fresh _ | Embed _ -> failwith "Impossible!"
                                in Pervasives.compare (rank x) (rank y)
                        end
                    | [], [] -> 0
                    | [], _  -> -1
                    | _, []  -> 1
                in
                compare xs ys
            let start = (Start, [])
            let accept (_, stack) = stack = []

            let printer ff (explain, stack) = match explain with
                | Push c | Pop c | Walk c -> E.printer ff c
                | Start  -> ()
        end
        let fold_bidi_lv f g v acc =
            let acc = fold_succ_e (fun e acc -> f `Forward e (E.dst e) acc) g v acc in
            let acc = fold_pred_e (fun e acc -> f `Backward e (E.src e) acc) g v acc in
            acc
        let rec fold_flow direction f g v (explain, stack) acc = match stack with
            | [] ->
                fold_bidi_lv begin fun d e w acc -> match d, E.label e with
                    | `Forward,  Typed tv -> f w (Automaton.Push e, tv::stack) acc
                    | `Backward, Typed _  -> acc
                    (* start: all forward edges *)
                    | d, Default when d = direction -> f w (Automaton.Walk e, stack) acc
                    | d, Default                    -> acc
                end g v acc

            | (Deref _)::tail ->
                fold_bidi_lv begin fun d e w acc -> match d, E.label e with
                    | `Forward,  Typed tv        -> f w (Automaton.Push e, tv::stack) acc
                    | `Backward, Typed (Deref _) -> f w (Automaton.Pop e, tail) acc
                    | `Backward, Typed _         -> acc
                    (* deref edge: non-variant *)
                    | _, Default -> f w (Automaton.Walk e, stack) acc
                end g v acc

            | (FnRet _)::tail ->
                fold_bidi_lv begin fun d e w acc -> match d, E.label e with
                    | `Forward,  Typed tv        -> f w (Automaton.Push e, tv::stack) acc
                    | `Backward, Typed (FnRet _) -> f w (Automaton.Pop e, tail) acc
                    | `Backward, Typed _         -> acc
                    (* fnRet edge: co-variant *)
                    | d, Default when d = direction -> f w (Automaton.Walk e, stack) acc
                    | d, Default                    -> acc
                end g v acc

            | (FnArg (i, _))::tail ->
                fold_bidi_lv begin fun d e w acc -> match d, E.label e with
                    | `Forward,  Typed tv                        -> f w (Automaton.Push e, tv::stack) acc
                    | `Backward, Typed (FnArg (j, _)) when i = j -> f w (Automaton.Pop e, tail) acc
                    | `Backward, Typed _                         -> acc
                    (* fnRet edge: contra-variant *)
                    | d, Default when d = direction -> acc
                    | d, Default                    -> f w (Automaton.Walk e, stack) acc
                end g v acc

            | (Fresh _)::_ | (Embed _)::_ ->
                failwith "Impossible!"

        and fold_forward f g v automaton acc = fold_flow `Forward f g v automaton acc
        and fold_backward f g v automaton acc = fold_flow `Backward f g v automaton acc
    end

    (* qualified-type constructors *)
    let embed e qt = perform
        let qv = Qual.Var (QualType.Var.Embed e) in
        return (QualType.create qt qv)
    let fresh qt = perform
        qv <-- fresh;
        return (QualType.create qt qv)

    let empty = return QualType.Empty

    (* qualified-type extenders *)
    let extend_type x y = perform
        let label = TypedConstraint.Typed (Qual.projvar x) in
        add_edge ~label:label x y
    let extend_deref qv = perform
        let qv' = QualType.deref qv in
        extend_type qv' qv;
        return (QualType.Base qv')
    let extend_fnRet qv = perform
        let qv' = QualType.fnRet qv in
        extend_type qv' qv;
        return (QualType.Base qv')
    let extend_fnArg i qv = perform
        let qv' = QualType.fnArg i qv in
        extend_type qv' qv;
        return (QualType.Base qv')

    let elaborate qv = function
        | QualType.Ref _ -> perform
            qv' <-- extend_deref qv;
            return (QualType.Ref (qv, qv'))
        | QualType.Fn (_, _, qtp) -> perform
            qtr <-- extend_fnRet qv;
            (_, qtp) <-- foldM begin fun (i, qtp) a -> perform
                qta <-- extend_fnArg i qv; return (i + 1, qta::qtp)
            end (0, []) qtp;
            return (QualType.Fn (qv, qtr, List.rev qtp))
        | QualType.Base _
        | QualType.Empty as qt ->
            return qt
    let abstract = function
        | QualType.Ref (qv, _)
        | QualType.Fn (qv, _, _) ->
            return (QualType.Base qv)
        | QualType.Base _
        | QualType.Empty as qt ->
            return qt


    (* qualified-type constraints *)
    let rec assign_ref l v = match l, v with
        | QualType.Empty, _
        | _, QualType.Empty ->
            return ()
        | QualType.Ref (ql, qtl), QualType.Ref (qv, qtv) -> perform
            eq qv ql;
            assign_ref qtl qtv (* referenced types should be non-variant *)
        | QualType.Fn (ql, qtrl, qtpl), QualType.Fn (qv, qtrv, qtpv) -> perform
            (* the function type remains non-variant *)
            eq qv ql;
            (* but the return value/parameters may be subtyped, since they cannot be written through *)
            assign qtrl qtrv;
            zipWithM_ assign qtpv qtpl
        | QualType.Base ql, QualType.Base qv ->
            eq qv ql
        | l, QualType.Base qvv -> perform
            v <-- elaborate qvv l;
            assign_ref l v
        | QualType.Base qvl, v -> perform
            l <-- elaborate qvl v;
            assign_ref l v
        | l, v -> perform
            ql <-- abstract l;
            qv <-- abstract v;
            assign_ref l qv;
            assign_ref ql v
    and assign l v = match l, v with
        | QualType.Empty, _
        | _, QualType.Empty ->
            return ()
        | QualType.Ref (ql, qtl), QualType.Ref (qv, qtv) -> perform
            leq qv ql;
            assign_ref qtl qtv   (* referenced types should be non-variant *)
        | QualType.Fn (ql, qtrl, qtpl), QualType.Fn (qv, qtrv, qtpv) -> perform
            leq qv ql;
            assign qtrl qtrv;
            zipWithM_ assign qtpv qtpl (* parameters are contravariant *)
        | QualType.Base ql, QualType.Base qv ->
            leq qv ql
        | l, QualType.Base qv -> perform
            v <-- elaborate qv l;
            assign l v
        | QualType.Base ql, v -> perform
            l <-- elaborate ql v;
            assign l v
        | l, v -> perform
            ql <-- abstract l;
            qv <-- abstract v;
            assign l qv;
            assign ql v

    let rec merge join meet lub x y = match x, y with
        | QualType.Empty, z
        | z, QualType.Empty -> perform
            return z
        | QualType.Ref (qx, qtx), QualType.Ref (qy, qty) -> perform
            qz <-- lub qx qy;
            qtz <-- join qtx qty;
            return (QualType.Ref (qz, qtz))
        | QualType.Fn (qx, qtrx, qtpx), QualType.Fn (qy, qtry, qtpy) -> perform
            qz <-- lub qx qy;
            qtrz <-- join qtrx qtry;
            qtpz <-- zipWithM meet qtpx qtpy;
            return (QualType.Fn (qz, qtrz, qtpz))
        | QualType.Base qx, QualType.Base qy -> perform
            qz <-- lub qx qy;
            return (QualType.Base qz)
        | x, QualType.Base qy -> perform
            y <-- elaborate qy x;
            merge join meet lub x y
        | QualType.Base ql, y -> perform
            x <-- elaborate ql y;
            merge join meet lub x y
        | x, y -> perform
            qy <-- abstract y;
            qz <-- merge join meet lub x qy;
            qz <-- abstract qz;
            qz <-- merge join meet lub qz y;
            abstract qz
    and join x y = merge join meet lub x y
    and meet x y = merge meet join glb x y (* i.e., invert the lattice *)


    (* qualified-type operations *)
    let annot qt s = match qt with
        | QualType.Ref (v, _)
        | QualType.Fn (v, _, _)
        | QualType.Base v -> perform
            annot v s;
            return qt
        | QualType.Empty -> perform
            return qt

    let deref = function
        | QualType.Ref (_, qt) ->
            return qt
        | QualType.Fn (qv, _, _)
        | QualType.Base qv ->
            extend_deref qv
        | QualType.Empty -> failwith "TODO: report invalid deref"

    let app qtf qta = match qtf with
        | QualType.Fn (_, qtr, qtp) -> perform
            zipWithM_ assign qtp qta;
            return qtr
        | QualType.Ref (qv, _)
        | QualType.Base qv -> perform
            (_, qtp) <-- foldM begin fun (i, qtp) a -> perform
                qta <-- extend_fnArg i qv;
                return (i + 1, qta::qtp)
            end (0, []) qta;
            zipWithM_ assign (List.rev qtp) qta;
            extend_fnRet qv
        | QualType.Empty -> failwith "TODO: report invalid app"

    let retval = function
        | QualType.Fn (_, qtr, _) ->
            return qtr
        | QualType.Ref (qv, _)
        | QualType.Base qv ->
            extend_fnRet qv
        | QualType.Empty -> failwith "TODO: report invalid retval"

    let args = function
        | QualType.Fn (_, _, qta) ->
            return qta
        | QualType.Ref (qv, _)
        | QualType.Base qv ->
            return [] (* TODO: would it be better to search the constraint graph? *)
        | QualType.Empty -> failwith "TODO: report invalid args"
end

