open Control.Data
open Control.Monad
open Control.Graph
open Control.UnionFind

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


module type UnionQualTypeMonad = sig
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
    module UnionTable : sig
        type t
        val empty : t
        (*val select_table : QualType.Qual.t -> QualType.t -> t -> QualType.t*)
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


module UnionQualTypeT (TypedQualVar : TypedQualVar)
                 (QM : QualMonad with type Qual.Var.t = TypedQualVar.t) = struct
    module QualType = struct
        module Var = TypedQualVar
        open Var
        module Qual = QM.Qual
        type t = Ref of Qual.t * t         (* reference types *)
               | Fn of Qual.t * t * t list (* function types *)
               | Base of Qual.t            (* base types *)
               | Empty                     (* empty/constant types (identity) *)

        module Reader = ReaderT (Qual) (Identity)
        include Reader
        module Ops = MonadOps (Reader)
        open Ops

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

        let rec clone = function
            | Ref (_, qt) -> perform
                qt <-- clone qt;
                ref qt
            | Fn (_, qtr, qtp) -> perform
                qtr <-- clone qtr;
                qtp <-- mapM clone qtp;
                fn qtr qtp
            | Base _ ->
                base
            | Empty ->
                return Empty


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


    (* union-find based map from qualifier variables to tables of qualified types which make up a union type *)
    module UnionTable = struct

        (* module for comparing qualified types by structure, ignoring qualifier variables *)
        module StructuralQualType = struct
            include QualType
            let rec compare x y = if x == y then 0 else match x, y with
                | Ref (_, qtx), Ref (_, qty) -> compare qtx qty
                | Fn (_, qtxr, qtxa), Fn (_, qtyr, qtya) ->
                    begin match compare qtxr qtyr with
                        | 0 ->
                            let rec compare_args xs ys = match xs, ys with
                                | x::xs, y::ys ->
                                    begin match compare x y with
                                        | 0 -> compare_args xs ys
                                        | i -> i
                                    end
                                | [], [] -> 0
                                | [], _ -> -1
                                | _, [] -> 1
                            in
                            compare_args qtxa qtya
                        | i -> i
                    end
                | Base _, Base _ -> 0
                | Empty, Empty -> 0
                | x, y ->
                    let rank = function Ref _ -> 0 | Fn _ -> 1 | Base _ -> 2 | Empty -> 3 in
                    Pervasives.compare (rank x) (rank y)
        end

        module Table = Map.Make (StructuralQualType)
        module Union = UnionFindMap (QualType.Qual)
        type t = QualType.t Table.t Union.t

        open Union
        let empty = empty
        let find = find
        let union = union
        let add = add
    end

    (* monad stack incorporating a unification table for unions *)
    module UnionTableQM = struct
        module State = StateT (UnionTable) (QM)
        include State

        (* lift monad operations *)
        let lift = State.lift

        module QualGraph = QM.QualGraph
        let add_edge ?label x y = lift (QM.add_edge ?label x y)
        let fresh = lift QM.fresh
        let eq x y = lift (QM.eq x y)
        let leq x y = lift (QM.leq x y)
        let lub x y = lift (QM.lub x y)
        let glb x y = lift (QM.glb x y)
        let annot x c = lift (QM.annot x c)

        (* monadic interface to UnionTable *)
        open UnionTable
        let empty = empty

        let find_table qv = perform
            map <-- get;
            try
                let head, rank, table, map' = Union.find_compress qv map in
                if map == map' then
                    return (head, table)
                else perform
                    put map';
                    return (head, table)
            with Not_found -> perform
                (* initialize a new table for qv *)
                put (Union.add qv Table.empty map);
                return (qv, Table.empty)

        let rec union_table qx qy = perform
            map <-- get;
            try
                let x, y, unified, map' = Union.union qx qy map in
                if map == map' then
                    return (x, y, unified)
                else perform
                    put map';
                    return (x, y, unified)
            with Not_found -> perform
                (* initialize and try again *)
                find_table qx;
                find_table qy;
                union_table qx qy

        let update_table qv table = perform
            modify (fun map -> Union.add qv table map)
    end
    include UnionTableQM
    module Ops = MonadOps (UnionTableQM)
    open Ops


    (* qualified-type constructors *)
    let embed e qt = perform
        let qv = QualType.Qual.Var (QualType.Var.Embed e) in
        return (QualType.create qt qv)
    let fresh qt = perform
        qv <-- fresh;
        return (QualType.create qt qv)

    let empty = return QualType.Empty


    (* qualified-type constraints *)

    (* unify the union tables of two qualifier variables *)
    let unify x y =
        let rec unify_qt eq1 x y = match x, y with
            | QualType.Ref (qx, qtx), QualType.Ref (qy, qty) -> perform
                eq1 qx qy;
                unify_qt eq qtx qty;
            | QualType.Fn (qx, qtrx, qtpx), QualType.Fn (qy, qtry, qtpy) -> perform
                eq1 qx qy;
                unify_qt eq qtrx qtry;
                zipWithM_ (unify_qt eq) qtpx qtpy
            | QualType.Base qx, QualType.Base qy -> perform
                eq1 qx qy;
                unify eq qx qy
            | _, _ -> failwith "Impossible!"
        and unify eq1 x y = perform
            (x, y, unified) <-- union_table x y;
            match unified with
                | None ->
                    return ()
                | Some (tx, ty) -> perform
                    (* tx is now the representative table, so expand it to include and unify with all qualified
                     * types in ty *)
                    tz <-- UnionTable.Table.fold begin fun k qty tzM ->
                        try
                            (* just unify *)
                            let qtx = UnionTable.Table.find k tx in perform
                            unify_qt eq1 qtx qty;
                            tzM
                        with Not_found ->
                            (* expand tx first, then unify *)
                            let qtx = QualType.create (QualType.clone k) x in perform
                            unify_qt eq1 qtx qty;
                            liftM (UnionTable.Table.add k qtx) tzM
                    end ty (return tx);
                    update_table x tz
        in
        unify (fun _ _ -> return ()) x y


    (* select from the union table of qx the qualified type that structurally matches y, and splice the tail onto qx *)
    let select_table qx y = perform
        let splice = function
            | QualType.Ref (_, qty)       -> return (QualType.Ref (qx, qty))
            | QualType.Fn (_, qtry, qtpy) -> return (QualType.Fn (qx, qtry, qtpy))
            | QualType.Base _
            | QualType.Empty              -> failwith "Impossible!"
        in
        (qx, tx) <-- find_table qx; (* also substitute qx with it's representative *)
        try
            splice (UnionTable.Table.find y tx)
        with Not_found -> perform
            (* expand tx to include a qualified type that matches y *)
            let x = QualType.create (QualType.clone y) qx in
            update_table qx (UnionTable.Table.add x x tx);
            splice x


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
        | QualType.Base ql, QualType.Base qv -> perform
            eq qv ql;
            unify ql qv
        | QualType.Base ql, QualType.Ref (qv, _)
        | QualType.Base ql, QualType.Fn (qv, _, _) -> perform
            l <-- select_table ql v;
            assign_ref l v
        | QualType.Ref (ql, _), QualType.Base qv
        | QualType.Fn (ql, _, _), QualType.Base qv -> perform
            v <-- select_table qv l;
            assign_ref l v
        | _, _ ->
            failwith "TODO: report incompatible assign_ref"
    and assign l v = match l, v with
        | QualType.Empty, _ ->
            failwith "TODO: report invalid assign"
        | _, QualType.Empty ->
            return ()
        | QualType.Ref (ql, qtl), QualType.Ref (qv, qtv) -> perform
            leq qv ql;
            assign_ref qtl qtv   (* referenced types should be non-variant *)
        | QualType.Fn (ql, qtrl, qtpl), QualType.Fn (qv, qtrv, qtpv) -> perform
            leq qv ql;
            assign qtrl qtrv;
            zipWithM_ assign qtpv qtpl (* parameters are contravariant *)
        | QualType.Base ql, QualType.Base qv -> perform
            leq qv ql;
            unify ql qv
        | QualType.Base ql, QualType.Ref (qv, _)
        | QualType.Base ql, QualType.Fn (qv, _, _) -> perform
            l <-- select_table ql v;
            assign l v
        | QualType.Ref (ql, _), QualType.Base qv
        | QualType.Fn (ql, _, _), QualType.Base qv -> perform
            v <-- select_table qv l;
            assign l v
        | _, _ ->
            failwith "TODO: report incompatible assign"

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
            unify qz qx;
            unify qz qy;
            return (QualType.Base qz)
        | QualType.Base qx, _ -> perform
            x <-- select_table qx y;
            join x y
        | _, QualType.Base qy -> perform
            y <-- select_table qy x;
            join x y
        | _, _ ->
            failwith "TODO: report incompatible merge"
    and join x y = merge join meet lub x y
    and meet x y = merge meet join glb x y (* i.e., invert the lattice *)


    (* qualified-type operations *)
    let annot qt s = match qt with
        | QualType.Ref (v, _)
        | QualType.Fn (v, _, _)
        | QualType.Base v -> perform
            annot v s;
            return qt
        | QualType.Empty -> failwith "TODO: report invalid annot"

    let deref = function
        | QualType.Ref (_, qt) ->
            return qt
        | QualType.Fn _
        | QualType.Base _
        | QualType.Empty -> failwith "TODO: report invalid deref"

    let app qtf qta = match qtf with
        | QualType.Fn (_, qtr, qtp) -> perform
            zipWithM_ assign qtp qta;
            return qtr
        | QualType.Ref _
        | QualType.Base _
        | QualType.Empty -> failwith "TODO: report invalid app"

    let retval = function
        | QualType.Fn (_, qtr, _) ->
            return qtr
        | QualType.Ref _
        | QualType.Base _
        | QualType.Empty -> failwith "TODO: report invalid retval"

    let args = function
        | QualType.Fn (_, _, qtp) ->
            return qtp
        | QualType.Ref _
        | QualType.Base _
        | QualType.Empty -> failwith "TODO: report invalid args"
end

