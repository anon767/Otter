open Control.Data
open Control.Monad
open Qual


(* qualified-type type *)
module QualTypeConstraints = struct
    (* qualifier variables that encode types in reverse *)
    module TypeVar = struct
        type t = Fresh of int
               | Deref of t
               | FnRet of t
               | FnArg of int * t

        let rec compare x y = if x == y then 0 else match x, y with
            | Fresh x, Fresh y ->
                Pervasives.compare x y
            | Deref x, Deref y
            | FnRet x, FnRet y ->
                compare x y
            | FnArg (ix, x), FnArg (iy, y) ->
                begin match Pervasives.compare ix iy with
                    | 0 -> compare x y
                    | i -> i
                end
            | x, y ->
                let rank = function Fresh _ -> 0 | Deref _ -> 1 | FnRet _ -> 2 | FnArg _ -> 3 in
                Pervasives.compare (rank x) (rank y)

        let rec hash = function
            | Fresh i -> i
            | Deref x -> 1 + (31 * hash x)
            | FnRet x -> 2 + (31 * hash x)
            | FnArg (i, x) -> 3 + i + (31 * hash x)

        let equal x y = (compare x y) = 0

        let rec printer ff = function
            | Fresh i      -> Format.fprintf ff "Fresh %d" i
            | Deref v      -> Format.fprintf ff "Deref @[(%a)@]" printer v
            | FnRet v      -> Format.fprintf ff "FnRet @[(%a)@]" printer v
            | FnArg (i, v) -> Format.fprintf ff "FnArg:%d @[(%a)@]" i printer v

        let fresh i = Fresh i
    end
    open TypeVar

    module Qual = Qual (TypeVar) (String)
    open Qual

    (* qualifier variable constructors *)
    let const s = Const s
    let deref = function
        | Var v -> Var (Deref v)
        | Const _ -> failwith "TODO: report deref of Const"
    let fnRet = function
        | Var v -> Var (FnRet v)
        | Const _ -> failwith "TODO: report fnRet of Const"
    let fnArg i = function
        | Var v -> Var (FnArg (i, v))
        | Const _ -> failwith "TODO: report fnArg of Const"

    (* qualifier variable operations *)
    let typevar = function
        | Var v -> v
        | Const _ -> failwith "TODO: report typevar of Const"
    let subst x y = function
        | Var v ->
            let x = typevar x in
            let y = typevar y in
            let rec subst v = if TypeVar.equal v x then y else match v with
                | Fresh _ as f -> f
                | Deref v      -> Deref (subst v)
                | FnRet v      -> FnRet (subst v)
                | FnArg (i, v) -> FnArg (i, subst v)
            in Var (subst v)
        | Const _ as c -> c

    module TypeVarLabel = struct
        type t = Default
               | TypeVar of TypeVar.t

        let compare x y = if x == y then 0 else match x, y with
            | TypeVar x, TypeVar y ->
                TypeVar.compare x y
            | x, y ->
                let rank = function Default -> 0 | TypeVar _ -> 1 in
                Pervasives.compare (rank x) (rank y)
        let default = Default

        let printer ff = function
            | TypeVar (Deref _)      -> Format.fprintf ff "Deref"
            | TypeVar (FnRet _)      -> Format.fprintf ff "FnRet"
            | TypeVar (FnArg (i, _)) -> Format.fprintf ff "FnArg:%d" i
            | _ -> ()
    end
    open TypeVarLabel

    module ConstraintGraph = struct
        include Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Qual) (TypeVarLabel)

        let add_typevar_edge g x y = add_edge_e g (E.create x (TypeVar (typevar x)) y)

        (* Type extension edges:
         *      var:    x -----> y      var:    x -----> y      var:    x -----> y
         *              ^        ^              ^        ^              ^        ^
         *              |        |              |        |              |        |
         *      deref: *x <====> *y     fnRet: rx =====> ry     fnArg: ax <===== ay
         *      (non-variant)           (co-variant)            (contra-variant)
         *)
        type cfl = TypeVar.t list
        let start_cfl = []
        let accept_cfl = Pervasives.(=) []
        let iter_succ_vl f = iter_succ_e (fun e -> f (E.dst e) (E.label e))
        let iter_pred_vl f = iter_pred_e (fun e -> f (E.src e) (E.label e))
        let iter_cfl f g x cfl = match cfl with
            (* start: all forward edges *)
            | [] ->
                iter_succ_vl begin fun y l -> match l with
                    | TypeVar tv -> f y (tv::cfl)
                    | Default -> f y cfl
                end g x

            (* deref edge: non-variant *)
            | (Deref _)::tail ->
                iter_succ_vl begin fun y l -> match l with
                    | TypeVar tv -> f y (tv::cfl)
                    | Default -> f y cfl
                end g x;
                iter_pred_vl begin fun y l -> match l with
                    | TypeVar (Deref _) -> f y tail
                    | TypeVar _ -> ()
                    | Default -> f y cfl
                end g x

            (* fnRet edge: co-variant *)
            | (FnRet _)::tail ->
                iter_succ_vl begin fun y l -> match l with
                    | TypeVar tv -> f y (tv::cfl)
                    | Default -> f y cfl
                end g x;
                iter_pred_vl begin fun y l -> match l with
                    | TypeVar (FnRet _) -> f y tail
                    | TypeVar _
                    | Default -> ()
                end g x

            (* fnRet edge: contra-variant *)
            | (FnArg (i, _))::tail ->
                iter_succ_vl begin fun y l -> match l with
                    | TypeVar tv -> f y (tv::cfl)
                    | Default -> ()
                end g x;
                iter_pred_vl begin fun y l -> match l with
                    | TypeVar (FnArg (j, _)) when i = j -> f y tail
                    | TypeVar _ -> ()
                    | Default -> f y cfl
                end g x

            | (Fresh _)::_ ->
                failwith "Impossible!"

        let vertex_printer = Qual.printer
        let edge_printer ff e =
            Format.fprintf ff "@[<2>%a@ <=%a= %a@]"
                Qual.printer (E.src e)
                TypeVarLabel.printer (E.label e)
                Qual.printer (E.dst e)

        let printer ff graph =
            ignore (fold_edges_e (fun e format -> Format.fprintf ff format edge_printer e; ",@ @[%a@]") graph "@[%a@]")
    end
    include ConstraintGraph

    module PathChecker = Graph.Cfl.Check (ConstraintGraph)
    type path_checker = PathChecker.path_checker
    let create_path_checker = PathChecker.create
    let check_path = PathChecker.check_path
end


module QualType = struct
    open QualTypeConstraints
    type const = Qual.const
    type t = Ref of Qual.t * t         (* reference types *)
           | Fn of Qual.t * t * t list (* function types *)
           | Base of Qual.t            (* base types *)
           | Empty                     (* empty/constant types (identity) *)

    include ReaderT (Qual) (Identity)

    (* explicit qualified-type constructors *)
    let ref qt = perform
        let subst_qt x y qt =
            let subst_qv = subst x y in
            let rec subst_qt = function
                | Ref (qv, qt)      -> Ref (subst_qv qv, subst_qt qt)
                | Fn (qv, qtr, qtp) -> Fn (subst_qv qv, subst_qt qtr, List.map subst_qt qtp)
                | Base qv           -> Base (subst_qv qv)
                | Empty             -> failwith "TODO: report invalid use of QualType constructor"
            in subst_qt qt
        in
        let rec shift_qt = function
            | Ref (qv, qt)   -> Ref (qv, shift_qt qt)
            | Fn (qv, _, _)
            | Base qv as qt  -> Ref (qv, subst_qt qv (deref qv) qt)
            | Empty          -> failwith "TODO: report invalid use of QualType constructor"
        in
        return (shift_qt qt)
    let fn qtr qtp = perform
        qv <-- ask;
        let qtr = match qtr with
            | Ref (_, qt)      -> Ref (fnRet qv, qt)
            | Fn (_, qtr, qtp) -> Fn (fnRet qv, qtr, qtp)
            | Base _           -> Base (fnRet qv)
            | Empty            -> failwith "TODO: report invalid use of QualType constructor"
        and (_, qtp) = List.fold_left begin fun (i, qtp) a ->
            let qt = match a with
                | Ref (_, qt)      -> Ref (fnArg i qv, qt)
                | Fn (_, qtr, qtp) -> Fn (fnArg i qv, qtr, qtp)
                | Base _           -> Base (fnArg i qv)
                | Empty            -> failwith "TODO: report invalid use of QualType constructor"
            in (i + 1, qt::qtp)
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
end


module type QualTypeMonad = sig
    include Monad
    module Constraints : Constraints

    val create : QualType.t QualType.monad -> QualType.t monad
    val empty  : QualType.t monad

    val assign : QualType.t -> QualType.t -> unit monad
    val join   : QualType.t -> QualType.t -> QualType.t monad
    val meet   : QualType.t -> QualType.t -> QualType.t monad

    val annot  : QualType.t -> QualType.const list -> QualType.t monad
    val deref  : QualType.t -> QualType.t monad
    val app    : QualType.t -> QualType.t list -> QualType.t monad
    val retval : QualType.t -> QualType.t monad
    val args   : QualType.t -> QualType.t list monad
end


module QualTypeT (M : Monad) = struct
    (* monad stack *)
    module Q = QualT (QualTypeConstraints) (M)
    include Q
    (* module Ops = MonadOps (Q) *)
    open Ops

    let create qt = perform
        qv <-- fresh;
        return (QualType.run qt qv)

    let empty = return QualType.Empty

    (* qualified-type extenders *)
    let extend_type x y =
        modify (fun g -> QualTypeConstraints.add_typevar_edge g x y)
    let extend_deref qv = perform
        let qv' = QualTypeConstraints.deref qv in
        extend_type qv' qv;
        return (QualType.Base qv')
    let extend_fnRet qv = perform
        let qv' = QualTypeConstraints.fnRet qv in
        extend_type qv' qv;
        return (QualType.Base qv')
    let extend_fnArg i qv = perform
        let qv' = QualTypeConstraints.fnArg i qv in
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
    let annot qt qlist = match qt with
        | QualType.Ref (qv, _)
        | QualType.Fn (qv, _, _)
        | QualType.Base qv -> perform
            annot qv qlist;
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

