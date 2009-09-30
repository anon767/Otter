open Data

(* basic monad signature *)
module type Monad = sig
    type 'a monad
    val return : 'a -> 'a monad
    val bind   : 'a monad -> ('a -> 'b monad) -> 'b monad

    type 'a param
    val focus : ('a * 'b) param -> 'a param * 'b
    val run   : 'a monad -> 'x param -> 'a param
end


(* extended monad operations *)
module MonadOps (M : Monad) = struct
    open M
    open List
    let bind_ m k = bind m (fun _ -> k)

    let liftM f m = bind m (fun x -> return (f x))

    let mapM f xs =
        let rec mapM f xs acc = match xs with
            | x'::xs' -> bind (f x') (fun r -> mapM f xs' (r::acc))
            | []      -> return (rev acc)
        in
        mapM f xs []

    let rec mapM_ f xs = match xs with
        | x'::xs' -> bind_ (f x') (mapM_ f xs')
        | []      -> return ()

    let zipWithM f xs ys =
        let rec zipWithM f xs yx acc = match xs, ys with
            | x'::xs', y'::ys' -> bind (f x' y') (fun r -> zipWithM f xs' ys' (r::acc))
            | _, _             -> return (rev acc)
        in
        zipWithM f xs ys []

    let rec zipWithM_ f xs ys = match xs, ys with
        | x'::xs', y'::ys' -> bind_ (f x' y') (zipWithM_ f xs' ys')
        | _, _             -> return ()

    let rec foldM f a = function
        | x::xs -> bind (f a x) (fun y -> foldM f y xs)
        | []    -> return a
end


(* identity monad : serves as base monad for monad transformers *)
module Identity = struct
    type 'a monad = Identity of 'a
    let runIdentity (Identity a) = a
    let return a = Identity a
    let bind a f = f (runIdentity a)

    type 'a param = 'a
    let focus x = x
    let run (Identity x) = fun _ -> x
end


(* reader monad *)
module ReaderT (R : Type) (M : Monad) = struct
    type 'a monad = Reader of (R.t -> 'a M.monad)
    let runReader (Reader m) = m
    let return a = Reader (fun _ -> M.return a)
    let bind m k = Reader (fun r -> M.bind (runReader m r) (fun a -> runReader (k a) r))
    let lift m   = Reader (fun _ -> m)

    type 'a param = 'a M.param * R.t
    let focus (a, r) = let x, y = M.focus a in (x, r), y
    let run (Reader m) = fun (a, r) -> (M.run (m r) a, r)

    let ask       = Reader M.return
    let local f m = Reader (fun r -> runReader m (f r))
end


(* state transformer *)
module StateT (S : Type) (M : Monad) = struct
    type 'a monad = State of (S.t -> ('a * S.t) M.monad)
    let runState (State m) = m
    let return a = State (fun s -> M.return (a, s))
    let bind m k = State (fun s -> M.bind (runState m s) (fun (a, s') -> runState (k a) s'))
    let lift a   = State (fun s -> M.bind a (fun a' -> M.return (a', s)))

    type 'a param = 'a M.param * S.t
    let focus (a, s) = let x, y = M.focus a in (x, s), y
    let run (State m) = fun (a, s) -> M.focus (M.run (m s) a)

    let get      = State (fun s -> M.return (s, s))
    let put s    = State (fun _ -> M.return ((), s))
    let modify f = State (fun s -> M.return ((), f s))
end


module type FreshType = sig
    include Type
    val fresh : int -> t
end


(* monad-transformer to manage fresh variables *)
module FreshT (Value : FreshType) (M : Monad) = struct
    type 'a monad = Fresh of (int -> ('a * int) M.monad)
    let runFresh (Fresh m) = m
    let return a = Fresh (fun s -> M.return (a, s))
    let bind m k = Fresh (fun s -> M.bind (runFresh m s) (fun (a, s') -> runFresh (k a) s'))
    let lift a   = Fresh (fun s -> M.bind a (fun a' -> M.return (a', s)))

    type 'a param = 'a M.param * int
    let focus (a, s) = let x, y = M.focus a in (x, s), y
    let run (Fresh m) = fun (a, s) -> M.focus (M.run (m s) a)

    let fresh = Fresh (fun s -> M.return (Value.fresh s, s + 1))
end


(* environment transformer to map names to values/types/qualifiers variables/etc *)
module EnvT (Key : OrderedType) (Value : Type) (M : Monad) = struct
    module Env = Map.Make (Key)
    type env = Value.t Env.t
    let empty = Env.empty

    type 'a monad = Env of (env -> ('a * env) M.monad)
    let runEnv (Env m) = m
    let return a = Env (fun s -> M.return (a, s))
    let bind m k = Env (fun s -> M.bind (runEnv m s) (fun (a, s') -> runEnv (k a) s'))
    let lift a   = Env (fun s -> M.bind a (fun a' -> M.return (a', s)))

    type 'a param = 'a M.param * env
    let focus (a, s) = let x, y = M.focus a in (x, s), y
    let run (Env m) = fun (a, s) -> M.focus (M.run (m s) a)

    let update v x = Env (fun s -> M.return ((), Env.add v x s))
    let lookup v   = Env (fun s -> M.return ((try Some (Env.find v s) with Not_found -> None), s))
end

