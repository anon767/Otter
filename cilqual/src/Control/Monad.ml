open Data

(* basic monad signature *)
module type Monad = sig
    type 'a monad
    val return : 'a -> 'a monad
    val bind   : 'a monad -> ('a -> 'b monad) -> 'b monad

    type 'a result
    val run    : 'a monad -> 'a result
end


(* extended monad operations *)
module MonadOps (M : Monad) = struct
    open M
    open List
    let bind_ m k = bind m (fun _ -> k)

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
    type 'a monad = Id of 'a
    let return a      = Id a
    let bind (Id a) f = f a

    type 'a result  = 'a
    let run (Id x) = x
end


(* reader monad *)
module ReaderT (R : Type)  (M : Monad) = struct
    type 'a monad = Reader of (R.t -> 'a M.monad)
    let runReader (Reader m) = m
    let return a = Reader (fun _ -> M.return a)
    let bind m k = Reader (fun r -> M.bind (runReader m r) (fun a -> runReader (k a) r))
    let lift m   = Reader (fun _ -> m)

    type 'a result = R.t -> 'a M.result
    let run (Reader m) = fun r -> M.run (m r)

    let ask       = Reader M.return
    let local f m = Reader (fun r -> runReader m (f r))
end


(* state transformer *)
module StateT (S : Type)  (M : Monad) = struct
    type 'a monad = State of (S.t -> ('a * S.t) M.monad)
    let runState (State m) = m
    let return a = State (fun s -> M.return (a, s))
    let bind m k = State (fun s -> M.bind (runState m s) (fun (a, s') -> runState (k a) s'))
    let lift a   = State (fun s -> M.bind a (fun a' -> M.return (a', s)))

    type 'a result = S.t -> ('a * S.t) M.result
    let run (State m) = fun s -> M.run (m s)

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
    include StateT (struct type t = int end) (M)
    let fresh = bind get (fun s -> bind (put (s + 1)) (fun _ -> return (Value.fresh s)))
end


(* environment transformer to map names to values/types/qualifiers variables/etc *)
module EnvT (Key : OrderedType) (Value : Type) (M : Monad) = struct
    module Env = Map.Make (Key)
    type env = Value.t Env.t
    let empty = Env.empty

    include StateT (struct type t = env end) (M)
    let update v x = modify (fun s -> Env.add v x s)
    let lookup v   = bind get (fun s -> return (try Some (Env.find v s) with Not_found -> None))
end

