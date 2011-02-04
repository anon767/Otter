(** Useful extensions to the {!List} module. *)

(** Remove the first matching item from a list. *)
let remove_first f list =
    let rec remove list = function
        | item::rest when f item -> Some (item, List.rev_append list rest)
        | item::rest -> remove (item::list) rest
        | [] -> None
    in
    remove [] list

(** Fold over a list using a binary-subdivision scheme. *)
let foldm f xs =
    let rec foldm xs = function
        | x::y::ys -> foldm ((f x y)::xs) ys
        | [ x ] -> foldm_next (x::xs)
        | [] -> foldm_next xs
    and foldm_next = function
        | [ x ] -> x
        | [] -> invalid_arg "empty list"
        | xs -> foldm [] xs
    in
    foldm [] xs
