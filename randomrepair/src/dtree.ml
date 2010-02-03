
(** protected methods decorator *)
type 'a protected = 'a

(** conditional probability distribution tree representation
    @param mass mass plays two roles: nodes initialized with mass > 0.0 contribute to the distribution,
                whereas nodes initialized with mass = 0 groups the nodes under them, in which case the
                mass is used internally as a normalizer.
*)
class ['data] tree mass data =
    let _ = if mass < 0.0 then invalid_arg "mass must be >= 0.0" in
    object
        inherit ['data] Domz.tree data as super
        val count = if mass > 0.0 then 1 else 0
        val weight = 1.0
        val mass = mass

        method count = count
        method setCount count = {< count = count >}
        method weight = weight
        method setWeight weight = {< weight = weight >}
        method mass = mass
        method setMass mass = {< mass = mass >}
        method adjust count mass = {< count = count; mass = mass >}
    end

(** conditional probability distribution tree interface *)
class virtual ['tree] node =
    object (self : 's)
        constraint 'tree = 'data #tree
        inherit ['tree] Domz.node as super

        method count = self#tree#count
        method weight = self#tree#weight
        method mass = self#tree#mass

        (** it's weight among siblings *)
        method setWeight weight =
            if weight < 0.0 then invalid_arg "weight must be >= 0";
            let tree = self#tree in
            let diff = (weight -. tree#weight) *. tree#mass in
            let newnode = {< tree = Some (tree#setWeight weight) >} in
            try (newnode#parentNode#_adjust 0 diff)#recentChild
            with Not_found -> newnode

        (** find the child at p in the distribution below this node, and return its offset *)
        method findN p =
            let rec findN n prec mu node =
                let mu' = mu *. node#weight in
                let prec' = prec +. (mu' *. node#mass) in
                if prec' <= p then
                    findN (n + node#count) prec' mu node#nextSibling
                else
                    match (try Some node#firstChild with Not_found -> None) with
                        | Some child -> findN n prec (mu *. node#weight) child
                        | None -> n
            in
            (* weight at current node only applies to it's relationship to it's parent *)
            findN 0 0.0 (1.0 /. self#mass) self#firstChild

        method appendChild (c : 's) =
            let newnode = super#appendChild c in
            newnode#_adjust c#count (c#weight *. c#mass)

        method remove =
            let tree = self#tree in
            let newnode = self#_adjust (-tree#count) (-.tree#weight *. tree#mass) in
            newnode#_remove (* do remove last to ensure that the null-hole is intact *)

        method _remove =
            super#remove

        method _adjust dcount dmass =
            let tree = self#tree in
            let mass = tree#mass +. dmass in
            let count = tree#count + dcount in
            let diff = tree#weight *. dmass in
            let newnode = {< tree = Some (tree#adjust count mass) >} in
            try (newnode#parentNode#_adjust dcount diff)#recentChild
            with Not_found -> newnode
    end
