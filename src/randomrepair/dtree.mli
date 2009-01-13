
(** protected methods decorator *)
type 'a protected

(** conditional probability distribution tree representation
    @param mass mass plays two roles: nodes initialized with mass > 0.0 contribute to the distribution,
                whereas nodes initialized with mass = 0 groups the nodes under them, in which case the
                mass is used internally as a normalizer.
*)
class ['data] tree :
    float ->
    'data ->
    object ('self)
        inherit ['data] Domz.tree

        method count : int
        method weight : float
        method mass : float

        method setCount : (int -> 'self) protected
        method setWeight : (float -> 'self) protected
        method setMass : (float -> 'self) protected
        method adjust : (int -> float -> 'self) protected
    end

(** conditional probability distribution tree interface *)
class virtual ['tree] node :
    object ('self)
        constraint 'tree = 'data #tree
        inherit ['tree] Domz.node

        method count : int
        method weight : float
        method mass : float

        (** it's weight among siblings *)
        method setWeight : float -> 'self

        (** find the child at p in the distribution below this node, and return its offset *)
        method findN : float -> int

        method _remove : 'self protected
        method _adjust : (int -> float -> 'self) protected
    end
