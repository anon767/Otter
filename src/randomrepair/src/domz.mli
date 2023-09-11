
(** Zipper-based tree data structure with W3C DOM-like interface
    http://www.st.cs.uni-sb.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf
    http://www.w3.org/TR/DOM-Level-3-Core/core.html
    http://www.w3.org/TR/DOM-Level-2-Traversal-Range/traversal.html
*)

(** protected methods decorator *)
type 'a protected

(** operations on null node (just removed node) *)
exception Null_node

(** tree representation *)
class ['data] tree :
    'data ->
    object ('self)
        method printer : Format.formatter -> unit

        method data : 'data

        method setData : ('data -> 'self) protected
        method left : ('self list) protected
        method setLeft : ('self list -> 'self) protected
        method right : ('self list) protected
        method setRight : ('self list -> 'self) protected
        method children : ('self list * 'self list) protected
        method setChildren : ('self list -> 'self list -> 'self) protected
    end

(** zipper-based tree-traversal interface *)
class virtual ['tree] node :
    object ('self)
        constraint 'tree = 'data #tree
        val virtual tree : 'tree option

        method tree : 'tree

        method data : 'data
        method setData : 'data -> 'self
        method isNull : bool
        method hasChildNodes : bool

        method parentNode : 'self
        method rootNode : 'self
        method firstChild : 'self
        method lastChild : 'self
        method recentChild : 'self
        method previousSibling : 'self
        method nextSibling : 'self

        (* mutation *)
        method appendChild : 'self -> 'self
        method remove : 'self

        (* printing *)
        method printer : Format.formatter -> unit

        (* protected (module-only) methods *)
        method _path : ('tree list * 'self * 'tree list) option protected
    end

(** pretty printer for node *)
val node_printer : Format.formatter -> 'data node -> unit

(** node filter action *)
type nodeFilterAction =
    | Accept (** show node *)
    | Reject (** don't show node, but do show it's children *)
    | Skip   (** don't show node or it's children *)

(** node filter interface *)
type 'node nodeFilter = 'node -> nodeFilterAction

(** filtered tree-traversal interface *)
class ['node] treeWalker :
    ?filter : 'node nodeFilter ->
    'node ->
    object ('self)
        constraint 'node = 'data #node

        (* dereference *)
        method currentNode : 'node
        method rootNode : 'node

        (* tree traversal *)
        method parentNode : 'self
        method firstChild : 'self
        method lastChild : 'self
        method previousSibling : 'self
        method nextSibling : 'self

        (* pre-order traversal *)
        method previousNode : 'self
        method nextNode : 'self

        (* printing *)
        method printer : Format.formatter -> unit
    end

(** treeWalker pretty printer *)
val treeWalker_printer : Format.formatter -> 'a #node treeWalker -> unit
