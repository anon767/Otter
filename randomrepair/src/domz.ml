
(** Zipper-based tree data structure with W3C DOM-like interface
    http://www.st.cs.uni-sb.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf
    http://www.w3.org/TR/DOM-Level-3-Core/core.html
    http://www.w3.org/TR/DOM-Level-2-Traversal-Range/traversal.html
*)

(** protected methods decorator *)
type 'a protected = 'a

(** operations on null node (just removed node) *)
exception Null_node

(** tree representation *)
class ['data] tree data =
    object (self : 's)
        val data : 'data = data
        val left : 's list = []
        val right : 's list = []

        method data = data
        method setData data = {< data = data >}

        method left = left
        method setLeft left = {< left = left >}

        method right = right
        method setRight right = {< right = right >}

        method children = (left, right)
        method setChildren left right = {< left = left; right = right >}

        method printer ff = Format.fprintf ff "<data>"
    end
        
    
(** zipper-based tree-traversal interface *)
class virtual ['tree] node =
    object (self : 's)
        constraint 'tree = 'data #tree
        val virtual tree : 'tree option
        val path : ('tree list * 's * 'tree list) option = None

        method tree = match tree with
            | Some current -> current
            | None -> raise Null_node
        method _path = path

        method data = self#tree#data
        method setData d' = {< tree = Some (self#tree#setData d') >}
        method isNull = tree = None

        method hasChildNodes = let left, right = self#tree#children in
            left <> [] || right <> []


        (* up *)
        method parentNode = match path with
            | Some (left, parent, right) ->
                let parent'tree = parent#tree in
                {< path = parent#_path;
                   tree = Some (parent'tree#setChildren left (if self#isNull then right else self#tree::right) ) >}
            | _ -> raise Not_found

        method rootNode = match path with
            | None -> self
            | Some _ -> self#parentNode#rootNode


        (* down *)
        method firstChild = let tree = self#tree in
            match List.rev_append tree#left tree#right with
                | first::rest -> {< path = Some ([], self, rest); tree = Some first >}
                | _ -> raise Not_found

        method lastChild = let tree = self#tree in
            match List.rev_append tree#right tree#left with
                | last::rest -> {< path = Some (rest, self, []); tree = Some last >}
                | _ -> raise Not_found

        method recentChild = let tree = self#tree in
            match tree#children with
                | left, child::rest  -> {< path = Some (left, self, rest);  tree = Some child >}
                | child::rest, right -> {< path = Some (rest, self, right); tree = Some child >}
                | _, _ -> raise Not_found


        (* right *)
        method nextSibling = match path with
            | Some (left, parent, next::rest) ->
                {< path = Some ((if self#isNull then left else self#tree::left), parent, rest); tree = Some next >}
            | _ -> raise Not_found


        (* left *)
        method previousSibling = match path with
            | Some (previous::rest, parent, right) ->
                {< path = Some (rest, parent, (if self#isNull then right else self#tree::right)); tree = Some previous >}
            | _ -> raise Not_found


        (* mutation *)
        method appendChild (c' : 's) =
            let tree = self#tree in
            let c'tree = c'#tree in
            {< tree = Some (tree#setChildren (List.rev_append tree#right tree#left) [ c'tree ]) >}

        method remove = ignore self#tree; {< tree = None >}


        (* printing *)
        method printer ff =
            let node_printer mark ff = Format.fprintf ff "@\n@[<hov 2>%c%t@]" mark in
            let rec tree_printer mark ff x =
                node_printer mark ff (fun ff -> Format.fprintf ff "%t%a" x#printer tree_list_printer (List.rev_append x#left x#right))
            and tree_list_printer ff =
                List.iter (tree_printer ' ' ff)
            and parents_printer ff = function
                | [] ->
                    if self#isNull then
                        node_printer ' ' ff (fun ff -> Format.pp_print_string ff "<Null node>")
                    else
                        tree_printer '>' ff self#tree
                | (left, parent, right)::child ->
                    node_printer ' ' ff
                        (fun ff -> Format.fprintf ff "%t%a%a%a"
                            parent#tree#printer
                            tree_list_printer (List.rev left)
                            parents_printer child
                            tree_list_printer right
                        )
            in
            let parents =
                let rec parents paths = function
                    | None -> paths
                    | Some ((_, up, _) as path) -> parents (path::paths) up#_path
                in
                parents [] path
            in
            Format.fprintf ff "@[%a@]@\n" parents_printer parents
      end

(** pretty printer for node *)
let node_printer ff (x : 'a node) = x#printer ff;;

(** node filter action *)
type nodeFilterAction =
    | Accept (** show node *)
    | Reject (** don't show node, but do show it's children *)
    | Skip   (** don't show node or it's children *)

(** node filter interface *)
type 'node nodeFilter = 'node -> nodeFilterAction

(** sub-tree traversal with filtering *)
class ['node] treeWalker ?(filter=fun _ -> Accept) rootNode =
    object (self : 's)
        constraint 'node = 'data #node
        val filter : 'node nodeFilter = filter
        val currentNode : 'node = rootNode
        val rootNode : 'node = rootNode
        val depth = 0

        (* dereference *)
        method currentNode = currentNode
        method rootNode = rootNode

        (* tree traversal *)
        method parentNode =
            if depth = 0 then raise Not_found;
            let parent = {< depth = depth - 1; currentNode = currentNode#parentNode >} in
            begin match filter parent#currentNode with
                | Accept -> parent
                | Skip | Reject -> parent#parentNode
            end

        method firstChild =
            let rec eachChild node =
                try (* go down *)
                    match filter node with
                        | Accept -> {< depth = depth + 1; currentNode = node >}
                        | Skip -> {< depth = depth + 1; currentNode = node >}#firstChild
                        | Reject -> raise Not_found
                with Not_found -> (* go right *)
                    eachChild node#nextSibling
            in
            eachChild currentNode#firstChild

        method lastChild =
            let rec eachChild node =
                try (* go down *)
                    match filter node with
                        | Accept -> {< depth = depth + 1; currentNode = node >}
                        | Skip -> {< depth = depth + 1; currentNode = node >}#lastChild
                        | Reject -> raise Not_found
                with Not_found -> (* go right *)
                    eachChild node#previousSibling
            in
            eachChild currentNode#lastChild

        method previousSibling =
            try
                let rec eachSibling node =
                    match filter node with
                        | Accept -> {< currentNode = node >}
                        | Skip ->
                            begin try {< currentNode = node >}#lastChild
                                with Not_found -> eachSibling node#previousSibling
                            end
                        | Reject -> eachSibling node#previousSibling
                in
                eachSibling currentNode#previousSibling
            with Not_found -> begin
                if depth = 0 then raise Not_found;
                let parentNode = currentNode#parentNode in
                begin match filter parentNode with
                    | Accept -> raise Not_found
                    | Skip | Reject -> {< depth = depth - 1; currentNode = parentNode >}#previousSibling
                end
            end

        method nextSibling =
            try
                let rec eachSibling node =
                    match filter node with
                        | Accept -> {< currentNode = node >}
                        | Skip ->
                            begin try {< currentNode = node >}#firstChild
                                with Not_found -> eachSibling node#nextSibling
                            end
                        | Reject -> eachSibling node#nextSibling
                in
                eachSibling currentNode#nextSibling
            with Not_found -> begin
                if depth = 0 then raise Not_found;
                let parentNode = currentNode#parentNode in
                begin match filter parentNode with
                    | Accept -> raise Not_found
                    | Skip | Reject -> {< depth = depth - 1; currentNode = parentNode >}#nextSibling
                end
            end

        (* pre-order traversal *)
        method previousNode =
            try
                let rec downprev node =
                    try downprev node#lastChild
                    with Not_found -> node
                in
                downprev self#previousSibling
            with Not_found -> try self#previousSibling
            with Not_found -> self#parentNode

        method nextNode =
            try self#firstChild
            with Not_found -> try self#nextSibling
            with Not_found ->
                let rec upnext node =
                    try node#nextSibling
                    with Not_found -> upnext node#parentNode
                in
                upnext self#parentNode

        (* printing *)
        method printer =
            currentNode#printer

    end

(** treeWalker pretty printer *)
let treeWalker_printer ff (x : 'a treeWalker) = x#printer ff;;


(*
#load "Domz.cmo";;
open Domz
#install_printer treeWalker_printer;;
#install_printer tree_printer;;
#install_printer node_printer;;
class str_tree s = object inherit [string] tree s val str = s method str = str method printer ff = Format.fprintf ff "%s" s end;;
class str_node s = object inherit [str_tree] node val tree = Some (new str_tree s) end;;
let a = new str_node "a";;
let b = new str_node "b";;
let c = new str_node "c";;
let d = a#appendChild c;;
let e = d#appendChild b;;
let f = e#firstChild#appendChild (new str_node "w");;
let g = f#remove;;
let h = g#setData "d";;
let i = g#parentNode#remove;;
let j = f#appendChild a
let k = j#recentChild#remove;;
let l = a#remove;;
let m = f#remove;;
let n = m#remove;;
let o = m#nextSibling#remove;;
let p = f#rootNode;;
let aa = f#appendChild (new str_node "c");;
let aa = aa#appendChild (new str_node "d");;
let aa = aa#firstChild#appendChild (new str_node "d");;
let aa = aa#appendChild (new str_node "e");;
let aa = aa#firstChild#appendChild (new str_node "f");;
let aa = aa#appendChild (new str_node "c");;
let aa = aa#recentChild#appendChild (new str_node "d");;
let aa = aa#appendChild (new str_node "e");;
let aa = aa#appendChild (new str_node "f");;
let aa = aa#parentNode#appendChild (new str_node "d");;
let aa = aa#firstChild#appendChild (new str_node "e");;
let aa = aa#parentNode#appendChild (new str_node "e");;
let aa = aa#recentChild#appendChild (new str_node "c");;
let aa = aa#firstChild#appendChild (new str_node "g");;
let aa = aa#firstChild#appendChild (new str_node "h");;
let aa = aa#firstChild#appendChild (new str_node "i");;
let aa = aa#firstChild#appendChild (new str_node "j");;
let aa = aa#firstChild#appendChild (new str_node "k");;
let aa = aa#rootNode#appendChild (new str_node "l");;
let aa = aa#lastChild#appendChild (new str_node "m");;
let aa = aa#appendChild (new str_node "n");;
let aa = aa#rootNode#appendChild (new str_node "o");;
let aa = aa#rootNode;;
let af = new treeWalker ~filter:(fun node -> if node#data = "c" then Reject else Accept) aa#rootNode;;
let af = af#parentNode;;
let af = af#firstChild;;
let af = af#lastChild;;
let af = af#previousSibling;;
let af = af#nextSibling;;
let ag = new treeWalker ~filter:(fun node -> if node#data = "c" then Skip else Accept) aa#rootNode;;
let ag = ag#parentNode;;
let ag = ag#firstChild;;
let ag = ag#lastChild;;
let ag = ag#previousSibling;;
let ag = ag#nextSibling;;
let ag = ag#previousNode;;
let ag = ag#nextNode;;
*)
