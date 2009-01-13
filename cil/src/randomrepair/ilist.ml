(* Iterator-supported list *)
(* TODO: is there a way to make cell type = 'a cell option? *)
class ['a] cell (content : 'a) =
    object (self)
        val mutable content = content
        val mutable nextcell : 'a cell option = None
        method get = content
        method set x = content <- x
        method next = nextcell
        method setNext c = nextcell <- Some c
    end

class ['a] iterator (cell : 'a cell option) =
    object
        val mutable cell : 'a cell option = cell
        val mutable prev : 'a cell option = None
        method next =
            match cell with
                | Some c -> begin
                        prev <- cell;
                        cell <- c#next;
                        c#get
                    end
                | None -> raise Not_found
        method set x =
            match prev with
                | Some c -> c#set x
                | None -> raise Not_found
    end

class ['a] c =
    object (self)
        val mutable head : 'a cell option = None
        val mutable tail : 'a cell option = None
        method append x =
            match tail with
                | Some c -> begin
                        let newcell = new cell x in
                        c#setNext newcell;
                        tail <- Some newcell
                    end
                | None -> begin
                        head <- Some (new cell x);
                        tail <- head
                    end
        method appendList l =
            List.iter self#append l
        method clear =
            head <- None;
            tail <- None
        method iterator = new iterator head
    end

