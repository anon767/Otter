class t : 
    object ('self)
        method bounding_paths : OtterCore.Decision.t list list option
        method with_bounding_paths : OtterCore.Decision.t list list option -> 'self
        method become : 'self -> unit
    end
=
    object (self : 'self)
        val mutable bounding_paths = None
        method bounding_paths = bounding_paths
        method with_bounding_paths bounding_paths = {< bounding_paths = bounding_paths >}

        method become (other : 'self) = 
            bounding_paths <- other#bounding_paths
    end

