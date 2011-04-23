class t :
    object ('self)
        (** Bounding paths
         *  None: this job is NOT a bounded job
         *  Some []: this is a bounded job, and has fallen out of bound
         *  Some paths: this is a bounded job, and is still bounded
         *)
        method bounding_paths : OtterCore.DecisionPath.t list option
        method with_bounding_paths : OtterCore.DecisionPath.t list option -> 'self
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

