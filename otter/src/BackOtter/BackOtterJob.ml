open OtterCore

class t job =
    object (self : 'self)
        inherit Job.t job#file job#fn as job_super

        method become (other : 'self) = 
            job_super#become other

        initializer
            self#become job
    end

