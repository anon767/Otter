open OtterCore
open OtterJob

class t file cmdline : 
    object ('self)
        inherit FileJob.t 
        inherit BackOtterJobExtension.t
    end
=
    object (self : 'self)
        inherit FileJob.t file cmdline as job_super
        inherit BackOtterJobExtension.t as b_super

        method become (other : 'self) = 
            job_super#become other;
            b_super#become other

    end

