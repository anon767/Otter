open OtterCore
open OtterJob

class t file ?scheme ?(points_to=(!FunctionJob.default_points_to) file) fn :
    object ('self)
        inherit FunctionJob.t 
        inherit BackOtterJobExtension.t
    end
=
    object (self : 'self)
        inherit FunctionJob.t file ?scheme ~points_to fn as job_super
        inherit BackOtterJobExtension.t as b_super

        method become (other : 'self) = 
            job_super#become other;
            b_super#become other

        initializer
            let job = FunctionJob.job_initializer file ?scheme points_to fn self in
            self#become job
    end

