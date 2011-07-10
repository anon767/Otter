open OtterCore
open OtterJob

class ['abandoned, 'truncated] t file ?(points_to=(!FunctionJob.default_points_to) file) fn :
    object ('self)
        inherit ['abandoned, 'truncated] FunctionJob.t
        inherit BackOtterJobExtension.t
    end
=
    object (self : 'self)
        inherit ['abandoned, 'truncated] FunctionJob.t file ~points_to fn as job_super
        inherit BackOtterJobExtension.t as b_super

        method append_decision_path decision = 
            if self#enable_record_decisions then
                let job = job_super#append_decision_path decision in
                job#postprocess_append_decision_path decision
            else self

        method become (other : 'self) =
            job_super#become other;
            b_super#become other

    end

