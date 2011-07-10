open OtterCore
open OtterJob

class ['abandoned, 'truncated] t file cmdline :
    object ('self)
        inherit ['abandoned, 'truncated] FileJob.t
        inherit BackOtterJobExtension.t
    end
=
    object (self : 'self)
        inherit ['abandoned, 'truncated] FileJob.t file cmdline as job_super
        inherit BackOtterJobExtension.t as b_super

        (* TODO: can we not duplicate this in BackOtterFunctionJob? *)
        method append_decision_path decision = 
            if !BackOtterJobExtension.arg_enable_record_decision then
                let job = job_super#append_decision_path decision in
                job#postprocess_append_decision_path decision
            else self

        method become (other : 'self) =
            job_super#become other;
            b_super#become other

    end

