open OtterCore
open OtterJob

class ['abandoned, 'truncated] t file cmdline :
    object ('self)
        inherit ['abandoned, 'truncated] FileJob.t
        inherit BackOtterJobExtension.t
        method printer : Format.formatter -> unit
    end
=
    object (self : 'self)
        inherit ['abandoned, 'truncated] FileJob.t file cmdline as job_super
        inherit BackOtterJobExtension.t as b_super

        initializer
            let job = b_super#initialize in
            self#become job

        (* TODO: can we not duplicate this in BackOtterFunctionJob? *)
        method append_decision_path decision = 
            if BackOtterJobExtension.enable_record_decisions self then
                let job = job_super#append_decision_path decision in
                job#postprocess_append_decision_path decision
            else self

        method printer ff =
            Format.fprintf ff "BackOtterFileJob@;";
            Format.fprintf ff "inherit @[<v>%t@]@;" job_super#printer;
            Format.fprintf ff "inherit @[<v>%t@]@;" b_super#printer

        method become (other : 'self) =
            job_super#become other;
            b_super#become other

    end

