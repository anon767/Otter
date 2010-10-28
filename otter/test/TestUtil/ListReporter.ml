open OtterCore

class ['reason] t = object (self)
    val completed = []

    method report = function
        | Job.Complete (completion : 'reason Job.job_completion) -> {< completed = completion::completed >}
        | _ -> self

    method should_continue = true

    method completed = completed
end

