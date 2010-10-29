open OtterCore

class ['self] t = object (self : 'self)
    val completed = []

    method report = function
        | Job.Complete completion -> {< completed = completion::completed >}
        | _ -> self

    method should_continue = true

    method completed = completed
end

