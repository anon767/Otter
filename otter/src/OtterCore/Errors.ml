
type t = [
    | `Failure of string
    | `FailureReached
]

let printer ff (error : t) = match error with
    | `Failure msg -> Format.fprintf ff "`Failure:%s" msg
    | `FailureReached -> Format.fprintf ff "`FailureReached"

let matcher name args =
    match name, args with
        | "failure", [ Cil.AStr reason_regexp ] ->
            let reason = Str.regexp reason_regexp in
            begin function
                | `Failure reason' -> Str.string_match reason reason' 0
                | _ -> false
            end
        | "failure", _ ->
            failwith "Invalid failure (should have exactly one regex string argument to match the failure reason)."
        | "failure_reached", [] ->
            begin function
                | `FailureReached -> true
                | _ -> false
            end
        | "failure_reached", _ ->
            failwith "Invalid failure_reached (takes no arguments)."
        | s, _ ->
            failwith "Unknown failure reason."
