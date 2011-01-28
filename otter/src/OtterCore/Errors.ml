
type t = [
    | `Failure of string
    | `FailureReached
    | `AssertionFailure of Cil.exp
    | `OutOfBounds of Cil.lval
]

let printer ff (error : t) = match error with
    | `Failure msg -> Format.fprintf ff "`Failure:%s" msg
    | `FailureReached -> Format.fprintf ff "`FailureReached"
    | `AssertionFailure exp -> Format.fprintf ff "`AssertionFailure: %a" Printcil.exp exp
    | `OutOfBounds lval -> Format.fprintf ff "`OutOfBounds: %a" Printcil.lval lval

let matcher name args =
    match name, args with
        | "assertion_failure", [] ->
            begin function
                | `AssertionFailure _ -> true
                | _ -> false
            end
        | "assertion_failure", _ ->
            failwith "Invalid assertion_failure (takes no arguments)."
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
        | "out_of_bounds", [] ->
            begin function
                | `OutOfBounds _ -> true
                | _ -> false
            end
        | "out_of_bounds", _ ->
            failwith "Invalid: out_of_bounds takes no arguments."
        | s, _ ->
            failwith "Unknown failure reason."
