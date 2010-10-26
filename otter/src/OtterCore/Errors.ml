
type t = [ `Failure of string ]

let printer ff (error : t) = match error with
    | `Failure msg -> Format.fprintf ff "`Failure:%s" msg

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
        | s, _ ->
            failwith "Unknown failure reason."
