
type t = [
    | `Failure of string
    | `FailingPath of t * Cil.fundec * DecisionPath.t   (* TODO: move this back to BackOtterErrors *)
    | `TargetReached of Target.t
    | `AssertionFailure of Cil.exp
    | `OutOfBounds of Cil.exp
    | `DivisionByZero of Cil.exp
]

let rec printer ff (error : t) = match error with
    | `Failure msg -> Format.fprintf ff "`Failure:%s" msg
    | `FailingPath (reason, fundec, failing_path) ->
        Format.fprintf ff "`FailingPath(@[%a@])" printer reason;
        OcamlUtilities.Output.debug_printf "@\n=> Extract the following failing path for function %s:@." fundec.Cil.svar.Cil.vname;
        OcamlUtilities.Output.debug_printf "@[%a@]@\n@." DecisionPath.print failing_path
    | `TargetReached target -> Format.fprintf ff "`TargetReached @[%a@]" Target.printer target
    | `AssertionFailure exp -> Format.fprintf ff "`AssertionFailure: %a" Printcil.exp exp
    | `OutOfBounds exp -> Format.fprintf ff "`OutOfBounds: %a" Printcil.exp exp
    | `DivisionByZero exp -> Format.fprintf ff "`DivisionByZero: %a" Printcil.exp exp

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
        (* TODO: have some sort of test on the failing path list *)
        | "failing_path", [] ->
            begin function
                | `FailingPath _ -> true
                | _ -> false
            end
        | "failing_path", _ ->
            failwith "Invalid failing_path (takes no arguments)."

        | "target_reached", [] ->
            begin function
                | `TargetReached target -> true
                | _ -> false
            end
        | "target_reached", _ ->
            failwith "Invalid target_reached (takes no arguments)."
        | "out_of_bounds", [] ->
            begin function
                | `OutOfBounds _ -> true
                | _ -> false
            end
        | "out_of_bounds", _ ->
            failwith "Invalid out_of_bounds (takes no arguments)."
        | "division_by_zero", [] ->
            begin function
                | `DivisionByZero _ -> true
                | _ -> false
            end
        | "division_by_zero", _ ->
            failwith "Invalid division_by_zero (takes no arguments)."
        | s, _ ->
            failwith "Unknown failure reason."
