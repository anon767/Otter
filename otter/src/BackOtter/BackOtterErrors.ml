
type t = [
    | `FailureReached
    | `FailingPaths of OtterCore.Decision.t list list
    | `SummaryReturn of OtterBytes.Bytes.bytes option
    | `SummaryExit of OtterBytes.Bytes.bytes option
    | OtterCore.Errors.t
]

let option_printer printer ff = function
    | Some x -> Format.fprintf ff "Some (@[%a@]@,)" printer x
    | None -> Format.fprintf ff "None"


let printer ff (error : t) = match error with
    | `FailureReached -> Format.fprintf ff "`FailureReached"
    | `FailingPaths (_:(OtterCore.Decision.t list list)) -> Format.fprintf ff "(FailingPaths)" (* TODO (martin): print something meaningful *)
    | `SummaryReturn return_opt -> Format.fprintf ff "`SummaryReturn(@[%a@])" (option_printer OtterBytes.BytesPrinter.bytes) return_opt
    | `SummaryExit exit_opt -> Format.fprintf ff "`SummaryExit(@[%a@])" (option_printer OtterBytes.BytesPrinter.bytes) exit_opt
    | #OtterCore.Errors.t as x -> OtterCore.Errors.printer ff x


let matcher name args =
    match name, args with
        | "failure_reached", [] ->
            begin function
                | `FailureReached -> true
                | _ -> false
            end
        | "failure_reached", _ ->
            failwith "Invalid failure_reached (takes no arguments)."

        (* TODO: have some sort of test on the failing path list *)
        | "failing_paths", [] ->
            begin function
                | `FailingPaths _ -> true
                | _ -> false
            end
        | "failing_paths", _ ->
            failwith "Invalid failing_paths (takes no arguments)."

        | _ ->
            OtterCore.Errors.matcher name args
