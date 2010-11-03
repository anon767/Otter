open OtterBytes
open OtterCore


type t = [
    | `FailingPaths of Decision.t list list
    | `SummaryReturn of Bytes.bytes option
    | `SummaryExit of Bytes.bytes option
    | `SummaryAbandoned of t * Cil.location
    | Errors.t
]


let option_printer printer ff = function
    | Some x -> Format.fprintf ff "Some (@[%a@]@,)" printer x
    | None -> Format.fprintf ff "None"


let rec printer ff (error : t) = match error with
    | `FailingPaths _ -> Format.fprintf ff "(FailingPaths)" (* TODO (martin): print something meaningful *)
    | `SummaryReturn return_opt -> Format.fprintf ff "`SummaryReturn(@[%a@])" (option_printer BytesPrinter.bytes) return_opt
    | `SummaryExit exit_opt -> Format.fprintf ff "`SummaryExit(@[%a@])" (option_printer BytesPrinter.bytes) exit_opt
    | `SummaryAbandoned (reason, loc) -> Format.fprintf ff "`SummaryAbandoned(@[%s@@%d: %a@])" loc.Cil.file loc.Cil.line printer reason
    | #Errors.t as x -> Errors.printer ff x


let matcher name args =
    match name, args with
        (* TODO: have some sort of test on the failing path list *)
        | "failing_paths", [] ->
            begin function
                | `FailingPaths _ -> true
                | _ -> false
            end
        | "failing_paths", _ ->
            failwith "Invalid failing_paths (takes no arguments)."

        | _ ->
            Errors.matcher name args
