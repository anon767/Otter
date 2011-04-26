open OcamlUtilities
open OtterBytes
open OtterCore


type t = [
    | `SummaryReturn of Bytes.bytes option
    | `SummaryExit of Bytes.bytes option
    | `SummaryAbandoned of t
    | Errors.t
]


let option_printer printer ff = function
    | Some x -> Format.fprintf ff "Some (@[%a@]@,)" printer x
    | None -> Format.fprintf ff "None"


let rec printer ff (error : t) = match error with
    | `SummaryReturn return_opt ->
        Format.fprintf ff "`SummaryReturn(@[%a@])" (option_printer BytesPrinter.bytes) return_opt

    | `SummaryExit exit_opt ->
        Format.fprintf ff "`SummaryExit(@[%a@])" (option_printer BytesPrinter.bytes) exit_opt

    | `SummaryAbandoned reason ->
        Format.fprintf ff "`SummaryAbandoned(@[%a@])" printer reason

    | #Errors.t as x -> Errors.printer ff x


let matcher name args =
    match name, args with
        (* TODO: target_reached and failing_path should be consolidated into one error *)
        | "target_reached", [] ->
            begin function
                | `FailingPath (`TargetReached target, _, _) -> true
                | error -> Errors.matcher name args error
            end
        | "target_reached", _ ->
            failwith "Invalid target_reached (takes no arguments)."

        | _ ->
            Errors.matcher name args
