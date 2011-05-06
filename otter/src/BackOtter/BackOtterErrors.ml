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
    | Some x -> Format.fprintf ff "Some (@[<h>%a@]@,)" printer x
    | None -> Format.fprintf ff "None"


let rec printer ff (error : t) = match error with
    | `SummaryReturn return_opt ->
        Format.fprintf ff "`SummaryReturn(@[<h>%a@])" (option_printer BytesPrinter.bytes) return_opt

    | `SummaryExit exit_opt ->
        Format.fprintf ff "`SummaryExit(@[<h>%a@])" (option_printer BytesPrinter.bytes) exit_opt

    | `SummaryAbandoned reason ->
        Format.fprintf ff "`SummaryAbandoned(@[<h>%a@])" printer reason

    | #Errors.t as x -> Errors.printer ff x


let matcher = Errors.matcher
