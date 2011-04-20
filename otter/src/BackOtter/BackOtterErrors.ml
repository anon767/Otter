open OcamlUtilities
open OtterBytes
open OtterCore


type t = [
    | `FailingPath of t * Cil.fundec * DecisionPath.t
    | `SummaryReturn of Bytes.bytes option
    | `SummaryExit of Bytes.bytes option
    | `SummaryAbandoned of t
    | Errors.t
]


let option_printer printer ff = function
    | Some x -> Format.fprintf ff "Some (@[%a@]@,)" printer x
    | None -> Format.fprintf ff "None"


let rec printer ff (error : t) = match error with
    | `FailingPath (reason, fundec, failing_path) ->
        Format.fprintf ff "`FailingPath(@[%a@])" printer reason;
        Output.debug_printf "@\n=> Extract the following failing path for function %s:@." fundec.Cil.svar.Cil.vname;
        Output.debug_printf "@[%a@]@\n@." DecisionPath.print failing_path

    | `SummaryReturn return_opt ->
        Format.fprintf ff "`SummaryReturn(@[%a@])" (option_printer BytesPrinter.bytes) return_opt

    | `SummaryExit exit_opt ->
        Format.fprintf ff "`SummaryExit(@[%a@])" (option_printer BytesPrinter.bytes) exit_opt

    | `SummaryAbandoned reason ->
        Format.fprintf ff "`SummaryAbandoned(@[%a@])" printer reason

    | #Errors.t as x -> Errors.printer ff x


let matcher name args =
    match name, args with
        (* TODO: have some sort of test on the failing path list *)
        | "failing_path", [] ->
            begin function
                | `FailingPath _ -> true
                | _ -> false
            end
        | "failing_path", _ ->
            failwith "Invalid failing_path (takes no arguments)."

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
